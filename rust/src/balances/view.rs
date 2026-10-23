//! This module converts SDK account state to wallet-facing CCD balance JSON.
//! It preserves legacy encrypted aggregation, staking fields, and payday estimates.

use super::Version;
use crate::{
    compatibility::{first_payday, format_time, fraction},
    network::NetworkFailure,
};
use chrono::{DateTime, TimeDelta, Utc};
use concordium_rust_sdk::{
    types::{AccountInfo, AccountStakingInfo, BakerPoolStatus, CooldownStatus, StakePendingChange},
    v2::Upward,
};
use serde_json::{Value, json};

/// This type contains the finalized-block inputs for pending-change estimates.
pub(super) struct Payday {
    /// This value is the next payday, if supported by the protocol.
    pub next: Option<DateTime<Utc>>,
    /// This duration is the epoch length from consensus.
    pub epoch_duration: TimeDelta,
    /// This value is the number of epochs per reward period, if supported.
    pub epochs: Option<u64>,
}

pub(super) fn validate(account: &AccountInfo) -> Result<(), NetworkFailure> {
    if account
        .cooldowns
        .iter()
        .any(|cooldown| matches!(cooldown.status, Upward::Unknown(_)))
    {
        return Err(NetworkFailure::InvalidResponse);
    }
    match &account.account_stake {
        Some(Upward::Unknown(_)) => Err(NetworkFailure::InvalidResponse),
        Some(Upward::Known(AccountStakingInfo::Baker {
            pool_info: Some(info),
            ..
        })) if matches!(info.open_status, Upward::Unknown(_)) => {
            Err(NetworkFailure::InvalidResponse)
        }
        _ => Ok(()),
    }
}

pub(super) fn has_pending(account: &AccountInfo) -> bool {
    matches!(
        account.account_stake.as_ref(),
        Some(Upward::Known(
            AccountStakingInfo::Baker {
                pending_change: Some(_),
                ..
            } | AccountStakingInfo::Delegated {
                pending_change: Some(_),
                ..
            }
        ))
    )
}

pub(super) fn balance(
    account: &AccountInfo,
    pool: Option<&BakerPoolStatus>,
    version: Version,
    payday: &Payday,
) -> Result<Value, NetworkFailure> {
    let encrypted = &account.account_encrypted_amount;
    let mut incoming = Vec::new();
    if let Some((amount, _)) = &encrypted.aggregated_amount {
        incoming.push(json!(amount));
    }
    incoming.extend(
        encrypted
            .incoming_amounts
            .iter()
            .map(|amount| json!(amount)),
    );
    let mut encrypted_value = json!({"selfAmount": encrypted.self_amount, "startIndex": encrypted.start_index, "incomingAmounts": incoming});
    if let Some((_, count)) = encrypted.aggregated_amount {
        encrypted_value["numAggregated"] = json!(count);
    }
    let schedule: Vec<Value> = account.account_release_schedule.schedule.iter().map(|release| json!({
        "timestamp": release.timestamp.timestamp_millis(), "amount": release.amount.micro_ccd.to_string(), "transactions": release.transactions,
    })).collect();
    let mut value = json!({
        "accountAmount": account.account_amount.micro_ccd.to_string(),
        "accountNonce": account.account_nonce,
        "accountIndex": account.account_index,
        "accountReleaseSchedule": {"total": account.account_release_schedule.total.micro_ccd.to_string(), "schedule": schedule},
        "accountEncryptedAmount": encrypted_value,
    });
    if version != Version::Legacy {
        let cooldowns = account.cooldowns.iter().map(|cooldown| {
            let status = match &cooldown.status {
                Upward::Known(CooldownStatus::Cooldown) => "cooldown",
                Upward::Known(CooldownStatus::PreCooldown) => "precooldown",
                Upward::Known(CooldownStatus::PrePreCooldown) => "preprecooldown",
                Upward::Unknown(_) => return Err(NetworkFailure::InvalidResponse),
            };
            Ok(json!({"timestamp": cooldown.end_time.millis, "amount": cooldown.amount.micro_ccd.to_string(), "status": status}))
        }).collect::<Result<Vec<_>, NetworkFailure>>()?;
        value["accountCooldowns"] = json!(cooldowns);
        value["accountAtDisposal"] = json!(account.available_balance.micro_ccd.to_string());
    }
    let Some(staking) = &account.account_stake else {
        return Ok(value);
    };
    let Upward::Known(staking) = staking else {
        return Err(NetworkFailure::InvalidResponse);
    };
    let (key, mut stake, pending) = match staking {
        AccountStakingInfo::Baker {
            staked_amount,
            restake_earnings,
            baker_info,
            pending_change,
            pool_info,
            is_suspended,
        } => {
            let mut stake = json!({
                "stakedAmount": staked_amount.micro_ccd.to_string(), "restakeEarnings": restake_earnings,
                "bakerId": baker_info.baker_id,
                "bakerElectionVerifyKey": baker_info.baker_election_verify_key,
                "bakerSignatureVerifyKey": baker_info.baker_signature_verify_key,
                "bakerAggregationVerifyKey": baker_info.baker_aggregation_verify_key,
                "isSuspended": is_suspended,
            });
            if let Some(info) = pool_info {
                if matches!(info.open_status, Upward::Unknown(_)) {
                    return Err(NetworkFailure::InvalidResponse);
                }
                stake["bakerPoolInfo"] = json!({"openStatus": info.open_status, "metadataUrl": info.metadata_url, "commissionRates": {
                    "finalizationCommission": fraction(info.commission_rates.finalization),
                    "bakingCommission": fraction(info.commission_rates.baking),
                    "transactionCommission": fraction(info.commission_rates.transaction)
                }});
            }
            ("accountBaker", stake, pending_change)
        }
        AccountStakingInfo::Delegated {
            staked_amount,
            restake_earnings,
            delegation_target,
            pending_change,
        } => {
            let mut stake = json!({"stakedAmount": staked_amount.micro_ccd.to_string(), "restakeEarnings": restake_earnings, "delegationTarget": delegation_target});
            if let Some(suspended) = pool
                .and_then(|pool| pool.active_baker_pool_status.as_ref())
                .and_then(|active| active.is_suspended)
            {
                stake["isSuspended"] = json!(suspended);
            }
            ("accountDelegation", stake, pending_change)
        }
    };
    if let Some(primed) = pool
        .and_then(|pool| pool.current_payday_status.as_ref())
        .and_then(|payday| payday.is_primed_for_suspension)
    {
        stake["isPrimedForSuspension"] = json!(primed);
    }
    if let Some(change) = pending {
        stake["pendingChange"] = pending_value(change, payday)?;
    }
    value[key] = stake;
    Ok(value)
}

fn pending_value(change: &StakePendingChange, payday: &Payday) -> Result<Value, NetworkFailure> {
    let effective = change.effective_time();
    let mut value = match change {
        StakePendingChange::ReduceStake { new_stake, .. } => {
            json!({"change": "ReduceStake", "newStake": new_stake.micro_ccd.to_string(), "effectiveTime": format_time(effective)})
        }
        StakePendingChange::RemoveStake { .. } => {
            json!({"change": "RemoveStake", "effectiveTime": format_time(effective)})
        }
    };
    if let (Some(next), Some(epochs)) = (payday.next, payday.epochs) {
        let estimated = first_payday(next, effective, payday.epoch_duration, epochs)?;
        value["estimatedChangeTime"] = json!(format_time(estimated));
    }
    Ok(value)
}
