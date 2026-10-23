//! This module selects the legacy pool and passive-delegation response fields.
//! It keeps optional suspension fields distinct from explicit null payday status.

use crate::{
    compatibility::{format_time, fraction},
    network::NetworkFailure,
};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::{
    types::{BakerPoolStatus, CommissionRates, PassiveDelegationStatus, PoolPendingChange},
    v2::Upward,
};
use serde_json::{Value, json};

pub(super) fn commissions(rates: &CommissionRates) -> Value {
    json!({"finalizationCommission": fraction(rates.finalization), "bakingCommission": fraction(rates.baking), "transactionCommission": fraction(rates.transaction)})
}

pub(super) fn effective_time(change: &PoolPendingChange) -> Option<DateTime<Utc>> {
    match change {
        PoolPendingChange::NoChange => None,
        PoolPendingChange::ReduceBakerCapital { effective_time, .. }
        | PoolPendingChange::RemovePool { effective_time } => Some(*effective_time),
    }
}

pub(super) fn pool(pool: &BakerPoolStatus) -> Result<Value, NetworkFailure> {
    let active = pool
        .active_baker_pool_status
        .as_ref()
        .ok_or(NetworkFailure::InvalidResponse)?;
    if matches!(active.pool_info.open_status, Upward::Unknown(_)) {
        return Err(NetworkFailure::InvalidResponse);
    }
    let pending = match &active.baker_stake_pending_change {
        PoolPendingChange::NoChange => json!({"pendingChangeType": "NoChange"}),
        PoolPendingChange::ReduceBakerCapital {
            baker_equity_capital,
            effective_time,
        } => {
            json!({"pendingChangeType": "ReduceBakerCapital", "bakerEquityCapital": baker_equity_capital.micro_ccd.to_string(), "effectiveTime": format_time(*effective_time)})
        }
        PoolPendingChange::RemovePool { effective_time } => {
            json!({"pendingChangeType": "RemovePool", "effectiveTime": format_time(*effective_time)})
        }
    };
    let mut value = json!({
        "poolType": "BakerPool", "bakerId": pool.baker_id, "bakerAddress": pool.baker_address,
        "allPoolTotalCapital": pool.all_pool_total_capital.micro_ccd.to_string(),
        "currentPaydayStatus": null,
        "bakerEquityCapital": active.baker_equity_capital.micro_ccd.to_string(),
        "delegatedCapital": active.delegated_capital.micro_ccd.to_string(),
        "delegatedCapitalCap": active.delegated_capital_cap.micro_ccd.to_string(),
        "poolInfo": {"openStatus": active.pool_info.open_status, "metadataUrl": active.pool_info.metadata_url, "commissionRates": commissions(&active.pool_info.commission_rates)},
        "bakerStakePendingChange": pending
    });
    if let Some(suspended) = active.is_suspended {
        value["isSuspended"] = json!(suspended);
    }
    if let Some(payday) = &pool.current_payday_status {
        let mut current = json!({
            "blocksBaked": payday.blocks_baked, "finalizationLive": payday.finalization_live,
            "transactionFeesEarned": payday.transaction_fees_earned.micro_ccd.to_string(),
            "effectiveStake": payday.effective_stake.micro_ccd.to_string(), "lotteryPower": payday.lottery_power,
            "bakerEquityCapital": payday.baker_equity_capital.micro_ccd.to_string(),
            "delegatedCapital": payday.delegated_capital.micro_ccd.to_string(),
            "commissionRates": commissions(&payday.commission_rates)
        });
        if let Some(primed) = payday.is_primed_for_suspension {
            current["isPrimedForSuspension"] = json!(primed);
        }
        if let Some(rounds) = payday.missed_rounds {
            current["missedRounds"] = json!(rounds);
        }
        value["currentPaydayStatus"] = current;
    }
    Ok(value)
}

pub(super) fn passive(status: &PassiveDelegationStatus) -> Value {
    json!({
        "delegatedCapital": status.delegated_capital.micro_ccd.to_string(),
        "commissionRates": commissions(&status.commission_rates),
        "currentPaydayTransactionFeesEarned": status.current_payday_transaction_fees_earned.micro_ccd.to_string(),
        "currentPaydayDelegatedCapital": status.current_payday_delegated_capital.micro_ccd.to_string(),
        "allPoolTotalCapital": status.all_pool_total_capital.micro_ccd.to_string()
    })
}
