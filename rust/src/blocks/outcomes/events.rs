//! This module projects contract, staking, and token events into legacy JSON.
//! Account attribution and optional token memos are selected explicitly.

use super::{known, leaf, tagged};
use crate::{compatibility::fraction, network::NetworkFailure};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    protocol_level_tokens::{TokenEvent, TokenEventDetails},
    types::{
        Address, BakerAddedEvent, BakerEvent, BakerKeysEvent, ContractTraceElement, DelegationEvent,
    },
};
use serde_json::{Value, json};

pub(super) fn baker_added(data: &BakerAddedEvent) -> Result<Value, NetworkFailure> {
    leaf(
        "BakerAdded",
        data,
        &[
            "bakerId",
            "account",
            "signKey",
            "electionKey",
            "aggregationKey",
            "stake",
            "restakeEarnings",
        ],
    )
}
pub(super) fn baker_keys(data: &BakerKeysEvent) -> Result<Value, NetworkFailure> {
    leaf(
        "BakerKeysUpdated",
        data,
        &[
            "bakerId",
            "account",
            "signKey",
            "electionKey",
            "aggregationKey",
        ],
    )
}
pub(super) fn trace(event: &ContractTraceElement) -> Result<Value, NetworkFailure> {
    if matches!(event, ContractTraceElement::Updated { data } if data.contract_version.0 > 1) {
        return Err(NetworkFailure::InvalidResponse);
    }
    Ok(match event {
        ContractTraceElement::Updated { data } => leaf(
            "Updated",
            data,
            &[
                "address",
                "instigator",
                "amount",
                "message",
                "receiveName",
                "events",
                "contractVersion",
            ],
        )?,
        ContractTraceElement::Transferred { from, amount, to } => tagged(
            "Transferred",
            json!({"from": Address::Contract(*from), "amount": amount.micro_ccd.to_string(), "to": Address::Account(*to)}),
        ),
        ContractTraceElement::Interrupted { address, events } => {
            tagged("Interrupted", json!({"address": address, "events": events}))
        }
        ContractTraceElement::Resumed { address, success } => {
            tagged("Resumed", json!({"address": address, "success": success}))
        }
        ContractTraceElement::Upgraded { address, from, to } => tagged(
            "Upgraded",
            json!({"address": address, "from": from, "to": to}),
        ),
    })
}
pub(super) fn baker(event: &BakerEvent, account: AccountAddress) -> Result<Value, NetworkFailure> {
    Ok(match event {
        BakerEvent::BakerAdded { data } => baker_added(data)?,
        BakerEvent::BakerKeysUpdated { data } => baker_keys(data)?,
        BakerEvent::BakerRemoved { baker_id } => tagged(
            "BakerRemoved",
            json!({"bakerId": baker_id, "account": account}),
        ),
        BakerEvent::BakerStakeIncreased {
            baker_id,
            new_stake,
        } => tagged(
            "BakerStakeIncreased",
            json!({"bakerId": baker_id, "account": account, "newStake": new_stake.micro_ccd.to_string()}),
        ),
        BakerEvent::BakerStakeDecreased {
            baker_id,
            new_stake,
        } => tagged(
            "BakerStakeDecreased",
            json!({"bakerId": baker_id, "account": account, "newStake": new_stake.micro_ccd.to_string()}),
        ),
        BakerEvent::BakerRestakeEarningsUpdated {
            baker_id,
            restake_earnings,
        } => tagged(
            "BakerSetRestakeEarnings",
            json!({"bakerId": baker_id, "account": account, "restakeEarnings": restake_earnings}),
        ),
        BakerEvent::BakerSetOpenStatus {
            baker_id,
            open_status,
        } => tagged(
            "BakerSetOpenStatus",
            json!({"bakerId": baker_id, "account": account, "openStatus": known(open_status)?}),
        ),
        BakerEvent::BakerSetMetadataURL {
            baker_id,
            metadata_url,
        } => tagged(
            "BakerSetMetadataURL",
            json!({"bakerId": baker_id, "account": account, "metadataURL": metadata_url}),
        ),
        BakerEvent::BakerSetTransactionFeeCommission {
            baker_id,
            transaction_fee_commission,
        } => tagged(
            "BakerSetTransactionFeeCommission",
            json!({"bakerId": baker_id, "account": account, "transactionFeeCommission": fraction(*transaction_fee_commission)}),
        ),
        BakerEvent::BakerSetBakingRewardCommission {
            baker_id,
            baking_reward_commission,
        } => tagged(
            "BakerSetBakingRewardCommission",
            json!({"bakerId": baker_id, "account": account, "bakingRewardCommission": fraction(*baking_reward_commission)}),
        ),
        BakerEvent::BakerSetFinalizationRewardCommission {
            baker_id,
            finalization_reward_commission,
        } => tagged(
            "BakerSetFinalizationRewardCommission",
            json!({"bakerId": baker_id, "account": account, "finalizationRewardCommission": fraction(*finalization_reward_commission)}),
        ),
        BakerEvent::DelegationRemoved { delegator_id } => tagged(
            "DelegationRemoved",
            json!({"delegatorId": delegator_id, "account": account}),
        ),
        BakerEvent::BakerSuspended { baker_id } => tagged(
            "BakerSuspended",
            json!({"bakerId": baker_id, "account": account}),
        ),
        BakerEvent::BakerResumed { baker_id } => tagged(
            "BakerResumed",
            json!({"bakerId": baker_id, "account": account}),
        ),
    })
}
pub(super) fn delegation(
    event: &DelegationEvent,
    account: AccountAddress,
) -> Result<Value, NetworkFailure> {
    Ok(match event {
        DelegationEvent::DelegationStakeIncreased {
            delegator_id,
            new_stake,
        } => tagged(
            "DelegationStakeIncreased",
            json!({"delegatorId": delegator_id, "account": account, "newStake": new_stake.micro_ccd.to_string()}),
        ),
        DelegationEvent::DelegationStakeDecreased {
            delegator_id,
            new_stake,
        } => tagged(
            "DelegationStakeDecreased",
            json!({"delegatorId": delegator_id, "account": account, "newStake": new_stake.micro_ccd.to_string()}),
        ),
        DelegationEvent::DelegationSetRestakeEarnings {
            delegator_id,
            restake_earnings,
        } => tagged(
            "DelegationSetRestakeEarnings",
            json!({"delegatorId": delegator_id, "account": account, "restakeEarnings": restake_earnings}),
        ),
        DelegationEvent::DelegationSetDelegationTarget {
            delegator_id,
            delegation_target,
        } => tagged(
            "DelegationSetDelegationTarget",
            json!({"delegatorId": delegator_id, "account": account, "delegationTarget": delegation_target}),
        ),
        DelegationEvent::DelegationAdded { delegator_id } => tagged(
            "DelegationAdded",
            json!({"delegatorId": delegator_id, "account": account}),
        ),
        DelegationEvent::DelegationRemoved { delegator_id } => tagged(
            "DelegationRemoved",
            json!({"delegatorId": delegator_id, "account": account}),
        ),
        DelegationEvent::BakerRemoved { baker_id } => tagged(
            "BakerRemoved",
            json!({"bakerId": baker_id, "account": account}),
        ),
    })
}
pub(super) fn token(event: &TokenEvent) -> Result<Value, NetworkFailure> {
    Ok(match &event.event {
        TokenEventDetails::Module(module) => tagged(
            "TokenModuleEvent",
            json!({"tokenId": event.token_id, "type": module.event_type, "details": module.details}),
        ),
        TokenEventDetails::Transfer(transfer) => {
            let mut value = tagged(
                "TokenTransfer",
                json!({"tokenId": event.token_id, "from": transfer.from, "to": transfer.to, "amount": transfer.amount}),
            );
            if let Some(memo) = &transfer.memo {
                value["memo"] = json!(memo);
            }
            value
        }
        TokenEventDetails::Mint(mint) => tagged(
            "TokenMint",
            json!({"tokenId": event.token_id, "target": mint.target, "amount": mint.amount}),
        ),
        TokenEventDetails::Burn(burn) => tagged(
            "TokenBurn",
            json!({"tokenId": event.token_id, "target": burn.target, "amount": burn.amount}),
        ),
    })
}
