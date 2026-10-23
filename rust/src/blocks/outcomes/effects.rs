//! This module expands account transaction effects into ordered legacy events.
//! It keeps account roles, memo events, and encrypted-transfer event order explicit.

use super::{events as event, known, leaf, tagged};
use crate::network::NetworkFailure;
use concordium_rust_sdk::{
    id::types::AccountAddress,
    protocol_level_tokens::OperationEvent,
    types::{AccountTransactionEffects as Effect, Address},
};
use serde_json::{Value, json};

fn schedule(
    releases: &[(
        concordium_rust_sdk::common::types::Timestamp,
        concordium_rust_sdk::common::types::Amount,
    )],
) -> Vec<Value> {
    releases
        .iter()
        .map(|(time, amount)| json!([time.timestamp_millis(), amount.micro_ccd.to_string()]))
        .collect()
}

pub(super) fn events(
    effect: &Effect,
    sender: AccountAddress,
) -> Result<Vec<Value>, NetworkFailure> {
    let output = match effect {
        Effect::None { .. } => return Err(NetworkFailure::InvalidResponse),
        Effect::ModuleDeployed { module_ref } => vec![tagged("ModuleDeployed", json!({"contents": module_ref}))],
        Effect::ContractInitialized { data } => {
            if data.contract_version.0 > 1 { return Err(NetworkFailure::InvalidResponse); }
            let mut value = leaf("ContractInitialized", data, &["ref", "address", "amount", "initName", "events", "contractVersion", "parameter"])?;
            if data.parameter.is_none() { value["parameter"] = json!(""); }
            vec![value]
        }
        Effect::ContractUpdateIssued { effects } => effects.iter().map(|value| event::trace(known(value)?)).collect::<Result<_, _>>()?,
        Effect::AccountTransfer { amount, to } => vec![tagged("Transferred", json!({"from": Address::Account(sender), "to": Address::Account(*to), "amount": amount.micro_ccd.to_string()}))],
        Effect::AccountTransferWithMemo { amount, to, memo } => vec![tagged("Transferred", json!({"from": Address::Account(sender), "to": Address::Account(*to), "amount": amount.micro_ccd.to_string()})), tagged("TransferMemo", json!({"memo": memo}))],
        Effect::BakerAdded { data } => vec![event::baker_added(data)?],
        Effect::BakerRemoved { baker_id } => vec![tagged("BakerRemoved", json!({"bakerId": baker_id, "account": sender}))],
        Effect::BakerStakeUpdated { data } => data.iter().map(|data| tagged(if data.increased {"BakerStakeIncreased"} else {"BakerStakeDecreased"}, json!({"bakerId": data.baker_id, "account": sender, "newStake": data.new_stake.micro_ccd.to_string()}))).collect(),
        Effect::BakerRestakeEarningsUpdated { baker_id, restake_earnings } => vec![tagged("BakerSetRestakeEarnings", json!({"bakerId": baker_id, "account": sender, "restakeEarnings": restake_earnings}))],
        Effect::BakerKeysUpdated { data } => vec![event::baker_keys(data)?],
        Effect::EncryptedAmountTransferred { removed, added } => vec![leaf("EncryptedAmountsRemoved", removed, &["account", "newAmount", "inputAmount", "upToIndex"])?, leaf("NewEncryptedAmount", added, &["account", "newIndex", "encryptedAmount"])?],
        Effect::EncryptedAmountTransferredWithMemo { removed, added, memo } => vec![leaf("EncryptedAmountsRemoved", removed, &["account", "newAmount", "inputAmount", "upToIndex"])?, leaf("NewEncryptedAmount", added, &["account", "newIndex", "encryptedAmount"])?, tagged("TransferMemo", json!({"memo": memo}))],
        Effect::TransferredToEncrypted { data } => vec![leaf("EncryptedSelfAmountAdded", data, &["account", "newAmount", "amount"])?],
        Effect::TransferredToPublic { removed, amount } => vec![leaf("EncryptedAmountsRemoved", removed, &["account", "newAmount", "inputAmount", "upToIndex"])?, tagged("AmountAddedByDecryption", json!({"account": sender, "amount": amount.micro_ccd.to_string()}))],
        Effect::TransferredWithSchedule { to, amount } => vec![tagged("TransferredWithSchedule", json!({"from": sender, "to": to, "amount": schedule(amount)}))],
        Effect::TransferredWithScheduleAndMemo { to, amount, memo } => vec![tagged("TransferredWithSchedule", json!({"from": sender, "to": to, "amount": schedule(amount)})), tagged("TransferMemo", json!({"memo": memo}))],
        Effect::CredentialKeysUpdated { cred_id } => vec![tagged("CredentialKeysUpdated", json!({"credId": cred_id}))],
        Effect::CredentialsUpdated { new_cred_ids, removed_cred_ids, new_threshold } => vec![tagged("CredentialsUpdated", json!({"account": sender, "newCredIds": new_cred_ids, "removedCredIds": removed_cred_ids, "newThreshold": new_threshold}))],
        Effect::DataRegistered { data } => vec![tagged("DataRegistered", json!({"data": data}))],
        Effect::BakerConfigured { data } => data.iter().map(|value| event::baker(known(value)?, sender)).collect::<Result<_, _>>()?,
        Effect::DelegationConfigured { data } => data.iter().map(|value| event::delegation(known(value)?, sender)).collect::<Result<_, _>>()?,
        Effect::TokenUpdate { events } => {
            let mut output = vec![];
            for value in events {
                // The legacy client's field-1 list contains only token events.
                // The node duplicates those in the unified field-2 list exposed by the SDK.
                match known(value)? { OperationEvent::Token(token) => output.push(event::token(token)?), OperationEvent::Lock(_) => {} }
            }
            output
        }
    };
    Ok(output)
}
