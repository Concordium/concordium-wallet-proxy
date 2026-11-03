//! This module selects the simple-status fields from legacy transaction events.
//! It keeps the original common fields separate for committed-outcome equality.

use super::{ProgressError, rejection};
use concordium_rust_sdk::types::{
    AccountTransactionEffects as Effect, BlockItemSummary, BlockItemSummaryDetails,
};
use serde_json::{Map, Value, json};

/// This type retains both parts of the Haskell outcome pair list.
#[derive(PartialEq)]
pub(super) struct Outcome {
    /// These fields retain the original sender even when event data overrides it.
    common: Map<String, Value>,
    /// These fields contain the transaction-specific simple outcome.
    details: Map<String, Value>,
}
impl Outcome {
    /// Merge fields only after complete outcome comparisons have finished.
    pub fn fields(self) -> Map<String, Value> {
        let mut fields = self.common;
        fields.extend(self.details);
        fields
    }
}

/// Project the supported transaction types and all supported rejection reasons.
pub(super) fn project(summary: &BlockItemSummary) -> Result<Outcome, ProgressError> {
    let mut common = json!({"transactionHash":summary.hash,"sender":null,"cost":"0"})
        .as_object()
        .unwrap()
        .clone();
    let details = match summary
        .details
        .as_known()
        .ok_or(ProgressError::Unexpected)?
    {
        BlockItemSummaryDetails::AccountCreation(_) => json!({"outcome":"success"}),
        BlockItemSummaryDetails::AccountTransaction(account) => {
            common.insert("sender".into(), json!(account.sender));
            common.insert("cost".into(), json!(account.cost.micro_ccd.to_string()));
            if let Some(sponsor) = &account.sponsor {
                common.insert(
                    "sponsor".into(),
                    json!({"sponsor":sponsor.sponsor,"cost":sponsor.cost.micro_ccd.to_string()}),
                );
            }
            let effect = account
                .effects
                .as_known()
                .ok_or(ProgressError::Unexpected)?;
            if let Effect::None { reject_reason, .. } = effect {
                json!({"outcome":"reject","rejectReason":rejection::description(reject_reason.as_known().ok_or(ProgressError::Unexpected)?)?})
            } else {
                success(summary, effect)?
            }
        }
        BlockItemSummaryDetails::Update(_) | BlockItemSummaryDetails::TokenCreationDetails(_) => {
            return Err(ProgressError::Unsupported);
        }
    };
    Ok(Outcome {
        common,
        details: details
            .as_object()
            .ok_or(ProgressError::Unexpected)?
            .clone(),
    })
}

/// Select fields for the successful transaction modes supported by Haskell.
fn success(summary: &BlockItemSummary, effect: &Effect) -> Result<Value, ProgressError> {
    if !matches!(
        effect,
        Effect::AccountTransfer { .. }
            | Effect::AccountTransferWithMemo { .. }
            | Effect::EncryptedAmountTransferred { .. }
            | Effect::EncryptedAmountTransferredWithMemo { .. }
            | Effect::TransferredToEncrypted { .. }
            | Effect::TransferredToPublic { .. }
            | Effect::BakerConfigured { .. }
            | Effect::DelegationConfigured { .. }
            | Effect::ModuleDeployed { .. }
            | Effect::ContractInitialized { .. }
            | Effect::ContractUpdateIssued { .. }
            | Effect::TokenUpdate { .. }
            | Effect::DataRegistered { .. }
    ) {
        return Err(ProgressError::Unsupported);
    }
    let legacy = crate::blocks::outcomes::value(summary)?;
    let events = legacy["result"]["events"]
        .as_array()
        .ok_or(ProgressError::Unexpected)?;
    let first = events.first();
    let mut result = json!({"outcome":"success"});
    match effect {
        Effect::AccountTransfer { .. } | Effect::AccountTransferWithMemo { .. } => {
            let transfer = event(first, "Transferred")?;
            result["to"] = address(&transfer["to"], "AddressAccount")?;
            copy(&mut result, transfer, &[("amount", "amount")])?;
            memo(&mut result, &events[1..]);
        }
        Effect::EncryptedAmountTransferred { .. }
        | Effect::EncryptedAmountTransferredWithMemo { .. } => {
            let removed = event(first, "EncryptedAmountsRemoved")?;
            let added = event(events.get(1), "NewEncryptedAmount")?;
            copy(
                &mut result,
                removed,
                &[
                    ("sender", "account"),
                    ("newSelfEncryptedAmount", "newAmount"),
                    ("inputEncryptedAmount", "inputAmount"),
                    ("aggregatedIndex", "upToIndex"),
                ],
            )?;
            copy(
                &mut result,
                added,
                &[("to", "account"), ("encryptedAmount", "encryptedAmount")],
            )?;
            memo(&mut result, &events[2..]);
        }
        Effect::TransferredToPublic { .. } => {
            let removed = event(first, "EncryptedAmountsRemoved")?;
            let added = event(events.get(1), "AmountAddedByDecryption")?;
            copy(
                &mut result,
                removed,
                &[
                    ("sender", "account"),
                    ("newSelfEncryptedAmount", "newAmount"),
                    ("inputEncryptedAmount", "inputAmount"),
                    ("aggregatedIndex", "upToIndex"),
                ],
            )?;
            copy(&mut result, added, &[("amountAdded", "amount")])?;
        }
        Effect::TransferredToEncrypted { .. } => {
            copy(
                &mut result,
                event(first, "EncryptedSelfAmountAdded")?,
                &[
                    ("sender", "account"),
                    ("newSelfEncryptedAmount", "newAmount"),
                    ("amountSubtracted", "amount"),
                ],
            )?;
        }
        Effect::BakerConfigured { .. } => {
            // Haskell checks only the first event, not the first event with a validator ID.
            if let Some(baker) = first.and_then(|first| first.get("bakerId")) {
                result["bakerId"] = baker.clone();
            }
        }
        Effect::DelegationConfigured { .. } => {}
        Effect::ModuleDeployed { .. } => copy(
            &mut result,
            event(first, "ModuleDeployed")?,
            &[("moduleRef", "contents")],
        )?,
        Effect::ContractInitialized { .. } => copy(
            &mut result,
            event(first, "ContractInitialized")?,
            &[
                ("moduleRef", "ref"),
                ("address", "address"),
                ("amount", "amount"),
                ("initName", "initName"),
                ("contractVersion", "contractVersion"),
                ("events", "events"),
            ],
        )?,
        Effect::ContractUpdateIssued { .. } => {
            result["trace"] = Value::Array(events.iter().map(trace).collect::<Result<_, _>>()?);
        }
        Effect::TokenUpdate { .. } => {
            if events.len() == 1 && events[0]["tag"] == "TokenTransfer" {
                let transfer = &events[0];
                if transfer["to"]["type"] != "account" {
                    return Err(ProgressError::Unexpected);
                }
                result["to"] = transfer["to"]["address"].clone();
                copy(
                    &mut result,
                    transfer,
                    &[("tokenId", "tokenId"), ("tokenAmount", "amount")],
                )?;
                if let Some(memo) = transfer.get("memo") {
                    result["memo"] = memo.clone();
                }
            } else {
                result = json!({});
            }
        }
        Effect::DataRegistered { .. } => copy(
            &mut result,
            event(first, "DataRegistered")?,
            &[("registeredData", "data")],
        )?,
        _ => return Err(ProgressError::Unsupported),
    }
    Ok(result)
}

/// Check the expected event tag before selecting its fields.
fn event<'a>(value: Option<&'a Value>, tag: &str) -> Result<&'a Value, ProgressError> {
    value
        .filter(|value| value["tag"] == tag)
        .ok_or(ProgressError::Unexpected)
}

/// Copy required event fields to their wallet-facing names.
fn copy(target: &mut Value, source: &Value, fields: &[(&str, &str)]) -> Result<(), ProgressError> {
    for (output, input) in fields {
        target[*output] = source.get(*input).ok_or(ProgressError::Unexpected)?.clone();
    }
    Ok(())
}

/// Read a legacy tagged address without leaking the tag into the simple response.
fn address(value: &Value, kind: &str) -> Result<Value, ProgressError> {
    if value["type"] != kind {
        return Err(ProgressError::Unexpected);
    }
    value
        .get("address")
        .cloned()
        .ok_or(ProgressError::Unexpected)
}

/// Attach a memo only when it is the single remaining event.
fn memo(target: &mut Value, events: &[Value]) {
    if let [memo] = events
        && memo["tag"] == "TransferMemo"
    {
        target["memo"] = memo["memo"].clone();
    }
}

/// Keep the four legacy contract trace kinds; an upgrade makes this endpoint fail.
fn trace(event: &Value) -> Result<Value, ProgressError> {
    let mut value = event.clone();
    let kind = match event["tag"].as_str() {
        Some("Updated") => "updated",
        Some("Interrupted") => "interrupted",
        Some("Resumed") => "resumed",
        Some("Transferred") => {
            return Ok(
                json!({"type":"transferred","from":address(&event["from"], "AddressContract")?,"to":address(&event["to"], "AddressAccount")?,"amount":event["amount"]}),
            );
        }
        _ => return Err(ProgressError::Unexpected),
    };
    value
        .as_object_mut()
        .ok_or(ProgressError::Unexpected)?
        .remove("tag");
    value["type"] = kind.into();
    Ok(value)
}
