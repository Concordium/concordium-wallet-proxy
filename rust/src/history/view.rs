//! This module projects persisted summaries into versioned account-history responses.
//! It computes account-relative public and encrypted effects before attributing sender or sponsor fees.

use super::{
    HistoryEntry, HistoryVersion,
    events::{Event, account},
    kinds::Kind,
    rejection::Rejection,
    rewards,
};
use concordium_rust_sdk::{
    common::types::Amount, id::types::AccountAddress, types::hashes::TransactionHash,
};
use num::{BigInt, BigRational, ToPrimitive};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

/// This record attributes the independently paid sponsor cost.
#[derive(Deserialize, Serialize)]
struct SponsorDetails {
    /// This account paid the sponsored fee.
    sponsor: AccountAddress,
    /// This amount was paid by the sponsor.
    cost: Amount,
}

/// This record decodes the supplemented legacy transaction summary, not SDK outcomes.
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct Summary {
    /// This account submitted the transaction, if an account was involved.
    sender: Option<AccountAddress>,
    /// This hash belongs to the persisted transaction.
    hash: TransactionHash,
    /// This amount was paid by the sender.
    cost: Amount,
    /// This number records the transaction energy use.
    energy_cost: u64,
    /// This value selects response type and detail behavior.
    #[serde(rename = "type")]
    kind: Kind,
    /// This value retains success events or a typed rejection reason.
    result: ResultDetails,
    /// This required index is validated but is not returned by history.
    #[serde(rename = "index")]
    _index: u64,
    /// This record attributes any separately sponsored fee.
    sponsor_details: Option<SponsorDetails>,
}

/// This value decodes a legacy transaction result.
#[derive(Deserialize)]
#[serde(tag = "outcome")]
enum ResultDetails {
    /// This result contains all events from a successful transaction.
    #[serde(rename = "success")]
    Success {
        /// These events are validated separately from summary decoding.
        events: Vec<Value>,
    },
    /// This result contains the legacy rejection object.
    #[serde(rename = "reject")]
    Reject {
        /// This reason retains the typed dynamic rejection fields.
        #[serde(rename = "rejectReason")]
        reason: Rejection,
    },
}

/// Check alias equality with the first 29 account-address bytes.
pub(super) fn same_account(left: AccountAddress, right: AccountAddress) -> bool {
    left.0[..29] == right.0[..29]
}

/// Decode the complete page without returning any partially formatted rows.
pub(super) fn page(
    entries: Vec<HistoryEntry>,
    account: AccountAddress,
    version: HistoryVersion,
    raw: bool,
) -> anyhow::Result<Vec<Value>> {
    entries
        .into_iter()
        .map(|entry| value(entry, account, version != HistoryVersion::V0, raw))
        .collect()
}

/// Format a persisted entry with the legacy block fields and summary envelope.
fn value(
    mut entry: HistoryEntry,
    account: AccountAddress,
    memos: bool,
    raw: bool,
) -> anyhow::Result<Value> {
    super::numbers::normalize(&mut entry.summary);
    let envelope = entry
        .summary
        .as_object()
        .ok_or_else(|| anyhow::anyhow!("Invalid summary envelope"))?;
    anyhow::ensure!(envelope.len() == 1, "Invalid Either summary envelope");
    let mut output = if let Some(summary) = envelope.get("Left") {
        anyhow::ensure!(
            summary["type"].get("contents").is_some(),
            "Missing summary type contents"
        );
        transaction(
            serde_json::from_value(summary.clone())?,
            account,
            memos,
            raw,
        )?
    } else if let Some(reward) = envelope.get("Right") {
        rewards::value(reward.clone(), account)?
    } else {
        anyhow::bail!("Missing legacy summary envelope");
    };
    output["id"] = json!(entry.id);
    output["blockHash"] = json!(entry.block);
    // Haskell converts the exact millisecond rational to Double before encoding.
    output["blockTime"] = json!(
        BigRational::new(BigInt::from(entry.timestamp), BigInt::from(1000))
            .to_f64()
            .expect("A Word64 timestamp fits a Double")
    );
    output["blockHeight"] = json!(entry.height);
    Ok(output)
}

/// Attribute effects and costs for one validated transaction summary.
fn transaction(
    summary: Summary,
    self_account: AccountAddress,
    memos: bool,
    raw: bool,
) -> anyhow::Result<Value> {
    let self_origin = summary
        .sender
        .is_some_and(|sender| same_account(sender, self_account));
    let origin = match summary.sender {
        Some(_) if self_origin => json!({"type":"self"}),
        Some(sender) => json!({"type":"account","address":sender}),
        None => json!({"type":"none"}),
    };
    let (kind, description) = summary.kind.labels(memos)?;
    let mut details = json!({"type":kind,"description":description});
    let (subtotal, encrypted) = match summary.result {
        ResultDetails::Success { events } => {
            let events = events
                .into_iter()
                .map(Event::decode)
                .collect::<anyhow::Result<Vec<_>>>()?;
            details["outcome"] = json!("success");
            details["events"] = json!(
                events
                    .iter()
                    .filter(|event| memos || event.tag() != "TransferMemo")
                    .map(|event| &event.description)
                    .collect::<Vec<_>>()
            );
            let name = summary.kind.account_name();
            effects(&mut details, &events, name.as_deref(), memos)?;
            let subtotal = subtotal(&events, self_account)?;
            let encrypted = encrypted(&events, name.as_deref(), summary.sender, self_origin);
            (subtotal, encrypted)
        }
        ResultDetails::Reject { reason } => {
            details["outcome"] = json!("reject");
            details["rejectReason"] = json!(reason.description()?);
            if raw {
                details["rawRejectReason"] = reason.raw_value()?;
            }
            (None, None)
        }
    };
    let cost = match &summary.sponsor_details {
        Some(sponsor) if same_account(sponsor.sponsor, self_account) => {
            Some(i128::from(sponsor.cost.micro_ccd))
        }
        _ if self_origin => Some(i128::from(summary.cost.micro_ccd)),
        _ => None,
    };
    let mut output = json!({"origin":origin,"energy":summary.energy_cost,"details":details,"transactionHash":summary.hash,"total":(subtotal.unwrap_or(0) - cost.unwrap_or(0)).to_string()});
    if let Some(cost) = cost {
        output["cost"] = json!(cost.to_string());
        if let Some(subtotal) = subtotal {
            output["subtotal"] = json!(subtotal.to_string());
        }
    }
    if let Some(sponsor) = summary.sponsor_details {
        output["sponsor"] = serde_json::to_value(sponsor)?;
    }
    if let Some(encrypted) = encrypted {
        output["encrypted"] = encrypted;
    }
    Ok(output)
}

/// Copy normalized event fields into transaction details.
fn copy(output: &mut Value, event: &Event, fields: &[(&str, &str)]) {
    for (target, source) in fields {
        output[*target] = event.value[*source].clone();
    }
}

/// Add the conditional fields only for the legacy type and exact event pattern.
fn effects(
    details: &mut Value,
    events: &[Event],
    kind: Option<&str>,
    memos: bool,
) -> anyhow::Result<()> {
    let tags: Vec<_> = events.iter().map(Event::tag).collect();
    let mut memo_from = None;
    match (kind, tags.as_slice()) {
        (Some("transfer" | "transferWithMemo"), ["Transferred", ..])
            if is_account(&events[0].value["from"]) && is_account(&events[0].value["to"]) =>
        {
            details["transferSource"] = events[0].value["from"]["address"].clone();
            details["transferDestination"] = events[0].value["to"]["address"].clone();
            details["transferAmount"] = events[0].value["amount"].clone();
            memo_from = Some(1);
        }
        (
            Some("encryptedAmountTransfer" | "encryptedAmountTransferWithMemo"),
            ["EncryptedAmountsRemoved", "NewEncryptedAmount", ..],
        ) => {
            copy(
                details,
                &events[0],
                &[
                    ("transferSource", "account"),
                    ("aggregatedIndex", "upToIndex"),
                    ("inputEncryptedAmount", "inputAmount"),
                    ("newSelfEncryptedAmount", "newAmount"),
                ],
            );
            copy(
                details,
                &events[1],
                &[
                    ("transferDestination", "account"),
                    ("encryptedAmount", "encryptedAmount"),
                    ("newIndex", "newIndex"),
                ],
            );
            memo_from = Some(2);
        }
        (Some("transferToPublic"), ["EncryptedAmountsRemoved", "AmountAddedByDecryption"]) => {
            copy(
                details,
                &events[0],
                &[
                    ("transferSource", "account"),
                    ("aggregatedIndex", "upToIndex"),
                    ("inputEncryptedAmount", "inputAmount"),
                    ("newSelfEncryptedAmount", "newAmount"),
                ],
            );
            copy(details, &events[1], &[("amountAdded", "amount")]);
        }
        (Some("transferToEncrypted"), ["EncryptedSelfAmountAdded"]) => copy(
            details,
            &events[0],
            &[
                ("transferSource", "account"),
                ("amountSubtracted", "amount"),
                ("newSelfEncryptedAmount", "newAmount"),
            ],
        ),
        (
            Some("transferWithSchedule" | "transferWithScheduleAndMemo"),
            ["TransferredWithSchedule", ..],
        ) => {
            copy(details, &events[0], &[("transferDestination", "to")]);
            details["transferAmount"] = json!(schedule(&events[0])?.to_string());
            memo_from = Some(1);
        }
        (Some("tokenUpdate" | "metaUpdate"), ["TokenTransfer"]) => {
            details["transferSource"] = events[0].value["from"]["address"].clone();
            details["transferDestination"] = events[0].value["to"]["address"].clone();
            copy(
                details,
                &events[0],
                &[("tokenId", "tokenId"), ("tokenTransferAmount", "amount")],
            );
            for name in ["fromLock", "toLock", "memo"] {
                if let Some(value) = events[0].value.get(name).filter(|value| !value.is_null()) {
                    details[name] = value.clone();
                }
            }
        }
        (Some("metaUpdate"), ["LockCreated"]) => copy(
            details,
            &events[0],
            &[("lockId", "lockId"), ("lockConfig", "lockConfig")],
        ),
        (Some("metaUpdate"), ["LockDestroyed"]) => {
            copy(details, &events[0], &[("lockId", "lockId")])
        }
        (Some("registerData"), ["DataRegistered"]) => {
            copy(details, &events[0], &[("registeredData", "data")])
        }
        _ => {}
    }
    if memos
        && let Some(index) = memo_from
        && let [memo] = &events[index..]
        && memo.tag() == "TransferMemo"
    {
        details["memo"] = memo.value["memo"].clone();
    }
    Ok(())
}

/// Check the legacy account address tag.
fn is_account(value: &Value) -> bool {
    value["type"] == "AddressAccount"
}

/// Sum scheduled releases with the Amount Word64 behavior.
fn schedule(event: &Event) -> anyhow::Result<u64> {
    let releases: Vec<(u64, Amount)> = serde_json::from_value(event.value["amount"].clone())?;
    Ok(releases
        .into_iter()
        .fold(0_u64, |sum, (_, amount)| sum.wrapping_add(amount.micro_ccd)))
}

/// Sum only the public-balance effects represented by the legacy formatter.
fn subtotal(events: &[Event], self_account: AccountAddress) -> anyhow::Result<Option<i128>> {
    let mut total = None;
    for event in events {
        let amount = || -> anyhow::Result<i128> {
            Ok(i128::from(
                serde_json::from_value::<Amount>(event.value["amount"].clone())?.micro_ccd,
            ))
        };
        let own_address = |value: &Value| -> anyhow::Result<bool> {
            Ok(is_account(value)
                && same_account(
                    serde_json::from_value(value["address"].clone())?,
                    self_account,
                ))
        };
        let effect = match event.tag() {
            "ContractInitialized" => Some(-amount()?),
            "Updated" if own_address(&event.value["instigator"])? => Some(-amount()?),
            "Transferred" => match (
                own_address(&event.value["from"])?,
                own_address(&event.value["to"])?,
            ) {
                (true, true) => Some(0),
                (true, false) => Some(-amount()?),
                (false, true) => Some(amount()?),
                _ => None,
            },
            "TransferredWithSchedule" => Some(
                i128::from(schedule(event)?)
                    * if same_account(account(&event.value, "from")?, self_account) {
                        -1
                    } else {
                        1
                    },
            ),
            "AmountAddedByDecryption" => Some(amount()?),
            "EncryptedSelfAmountAdded" => Some(-amount()?),
            _ => None,
        };
        if let Some(effect) = effect {
            total = Some(total.unwrap_or(0) + effect);
        }
    }
    Ok(total)
}

/// Select sender or receiver encrypted-balance fields without inventing missing details.
fn encrypted(
    events: &[Event],
    kind: Option<&str>,
    sender: Option<AccountAddress>,
    own: bool,
) -> Option<Value> {
    sender?;
    let tags: Vec<_> = events.iter().map(Event::tag).collect();
    match (kind, tags.as_slice(), own) {
        (
            Some("encryptedAmountTransfer" | "encryptedAmountTransferWithMemo"),
            ["EncryptedAmountsRemoved", "NewEncryptedAmount", ..],
            true,
        ) => Some(
            json!({"encryptedAmount":events[1].value["encryptedAmount"],"newStartIndex":events[0].value["upToIndex"],"newSelfEncryptedAmount":events[0].value["newAmount"]}),
        ),
        (
            Some("encryptedAmountTransfer" | "encryptedAmountTransferWithMemo"),
            ["EncryptedAmountsRemoved", "NewEncryptedAmount", ..],
            false,
        ) => Some(
            json!({"encryptedAmount":events[1].value["encryptedAmount"],"newIndex":events[1].value["newIndex"]}),
        ),
        (
            Some("transferToPublic"),
            ["EncryptedAmountsRemoved", "AmountAddedByDecryption"],
            true,
        ) => Some(
            json!({"newStartIndex":events[0].value["upToIndex"],"newSelfEncryptedAmount":events[0].value["newAmount"]}),
        ),
        (Some("transferToEncrypted"), ["EncryptedSelfAmountAdded"], true) => {
            Some(json!({"newSelfEncryptedAmount":events[0].value["newAmount"]}))
        }
        _ => None,
    }
}
