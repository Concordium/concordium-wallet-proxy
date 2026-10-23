//! This module projects typed SDK summaries into the legacy transaction schema.
//! Unknown variants fail the complete response instead of producing partial outcomes.

mod effects;
mod events;
mod keys;
mod payloads;

use crate::network::NetworkFailure;
use concordium_rust_sdk::{
    types::{AccountTransactionEffects, BlockItemSummary, BlockItemSummaryDetails},
    v2::Upward,
};
use serde::Serialize;
use serde_json::{Value, json};

fn known<T>(value: &Upward<T>) -> Result<&T, NetworkFailure> {
    value.as_known().ok_or(NetworkFailure::InvalidResponse)
}
fn encode(value: impl Serialize) -> Result<Value, NetworkFailure> {
    serde_json::to_value(value).map_err(|_| NetworkFailure::InvalidResponse)
}
fn select(value: impl Serialize, fields: &[&str]) -> Result<Value, NetworkFailure> {
    let source = encode(value)?;
    let mut output = json!({});
    for field in fields {
        output[*field] = source
            .get(*field)
            .ok_or(NetworkFailure::InvalidResponse)?
            .clone();
    }
    Ok(output)
}
fn leaf(tag: &str, value: impl Serialize, fields: &[&str]) -> Result<Value, NetworkFailure> {
    Ok(tagged(tag, select(value, fields)?))
}
fn success(events: Vec<Value>) -> Value {
    json!({"outcome": "success", "events": events})
}
fn tagged(tag: &str, mut fields: Value) -> Value {
    fields["tag"] = json!(tag);
    fields
}

pub(super) fn value(summary: &BlockItemSummary) -> Result<Value, NetworkFailure> {
    let mut value = json!({"sender": null, "sponsorDetails": null, "hash": summary.hash, "cost": "0", "energyCost": summary.energy_cost, "index": summary.index});
    let (kind, contents, result) = match known(&summary.details)? {
        BlockItemSummaryDetails::AccountTransaction(details) => {
            value["sender"] = json!(details.sender);
            value["cost"] = json!(details.cost.micro_ccd.to_string());
            if let Some(sponsor) = &details.sponsor {
                value["sponsorDetails"] =
                    json!({"sponsor": sponsor.sponsor, "cost": sponsor.cost.micro_ccd.to_string()});
            }
            let effect = known(&details.effects)?;
            let result = match effect {
                AccountTransactionEffects::None { reject_reason, .. } => {
                    json!({"outcome": "reject", "rejectReason": payloads::reject(known(reject_reason)?)?})
                }
                _ => success(effects::events(effect, details.sender)?),
            };
            (
                "accountTransaction",
                encode(effect.transaction_type())?,
                result,
            )
        }
        BlockItemSummaryDetails::AccountCreation(details) => (
            "credentialDeploymentTransaction",
            encode(details.credential_type)?,
            success(vec![
                tagged("AccountCreated", json!({"contents": details.address})),
                tagged(
                    "CredentialDeployed",
                    json!({"regId": details.reg_id, "account": details.address}),
                ),
            ]),
        ),
        BlockItemSummaryDetails::Update(details) => {
            let payload = known(&details.payload)?;
            (
                "updateTransaction",
                payloads::update_type(payload)?,
                success(vec![tagged(
                    "UpdateEnqueued",
                    json!({"effectiveTime": details.effective_time, "payload": payloads::update(payload)?}),
                )]),
            )
        }
        BlockItemSummaryDetails::TokenCreationDetails(details) => {
            let mut output = vec![tagged(
                "TokenCreated",
                json!({"payload": {"tokenId": details.create_plt.token_id, "tokenModule": details.create_plt.token_module, "decimals": details.create_plt.decimals, "initializationParameters": details.create_plt.initialization_parameters}}),
            )];
            for event in &details.events {
                output.push(events::token(event)?);
            }
            (
                "updateTransaction",
                json!("updateCreatePLT"),
                success(output),
            )
        }
    };
    value["type"] = json!({"type": kind, "contents": contents});
    value["result"] = result;
    Ok(value)
}
