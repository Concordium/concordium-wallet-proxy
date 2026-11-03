//! This module decodes persisted token and lock events without inferring node-wire variants.
//! It retains legacy holder text, opaque CBOR, decimal arithmetic, and optional lock metadata.

use super::events::field;
use crate::compatibility::show_bytes;
use concordium_rust_sdk::{
    base::protocol_level_locks::LockId,
    id::types::AccountAddress,
    protocol_level_tokens::{
        RawCbor, TokenAmount, TokenHolder, TokenId, TokenModuleCborTypeDiscriminator,
    },
    types::{CreatePlt, Memo},
};
use serde::Serialize;
use serde_json::Value;

/// Decode one supported event and produce the legacy English description.
pub(super) fn description(tag: &str, value: &mut Value) -> anyhow::Result<String> {
    Ok(match tag {
        "TokenModuleEvent" => {
            let token = field::<TokenId>(value, "tokenId")?;
            let kind = field::<TokenModuleCborTypeDiscriminator>(value, "type")?;
            field::<RawCbor>(value, "details")?;
            format!(
                "{token} token module event occurred of type {}.",
                show_bytes(kind.as_ref().as_bytes())
            )
        }
        "TokenTransfer" => {
            let token = field::<TokenId>(value, "tokenId")?;
            let amount = amount(value)?;
            let from = holder(field::<TokenHolder>(value, "from")?);
            let to = holder(field::<TokenHolder>(value, "to")?);
            let from_lock = optional_lock(value, "fromLock")?;
            let to_lock = optional_lock(value, "toLock")?;
            let memo = if value.get("memo").is_some_and(|memo| !memo.is_null()) {
                field::<Memo>(value, "memo")?;
                format!(
                    " with memo {}",
                    value["memo"]
                        .as_str()
                        .ok_or_else(|| anyhow::anyhow!("Invalid memo"))?
                )
            } else {
                String::new()
            };
            format!("{amount} {token} transferred from {from}{from_lock} to {to}{to_lock}{memo}.")
        }
        "TokenMint" | "TokenBurn" => {
            let token = field::<TokenId>(value, "tokenId")?;
            let amount = amount(value)?;
            let target = holder(field::<TokenHolder>(value, "target")?);
            format!(
                "{amount} {token} {} {target}.",
                if tag == "TokenMint" {
                    "minted to"
                } else {
                    "burned from"
                }
            )
        }
        "TokenCreated" => {
            let payload = field::<CreatePlt>(value, "payload")?;
            // Aeson encodePretty sorts object keys and uses four-space indentation.
            let payload = serde_json::to_value(payload)?;
            let mut bytes = Vec::new();
            let formatter = serde_json::ser::PrettyFormatter::with_indent(b"    ");
            payload.serialize(&mut serde_json::Serializer::with_formatter(
                &mut bytes, formatter,
            ))?;
            format!("Token created: {}", String::from_utf8(bytes)?)
        }
        "LockCreated" => {
            let id = field::<LockId>(value, "lockId")?;
            // The legacy field is raw hexadecimal CBOR, not a decoded lock schema.
            field::<RawCbor>(value, "lockConfig")?;
            format!("Lock created: {id}.")
        }
        "LockDestroyed" => format!("Lock destroyed: {}.", field::<LockId>(value, "lockId")?),
        _ => anyhow::bail!("Unsupported token history event: {tag}"),
    })
}

/// Read the account represented by a legacy token holder.
fn holder(holder: TokenHolder) -> AccountAddress {
    match holder {
        TokenHolder::Account { address } => address,
    }
}

/// Decode an optional lock without treating absence or null as a present identifier.
fn optional_lock(value: &mut Value, name: &str) -> anyhow::Result<String> {
    if value.get(name).is_none_or(Value::is_null) {
        return Ok(String::new());
    }
    Ok(format!(" locked by {}", field::<LockId>(value, name)?))
}

/// Format a token amount with the legacy Haskell TokenRawAmount arithmetic.
fn amount(value: &mut Value) -> anyhow::Result<String> {
    let digits = value["amount"]["value"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("Token value is not decimal text"))?;
    anyhow::ensure!(
        !digits.is_empty() && digits.bytes().all(|byte| byte.is_ascii_digit()),
        "Invalid token decimal text"
    );
    let amount = field::<TokenAmount>(value, "amount")?;
    if amount.decimals() == 0 {
        return Ok(amount.value().to_string());
    }
    // Utils.tokenAmountToString computes the divisor as TokenRawAmount (Word64),
    // not Integer. Retain its wraparound and fail when it divides by zero.
    let divisor = 10_u64.wrapping_pow(u32::from(amount.decimals()));
    anyhow::ensure!(divisor > 0, "Legacy token decimal divisor is zero");
    Ok(format!(
        "{}.{:0>width$}",
        amount.value() / divisor,
        amount.value() % divisor,
        width = usize::from(amount.decimals())
    ))
}
