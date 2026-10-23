//! This module reads the legacy JSON submission envelopes and binary transaction body.
//! SDK types own credential and transaction decoding; malformed inputs can fail locally.

use super::Submission;
use anyhow::{Context, bail};
use concordium_rust_sdk::{
    common::types::{TransactionSignature, TransactionSignaturesV1},
    common::{Deserial, to_bytes},
    types::transactions::{AccountTransaction, AccountTransactionV1, BlockItem, EncodedPayload},
};
use serde_json::Value;

mod json;

/// Read the supported credential envelope version after decoding its contents.
pub(super) fn credential(body: &[u8]) -> anyhow::Result<Submission> {
    let value = json::read(body)?;
    let version = value
        .get("v")
        .and_then(Value::as_u64)
        .context("Missing or invalid credential version")?;
    let mut credential = value
        .get("value")
        .context("Missing credential value")?
        .clone();
    normalize_credential_keys(&mut credential)?;
    // Decode before checking the version, as the Haskell Versioned parser does.
    let item: Submission =
        concordium_rust_sdk::types::transactions::BlockItem::CredentialDeployment(
            serde_json::from_value(credential).context("Invalid credential deployment")?,
        );
    if version != 0 {
        bail!("Invalid version number {version}");
    }
    Ok(item)
}

/// Read signatures and the unsigned serialized header/payload without decoding payload variants.
pub(super) fn transaction(body: &[u8]) -> anyhow::Result<Submission> {
    let value = json::read(body)?;
    let mut signatures = value
        .get("signatures")
        .context("Missing signatures")?
        .clone();
    let signatures = signature_map(&mut signatures)?;
    let text = value
        .get("transaction")
        .and_then(Value::as_str)
        .context("Missing or invalid transaction")?;
    let binary = hex(text)?;
    // Haskell serializes the JSON signature map, joins it to the body, then decodes.
    // This preserves canonical validation and the wrapped count of a 256-entry map.
    let mut encoded = to_bytes(&signatures);
    encoded.extend_from_slice(&binary);
    let transaction =
        AccountTransaction::<EncodedPayload>::deserial(&mut std::io::Cursor::new(encoded))
            .context("Invalid serialized account transaction")?;
    // Cereal decode accepts unused bytes. Do not include them in the submitted item or hash.
    Ok(transaction.into())
}

/// Read sender and optional sponsor signatures before the extended binary body.
pub(super) fn extended(body: &[u8]) -> anyhow::Result<Submission> {
    let value = json::read(body)?;
    let mut signatures = value
        .get("signatures")
        .context("Missing signatures")?
        .clone();
    let sender = signature_map(
        signatures
            .get_mut("sender")
            .context("Missing sender signatures")?,
    )?;
    let sponsor = match signatures.get_mut("sponsor") {
        None | Some(Value::Null) => None,
        Some(value) => Some(signature_map(value)?),
    };
    let signatures = TransactionSignaturesV1 { sender, sponsor };
    let text = value
        .get("transaction")
        .and_then(Value::as_str)
        .context("Missing or invalid transaction")?;
    let mut encoded = to_bytes(&signatures);
    encoded.extend_from_slice(&hex(text)?);
    let transaction =
        AccountTransactionV1::<EncodedPayload>::deserial(&mut std::io::Cursor::new(encoded))
            .context("Invalid serialized extended transaction")?;
    Ok(BlockItem::AccountTransactionV1(transaction))
}

/// Normalize both levels of one indexed signature map before SDK decoding.
fn signature_map(value: &mut Value) -> anyhow::Result<TransactionSignature> {
    json::indices(value, 255)?;
    for map in value
        .as_object_mut()
        .context("Invalid signature map")?
        .values_mut()
    {
        json::indices(map, 255)?;
    }
    serde_json::from_value(std::mem::take(value)).context("Invalid signatures")
}

/// Normalize integral JSON key aliases in the credential's indexed maps.
fn normalize_credential_keys(value: &mut Value) -> anyhow::Result<()> {
    let Some(contents) = value.pointer_mut("/credential/contents") else {
        return Ok(());
    };
    if let Some(keys) = contents.pointer_mut("/credentialPublicKeys/keys") {
        json::indices(keys, 255)?;
    }
    if let Some(revokers) = contents.get_mut("arData") {
        json::indices(revokers, u32::MAX as u64)?;
    }
    Ok(())
}

/// Decode the unsigned transaction bytes without adding whitespace or a prefix convention.
fn hex(text: &str) -> anyhow::Result<Vec<u8>> {
    anyhow::ensure!(
        text.len().is_multiple_of(2),
        "Invalid base-16 transaction length"
    );
    text.as_bytes()
        .as_chunks::<2>()
        .0
        .iter()
        .map(|pair| {
            let high = char::from(pair[0])
                .to_digit(16)
                .context("Invalid base-16 transaction")?;
            let low = char::from(pair[1])
                .to_digit(16)
                .context("Invalid base-16 transaction")?;
            Ok((high * 16 + low) as u8)
        })
        .collect()
}
