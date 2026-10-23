//! This module reads bare binary block items with the legacy P10 acceptance rules.
//! It uses SDK decoders and keeps account payloads opaque until node validation.

use super::Submission;
use anyhow::Context;
use concordium_rust_sdk::{
    base::updates::{EncodedUpdatePayload, UpdateInstruction, UpdatePayload},
    common::{Deserial, Get},
    types::transactions::{AccountTransaction, AccountTransactionV1, BlockItem, EncodedPayload},
};

/// Read a bare item without including unused trailing bytes in submission or hashing.
pub(super) fn decode(bytes: &[u8]) -> anyhow::Result<Submission> {
    let mut source = std::io::Cursor::new(bytes);
    let tag: u8 = source.get().context("Missing block item tag")?;
    Ok(match tag {
        0 => BlockItem::AccountTransaction(AccountTransaction::<EncodedPayload>::deserial(
            &mut source,
        )?),
        1 => BlockItem::CredentialDeployment(source.get()?),
        2 => {
            let mut update: UpdateInstruction = source.get()?;
            validate_update(&mut update)?;
            BlockItem::UpdateInstruction(update)
        }
        // The SDK's BlockItem decoder omits tag 3, but supplies the supported
        // AccountTransactionV1 decoder and submission variant. Use them directly.
        3 => BlockItem::AccountTransactionV1(AccountTransactionV1::<EncodedPayload>::deserial(
            &mut source,
        )?),
        _ => anyhow::bail!("Unknown block item kind: {tag}"),
    })
}

/// Check P10 governance variants and decode the complete isolated update payload.
fn validate_update(update: &mut UpdateInstruction) -> anyhow::Result<()> {
    let bytes = update.payload.as_ref();
    let tag = bytes.first().context("Missing update payload tag")?;
    let supported = match tag {
        // P10 uses chain parameters version 3 and authorizations version 2.
        1 | 3 | 4 | 5 | 7 | 12..=24 => true,
        10 => matches!(bytes.get(1), Some(0 | 1 | 4)),
        11 => matches!(bytes.get(1), Some(0 | 3)),
        _ => false,
    };
    anyhow::ensure!(
        supported,
        "Update payload is not supported by protocol version 10"
    );
    let mut source = std::io::Cursor::new(bytes);
    let payload =
        UpdatePayload::deserial(&mut source).context("Invalid governance update payload")?;
    anyhow::ensure!(
        source.position() == bytes.len() as u64,
        "Trailing data in isolated update payload"
    );
    // Haskell submits and hashes the decoded payload's serialization, not an
    // unchecked copy of its original bytes. Keep the original header unchanged.
    update.payload = EncodedUpdatePayload::encode(&payload);
    Ok(())
}
