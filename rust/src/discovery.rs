//! This module discovers accounts from legacy public-key bindings.
//! It preserves key parsing, query precedence, and the existing response fields.

use crate::http::error;
use axum::{
    Json, Router,
    extract::{Path, Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::{
    common::from_bytes,
    id::types::{AccountAddress, VerifyKey},
};
use serde_json::{Value, json};
use std::{future::Future, io::Cursor, sync::Arc};

/// This type holds one legacy public-key binding.
pub struct Binding {
    /// This address identifies the account.
    pub address: AccountAddress,
    /// This index identifies the account credential.
    pub credential_index: u8,
    /// This index identifies the credential key.
    pub key_index: u8,
    /// This flag identifies a simple account.
    pub is_simple_account: bool,
}

/// This interface reads public-key bindings without changing external tables.
pub trait DiscoveryHistory: Send + Sync {
    /// Read bindings in ascending credential-index order, retaining duplicates.
    fn key_accounts(
        &self,
        key: [u8; 33],
        only_simple: bool,
    ) -> impl Future<Output = anyhow::Result<Vec<Binding>>> + Send;
}

/// Build the existing public-key discovery route.
pub fn routes<H: DiscoveryHistory + 'static>(history: Arc<H>) -> Router {
    Router::new()
        .route("/v0/keyAccounts/{key}", get(discover::<H>))
        .with_state(history)
}

async fn discover<H: DiscoveryHistory>(
    State(history): State<Arc<H>>,
    Path(key): Path<String>,
    Query(query): Query<Vec<(String, String)>>,
) -> Response {
    let key = match parse_key(&key) {
        Ok(key) => key,
        Err(message) => return parse_error(&format!("Invalid verify key: {message}")),
    };
    let only_simple = match query
        .iter()
        .find(|(name, _)| name == "onlySimple")
        .map(|(_, value)| value.as_str())
    {
        None | Some("n") => false,
        Some("y") => true,
        Some(other) => {
            return parse_error(&format!(
                "Invalid `onlySimple` parameter (should be `y` or `n`): {other}"
            ));
        }
    };
    match history.key_accounts(key, only_simple).await {
        Ok(bindings) => {
            let raw_key: [u8; 32] = key[1..].try_into().expect("The key has 32 bytes");
            let verify_key = json!({"schemeId": "Ed25519", "verifyKey": concordium_rust_sdk::common::base16_encode_string(&raw_key)});
            let values: Vec<Value> = bindings
                .into_iter()
                .map(|binding| {
                    json!({
                        "public_key": verify_key,
                        "address": binding.address,
                        "credential_index": binding.credential_index,
                        "key_index": binding.key_index,
                        "is_simple_account": binding.is_simple_account,
                    })
                })
                .collect();
            Json(values).into_response()
        }
        Err(failure) => {
            tracing::warn!(%failure, "Public-key account discovery failed");
            error(
                StatusCode::INTERNAL_SERVER_ERROR,
                0,
                "Internal server error",
            )
        }
    }
}

fn parse_error(message: &str) -> Response {
    error(
        StatusCode::BAD_REQUEST,
        1,
        &format!("Parsing input failed: {message}"),
    )
}

fn parse_key(text: &str) -> Result<[u8; 33], String> {
    // Base16 validates the entire input before Cereal reads its first 32 bytes.
    // Cereal accepts trailing bytes, but does not include them in the stored key.
    let bytes = text.as_bytes();
    if !bytes.len().is_multiple_of(2) {
        return Err("invalid bytestring size".into());
    }
    let mut key = [0; 33];
    for (index, pair) in bytes.as_chunks::<2>().0.iter().enumerate() {
        let mut byte = 0;
        for (offset, digit) in pair.iter().enumerate() {
            let nibble = match digit {
                b'0'..=b'9' => digit - b'0',
                b'a'..=b'f' => digit - b'a' + 10,
                b'A'..=b'F' => digit - b'A' + 10,
                _ => {
                    return Err(format!(
                        "invalid character at offset: {}",
                        index * 2 + offset
                    ));
                }
            };
            byte = byte * 16 + nibble;
        }
        if index < 32 {
            key[index + 1] = byte;
        }
    }
    if bytes.len() < 64 {
        return Err("too few bytes\nFrom:\tdemandInput\n\n".into());
    }
    from_bytes::<VerifyKey, _>(&mut Cursor::new(key)).map_err(|_| {
        "Failed reading: Cannot decode verification key.\nEmpty call stack\n".to_string()
    })?;
    Ok(key)
}
