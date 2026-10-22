//! This module serves account nonces and encryption keys.
//! It preserves account-specific validation and missing-account errors.

use crate::{http::error, network::NetworkFailure};
use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::{id::types::AccountAddress, types::queries::AccountNonceResponse};
use serde_json::{Value, json};
use std::{future::Future, sync::Arc};

/// This interface supplies account preparation queries.
pub trait AccountNode: Send + Sync {
    /// Read the next nonce, including pending transactions.
    fn next_nonce(
        &self,
        address: AccountAddress,
    ) -> impl Future<Output = Result<AccountNonceResponse, NetworkFailure>> + Send;
    /// Read the encryption key from the best block in its hexadecimal representation.
    fn encryption_key(
        &self,
        address: AccountAddress,
    ) -> impl Future<Output = Result<String, NetworkFailure>> + Send;
}

/// Build the existing account preparation routes.
pub fn routes<N: AccountNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/accNonce/{address}", get(nonce::<N>))
        .route("/v0/accEncryptionKey/{address}", get(key::<N>))
        .with_state(node)
}

async fn nonce<N: AccountNode>(
    State(node): State<Arc<N>>,
    Path(address): Path<String>,
) -> Response {
    let Ok(address) = address.parse() else {
        return malformed();
    };
    respond(
        node.next_nonce(address)
            .await
            .map(|nonce| json!({"nonce": nonce.nonce, "allFinal": nonce.all_final})),
    )
}

async fn key<N: AccountNode>(State(node): State<Arc<N>>, Path(address): Path<String>) -> Response {
    let Ok(address) = address.parse() else {
        return malformed();
    };
    respond(
        node.encryption_key(address)
            .await
            .map(|key| json!({"accountEncryptionKey": key})),
    )
}

fn malformed() -> Response {
    error(StatusCode::BAD_REQUEST, 1, "Malformed account address")
}

fn respond(result: Result<Value, NetworkFailure>) -> Response {
    match result {
        Ok(value) => Json(value).into_response(),
        Err(failure) if failure.is_not_found() => {
            error(StatusCode::NOT_FOUND, 2, "Account does not exist")
        }
        Err(failure) => failure.response(),
    }
}
