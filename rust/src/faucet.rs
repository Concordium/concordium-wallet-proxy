//! This module serves optional testnet CCD drops with persisted signed transactions.
//! It preserves recipient-keyed retries and legacy recipient balance and nonce checks.

pub mod database;
mod transaction;

use crate::{http::error, network::NetworkFailure, submission::Submission};
use axum::{
    Json, Router,
    body::to_bytes,
    extract::{Path, Request, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::put,
};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    smart_contracts::common::Amount,
    types::{Nonce, hashes::TransactionHash},
};
use serde_json::json;
use std::{future::Future, sync::Arc};
pub use transaction::{DropSettings, DropTime, SystemDropTime};

/// This type stores the legacy account transaction without a block-item tag.
pub struct Record {
    /// This identifier permits deletion of the exact row observed by the request.
    pub id: i64,
    /// These bytes encode the signature, header, and payload.
    pub transaction: Vec<u8>,
}

/// This interface manages only the existing recipient table.
pub trait DropStore: Send + Sync {
    /// Read the transaction for one recipient.
    fn lookup(
        &self,
        address: AccountAddress,
    ) -> impl Future<Output = anyhow::Result<Option<Record>>> + Send;
    /// Insert a signed transaction unless the recipient already exists.
    fn insert(
        &self,
        address: AccountAddress,
        transaction: &[u8],
    ) -> impl Future<Output = anyhow::Result<bool>> + Send;
    /// Delete only the observed record before retrying.
    fn delete(&self, id: i64) -> impl Future<Output = anyhow::Result<()>> + Send;
}

/// This interface supplies the node operations needed by testnet drops.
pub trait DropNode: Send + Sync {
    /// Read finalized account nonce and CCD balance.
    fn account(
        &self,
        address: AccountAddress,
    ) -> impl Future<Output = Result<(Nonce, Amount), NetworkFailure>> + Send;
    /// Read the funding account's next available nonce.
    fn nonce(
        &self,
        address: AccountAddress,
    ) -> impl Future<Output = Result<Nonce, NetworkFailure>> + Send;
    /// Check whether a transaction is pending or successfully finalized.
    fn acceptable(
        &self,
        hash: TransactionHash,
    ) -> impl Future<Output = Result<bool, NetworkFailure>> + Send;
    /// Submit the exact persisted transaction.
    fn submit(
        &self,
        transaction: &Submission,
    ) -> impl Future<Output = Result<(), NetworkFailure>> + Send;
}

/// This service shares drop configuration and controlled dependencies.
pub struct Faucet<N, S, T> {
    /// These settings are absent when the faucet is disabled.
    pub settings: Option<DropSettings>,
    /// This adapter supplies node queries and submissions.
    pub node: Arc<N>,
    /// This adapter stores signed transactions by recipient.
    pub store: Arc<S>,
    /// This source supplies time and release counts.
    pub time: T,
}

/// Build the existing optional drop route, including its disabled response.
pub fn routes<N: DropNode + 'static, S: DropStore + 'static, T: DropTime + 'static>(
    faucet: Arc<Faucet<N, S, T>>,
) -> Router {
    Router::new()
        .route("/v0/testnetGTUDrop/{address}", put(drop::<N, S, T>))
        .with_state(faucet)
}

async fn drop<N: DropNode, S: DropStore, T: DropTime>(
    State(faucet): State<Arc<Faucet<N, S, T>>>,
    Path(address): Path<String>,
    request: Request,
) -> Response {
    let Some(settings) = &faucet.settings else {
        return error(StatusCode::NOT_FOUND, 2, "Not found");
    };
    let Ok(address) = address.parse() else {
        return error(StatusCode::BAD_REQUEST, 1, "Malformed account address");
    };
    if let Err(failure) = faucet.node.account(address).await {
        return if failure.is_not_found() {
            error(
                StatusCode::BAD_REQUEST,
                1,
                "Account creation is not finalized",
            )
        } else {
            failure.response()
        };
    }
    let body = match to_bytes(request.into_body(), usize::MAX).await {
        Ok(body) => body,
        Err(_) => {
            return error(
                StatusCode::INTERNAL_SERVER_ERROR,
                0,
                "Internal server error",
            );
        }
    };
    let scheduled = match crate::compatibility::json::read(&body) {
        Err(_) => false,
        Ok(value) => match value.get("type").and_then(|value| value.as_str()) {
            Some("normal") => false,
            Some("scheduled") => true,
            _ => return configuration(StatusCode::BAD_REQUEST, 1),
        },
    };
    match execute(&faucet, settings, address, scheduled).await {
        Ok(hash) => Json(json!({"submissionId":hash})).into_response(),
        Err(Failure::Node(failure)) => failure.response(),
        Err(Failure::Configuration) => configuration(StatusCode::BAD_GATEWAY, 0),
        Err(Failure::Submission) => configuration(StatusCode::BAD_REQUEST, 1),
        Err(Failure::Internal(failure)) => {
            tracing::error!(error = %failure, "Testnet drop failed");
            error(
                StatusCode::INTERNAL_SERVER_ERROR,
                0,
                "Internal server error",
            )
        }
    }
}

/// This type separates dependency failures from legacy configuration failures.
enum Failure {
    /// This failure came from the node adapter.
    Node(
        /// This failure retains the shared node error classification.
        NetworkFailure,
    ),
    /// The recipient balance failed the legacy funding check.
    Configuration,
    /// The node did not accept the submitted transaction.
    Submission,
    /// Storage or stored transaction decoding failed.
    Internal(
        /// This error supplies an internal diagnostic without signed transaction contents.
        anyhow::Error,
    ),
}
impl From<NetworkFailure> for Failure {
    fn from(value: NetworkFailure) -> Self {
        Self::Node(value)
    }
}
impl From<anyhow::Error> for Failure {
    fn from(value: anyhow::Error) -> Self {
        Self::Internal(value)
    }
}
fn configuration(status: StatusCode, code: u8) -> Response {
    error(status, code, "Server configuration error")
}

async fn execute<N: DropNode, S: DropStore, T: DropTime>(
    faucet: &Faucet<N, S, T>,
    settings: &DropSettings,
    address: AccountAddress,
    scheduled: bool,
) -> Result<TransactionHash, Failure> {
    loop {
        let transaction = match faucet.store.lookup(address).await? {
            None => {
                let nonce = faucet.node.nonce(settings.address).await?;
                let transaction =
                    transaction::build(settings, address, nonce, scheduled, &faucet.time)?;
                let bytes = concordium_rust_sdk::common::to_bytes(&transaction);
                if !faucet.store.insert(address, &bytes).await? {
                    continue;
                }
                transaction
            }
            Some(record) => {
                let transaction = transaction::decode(&record.transaction)?;
                // The legacy code queries the recipient here, not the funding account.
                let (nonce, balance) = faucet.node.account(address).await?;
                let hash = Submission::AccountTransaction(transaction.clone()).hash();
                if faucet.node.acceptable(hash).await? {
                    return Ok(hash);
                }
                if balance.micro_ccd < settings.amount.micro_ccd.wrapping_add(1_000_000) {
                    return Err(Failure::Configuration);
                }
                if nonce > transaction.header.nonce
                    || transaction.header.expiry.seconds < faucet.time.seconds()
                {
                    faucet.store.delete(record.id).await?;
                    continue;
                }
                transaction
            }
        };
        let item = Submission::AccountTransaction(transaction);
        faucet
            .node
            .submit(&item)
            .await
            .map_err(|_| Failure::Submission)?;
        return Ok(item.hash());
    }
}
