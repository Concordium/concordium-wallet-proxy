//! This module submits credential deployments and standard account transactions.
//! It calculates identifiers locally and never treats a rejected submission as success.

mod request;

use crate::{http::error, network::NetworkFailure};
use axum::{
    Json, Router,
    body::Bytes,
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::put,
};
use concordium_rust_sdk::types::transactions::{BlockItem, EncodedPayload};
use std::{future::Future, sync::Arc};

/// This type contains a fully decoded submission with an opaque account payload.
pub type Submission = BlockItem<EncodedPayload>;

/// This interface sends decoded submissions without signing or modifying them.
pub trait SubmissionNode: Send + Sync {
    /// Submit one block item with the existing transport-reconnection policy.
    /// The response hash is not the wallet's identifier; that is calculated locally.
    fn submit(&self, item: &Submission) -> impl Future<Output = Result<(), NetworkFailure>> + Send;
}

/// Build the existing credential and standard transaction submission routes.
pub fn routes<N: SubmissionNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/submitCredential", put(credential::<N>))
        .route("/v0/submitTransfer", put(transfer::<N>))
        .with_state(node)
}

async fn credential<N: SubmissionNode>(State(node): State<Arc<N>>, body: Bytes) -> Response {
    send(
        node,
        request::credential(&body),
        "Credential rejected by the node",
    )
    .await
}

async fn transfer<N: SubmissionNode>(State(node): State<Arc<N>>, body: Bytes) -> Response {
    send(
        node,
        request::transaction(&body),
        "Transaction rejected by the node",
    )
    .await
}

async fn send<N: SubmissionNode>(
    node: Arc<N>,
    decoded: anyhow::Result<Submission>,
    rejected: &str,
) -> Response {
    let item = match decoded {
        Ok(item) => item,
        Err(failure) => {
            // Do not log credential proofs, transaction contents, or signatures.
            tracing::debug!("Submission request failed validation");
            return error(
                StatusCode::BAD_REQUEST,
                1,
                &format!("Parsing input failed: {failure}"),
            );
        }
    };
    let hash = item.hash();
    if node.submit(&item).await.is_err() {
        // Haskell maps transport failures, timeouts, and RPC rejection to rejection.
        return error(StatusCode::BAD_REQUEST, 1, rejected);
    }
    Json(serde_json::json!({"submissionId": hash})).into_response()
}
