//! This module reports submission lifecycle states and legacy simple outcomes.
//! It retains sorted block references and does not expose SDK transaction-summary JSON.

mod outcome;
use crate::compatibility::rejection;

use crate::{http::error, network::NetworkFailure};
use axum::{
    Json, Router,
    extract::{Path, State},
    http::{StatusCode, header},
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::types::{TransactionStatus, hashes::TransactionHash};
use serde_json::{Value, json};
use std::{future::Future, sync::Arc};

/// This interface looks up submission status without changing chain state.
pub trait ProgressNode: Send + Sync {
    /// Read lifecycle status for the supplied transaction hash.
    fn submission_status(
        &self,
        hash: TransactionHash,
    ) -> impl Future<Output = Result<TransactionStatus, NetworkFailure>> + Send;
}

/// Build the existing submission-status route.
pub fn routes<N: ProgressNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/submissionStatus/{hash}", get(status::<N>))
        .with_state(node)
}

async fn status<N: ProgressNode>(State(node): State<Arc<N>>, Path(hash): Path<String>) -> Response {
    let Ok(hash) = hash.trim_end().parse() else {
        return error(StatusCode::BAD_REQUEST, 1, "Malformed transaction hash");
    };
    let result = match node.submission_status(hash).await {
        Ok(status) => value(status),
        Err(failure) if failure.is_not_found() => Ok(json!({"status":"absent"})),
        Err(failure) => Err(ProgressError::Node(failure)),
    };
    match result {
        Ok(value) => ([(header::CONTENT_LANGUAGE, "en")], Json(value)).into_response(),
        Err(ProgressError::Unsupported) => error(
            StatusCode::BAD_REQUEST,
            1,
            "GRPC call failed: Unsupported transaction type for simple statuses.",
        ),
        Err(ProgressError::Unexpected) => NetworkFailure::InvalidResponse.response(),
        Err(ProgressError::Node(failure)) => failure.response(),
    }
}

/// This type distinguishes unsupported successful transactions from invalid node data.
enum ProgressError {
    /// The legacy endpoint does not support this successful transaction type.
    Unsupported,
    /// The node response lacks a required outcome or contains unsupported data.
    Unexpected,
    /// This failure retains the source of a node query or projection error.
    Node(
        /// This failure uses the shared node-error response contract.
        NetworkFailure,
    ),
}
impl From<NetworkFailure> for ProgressError {
    fn from(value: NetworkFailure) -> Self {
        Self::Node(value)
    }
}

/// Convert lifecycle maps while comparing complete projected outcomes before merging fields.
fn value(status: TransactionStatus) -> Result<Value, ProgressError> {
    let (name, outcomes) = match status {
        TransactionStatus::Received => return Ok(json!({"status":"received"})),
        TransactionStatus::Committed(outcomes) => ("committed", outcomes),
        TransactionStatus::Finalized(outcomes) => {
            if outcomes.len() != 1 {
                return Err(ProgressError::Unexpected);
            }
            ("finalized", outcomes)
        }
    };
    let mut output = json!({"status":name,"blockHashes":outcomes.keys().collect::<Vec<_>>()});
    let mut projections = outcomes.values().map(outcome::project);
    let first = projections.next().ok_or(ProgressError::Unexpected)??;
    let mut equal = true;
    for projected in projections {
        equal &= projected? == first;
    }
    if equal {
        output.as_object_mut().unwrap().extend(first.fields());
    } else {
        output["outcome"] = "ambiguous".into();
    }
    Ok(output)
}
