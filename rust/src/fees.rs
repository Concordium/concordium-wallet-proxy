//! This module estimates transfer, validator, and delegation fees.
//! It keeps legacy payload sizes and query-presence rules separate from node access.

mod calculation;
mod numbers;

use crate::{http::error, network::NetworkFailure};
use axum::{
    Json, Router,
    extract::{Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::types::{ProtocolVersion, chain_parameters::EnergyRate};
use std::{future::Future, sync::Arc};

/// This interface supplies the rate and protocol before request validation.
pub trait FeeNode: Send + Sync {
    /// Read consensus, then chain parameters at Best, without an account query.
    fn fee_context(
        &self,
    ) -> impl Future<Output = Result<(EnergyRate, ProtocolVersion), NetworkFailure>> + Send;
}

/// Build the existing transaction-cost route.
pub fn routes<N: FeeNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/transactionCost", get(estimate::<N>))
        .with_state(node)
}

async fn estimate<N: FeeNode>(
    State(node): State<Arc<N>>,
    Query(query): Query<Vec<(String, String)>>,
) -> Response {
    let (rate, protocol) = match node.fee_context().await {
        Ok(context) => context,
        Err(failure) => return failure.response(),
    };
    match calculation::energy(&query, protocol) {
        Ok(energy) => Json(serde_json::json!({"energy": energy, "cost": calculation::cost(&rate, energy).to_string()})).into_response(),
        Err(FeeError::Parse(message)) => error(StatusCode::BAD_REQUEST, 1, &format!("Parsing input failed: {message}")),
        Err(FeeError::MemoUnsupported) => error(StatusCode::NOT_FOUND, 2, "The required action is not supported. The node's protocol version is incompatible with it."),
    }
}

/// This type describes a legacy fee-validation error.
enum FeeError {
    /// This message identifies the invalid or missing query value.
    Parse(String),
    /// Protocol version 1 cannot include a memo.
    MemoUnsupported,
}

/// Read the first query value, including an empty value.
fn first<'a>(query: &'a [(String, String)], key: &str) -> Option<&'a str> {
    query
        .iter()
        .find(|(name, _)| name == key)
        .map(|(_, value)| value.as_str())
}
