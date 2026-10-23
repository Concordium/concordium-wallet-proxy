//! This module estimates transfer, staking, contract, and token transaction fees.
//! It keeps legacy payload sizes and query-presence rules separate from node access.

mod calculation;
mod contract;
mod numbers;
mod token;
mod token_json;

use crate::{http::error, network::NetworkFailure};
use axum::{
    Json, Router,
    extract::{Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::types::{
    ProtocolVersion,
    chain_parameters::EnergyRate,
    smart_contracts::{ContractContext, InvokeContractResult},
};
use std::{future::Future, sync::Arc};

/// This interface supplies the rate and protocol before request validation.
pub trait FeeNode: Send + Sync {
    /// Read consensus, then chain parameters at Best, without an account query.
    fn fee_context(
        &self,
    ) -> impl Future<Output = Result<(EnergyRate, ProtocolVersion), NetworkFailure>> + Send;
    /// Invoke a contract at Best without submitting a transaction.
    fn invoke_contract(
        &self,
        context: &ContractContext,
    ) -> impl Future<Output = Result<InvokeContractResult, NetworkFailure>> + Send;
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
    let prepared = match calculation::prepare(&query, protocol) {
        Ok(prepared) => prepared,
        Err(failure) => return failure.response(),
    };
    let (energy, success) = match prepared {
        calculation::Estimate::Fixed(energy) => (energy, None),
        calculation::Estimate::Contract(estimate) => {
            let result = match node.invoke_contract(&estimate.context).await {
                Ok(result) => result,
                Err(failure) => return failure.response(),
            };
            (
                estimate.energy(result.used_energy().energy),
                Some(matches!(result, InvokeContractResult::Success { .. })),
            )
        }
    };
    let mut value =
        serde_json::json!({"energy": energy, "cost": calculation::cost(&rate, energy).to_string()});
    if let Some(success) = success {
        value["success"] = success.into();
    }
    Json(value).into_response()
}

/// This type describes a legacy fee-validation error.
enum FeeError {
    /// This message identifies the invalid or missing query value.
    Parse(String),
    /// Protocol version 1 cannot include a memo.
    MemoUnsupported,
}

impl FeeError {
    /// Format an input error with the shared language and classification fields.
    fn response(self) -> Response {
        match self {
            Self::Parse(message) => error(
                StatusCode::BAD_REQUEST,
                1,
                &format!("Parsing input failed: {message}"),
            ),
            Self::MemoUnsupported => error(
                StatusCode::NOT_FOUND,
                2,
                "The required action is not supported. The node's protocol version is incompatible with it.",
            ),
        }
    }
}

/// Read the first query value, including an empty value.
fn first<'a>(query: &'a [(String, String)], key: &str) -> Option<&'a str> {
    query
        .iter()
        .find(|(name, _)| name == key)
        .map(|(_, value)| value.as_str())
}
