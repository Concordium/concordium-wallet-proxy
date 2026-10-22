//! This module serves cached cryptographic parameters and live network information.
//! It selects the wallet-facing consensus fields without SDK statistics.

use crate::{dependencies::Dependencies, http::error};
use axum::{
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use concordium_rust_sdk::types::{queries::ConsensusInfo, CryptographicParameters};
use serde_json::{json, Value};
use std::{future::Future, sync::Arc};

/// This interface supplies live consensus information.
pub trait NetworkNode: Send + Sync {
    /// Read the current consensus state.
    fn consensus_info(&self) -> impl Future<Output = Result<ConsensusInfo, NetworkFailure>> + Send;
}

/// This type preserves the source of a failed network query.
#[derive(Debug)]
pub enum NetworkFailure {
    /// The transport connection failed.
    Transport,
    /// The SDK could not decode the response.
    InvalidResponse,
    /// The node returned a non-OK RPC status.
    Status(
        /// This status contains the node's code and message.
        tonic::Status,
    ),
}

/// This type shares cached startup data and a live node adapter.
pub struct Network<D: Dependencies> {
    /// This adapter supplies independent live queries.
    pub node: D::Node,
    /// These parameters were acquired before the listener started.
    pub parameters: Arc<CryptographicParameters>,
}

impl<D: Dependencies> Clone for Network<D> {
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
            parameters: self.parameters.clone(),
        }
    }
}

/// Build the existing network-information routes.
pub fn routes<D: Dependencies + 'static>(network: Network<D>) -> Router
where
    D::Node: NetworkNode + 'static,
{
    Router::new()
        .route("/v0/global", get(global::<D>))
        .route("/v0/epochLength", get(epoch::<D>))
        .route("/v0/genesisHash", get(genesis::<D>))
        .route("/v0/consensusInfo", get(consensus::<D>))
        .with_state(network)
}

async fn global<D: Dependencies>(State(network): State<Network<D>>) -> Json<Value>
where
    D::Node: NetworkNode,
{
    Json(json!({"v": 0, "value": network.parameters.as_ref()}))
}

async fn epoch<D: Dependencies>(State(network): State<Network<D>>) -> Response
where
    D::Node: NetworkNode,
{
    match network.node.consensus_info().await {
        Ok(info) => {
            Json(json!({"epochLength": info.epoch_duration.num_milliseconds()})).into_response()
        }
        Err(failure) => failure.response(),
    }
}

async fn genesis<D: Dependencies>(State(network): State<Network<D>>) -> Response
where
    D::Node: NetworkNode,
{
    match network.node.consensus_info().await {
        Ok(info) => Json(json!({"genesis_block": info.genesis_block})).into_response(),
        Err(failure) => failure.response(),
    }
}

async fn consensus<D: Dependencies>(State(network): State<Network<D>>) -> Response
where
    D::Node: NetworkNode,
{
    match network.node.consensus_info().await {
        Ok(info) => Json(consensus_value(&info)).into_response(),
        Err(failure) => failure.response(),
    }
}

/// Select the fields and units used by the Haskell HTTP handler.
pub fn consensus_value(info: &ConsensusInfo) -> Value {
    let mut value = json!({
        "bestBlock": info.best_block,
        "bestBlockHeight": info.best_block_height,
        "genesisBlock": info.genesis_block,
        "genesisTime": info.genesis_time,
        "lastFinalizedBlock": info.last_finalized_block,
        "lastFinalizedBlockHeight": info.last_finalized_block_height,
        "lastFinalizedTime": info.last_finalized_time,
        "epochDuration": info.epoch_duration.num_milliseconds(),
        "protocolVersion": info.protocol_version,
        "genesisIndex": info.genesis_index,
        "currentEraGenesisBlock": info.current_era_genesis_block,
        "currentEraGenesisTime": info.current_era_genesis_time,
    });
    if let Some(slot) = info.slot_duration {
        value["slotDuration"] = json!(slot);
    }
    if let Some(bft) = &info.concordium_bft_status {
        value["concordiumBFTStatus"] = json!({
            "currentTimeoutDuration": bft.current_timeout_duration.num_milliseconds(),
            "currentRound": bft.current_round,
            "currentEpoch": bft.current_epoch,
            "triggerBlockTime": bft.trigger_block_time,
        });
    }
    value
}

impl NetworkFailure {
    fn response(self) -> Response {
        tracing::warn!(failure = ?self, "Network-information query failed");
        match self {
            Self::Transport => error(
                StatusCode::BAD_GATEWAY,
                0,
                "Cannot communicate with node via GRPC endpoint.",
            ),
            Self::InvalidResponse => error(
                StatusCode::INTERNAL_SERVER_ERROR,
                0,
                "Internal server error",
            ),
            Self::Status(status) => {
                let (http, code, message) = match status.code() {
                    tonic::Code::NotFound => (
                        StatusCode::NOT_FOUND,
                        2,
                        format!("Requested object was not found: {}", status.message()),
                    ),
                    tonic::Code::Cancelled
                    | tonic::Code::ResourceExhausted
                    | tonic::Code::DeadlineExceeded => (
                        StatusCode::SERVICE_UNAVAILABLE,
                        3,
                        format!(
                            "The node is overloaded so the request was cancelled: {}",
                            status.message()
                        ),
                    ),
                    other => (
                        StatusCode::BAD_GATEWAY,
                        0,
                        format!("Non-OK GRPC status code '{}'.", rpc_code_name(other)),
                    ),
                };
                error(http, code, &format!("GRPC call failed: {message}"))
            }
        }
    }
}

fn rpc_code_name(code: tonic::Code) -> &'static str {
    match code {
        tonic::Code::Ok => "OK",
        tonic::Code::Cancelled => "CANCELLED",
        tonic::Code::Unknown => "UNKNOWN",
        tonic::Code::InvalidArgument => "INVALID_ARGUMENT",
        tonic::Code::DeadlineExceeded => "DEADLINE_EXCEEDED",
        tonic::Code::NotFound => "NOT_FOUND",
        tonic::Code::AlreadyExists => "ALREADY_EXISTS",
        tonic::Code::PermissionDenied => "PERMISSION_DENIED",
        tonic::Code::ResourceExhausted => "RESOURCE_EXHAUSTED",
        tonic::Code::FailedPrecondition => "FAILED_PRECONDITION",
        tonic::Code::Aborted => "ABORTED",
        tonic::Code::OutOfRange => "OUT_OF_RANGE",
        tonic::Code::Unimplemented => "UNIMPLEMENTED",
        tonic::Code::Internal => "INTERNAL",
        tonic::Code::Unavailable => "UNAVAILABLE",
        tonic::Code::DataLoss => "DATA_LOSS",
        tonic::Code::Unauthenticated => "UNAUTHENTICATED",
    }
}
