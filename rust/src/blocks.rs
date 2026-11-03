//! This module serves block metadata, height lookup, and transaction outcomes.
//! It keeps legacy validation and response fields separate from SDK query models.

pub(crate) mod outcomes;

use crate::{compatibility::format_time, http::error, network::NetworkFailure};
use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::types::{BlockItemSummary, hashes::BlockHash, queries::BlockInfo};
use serde_json::{Value, json};
use std::{future::Future, sync::Arc};

/// This interface supplies block queries and complete outcome streams.
pub trait BlockNode: Send + Sync {
    /// Read a block selected by its hash.
    fn block_info(
        &self,
        hash: BlockHash,
    ) -> impl Future<Output = Result<BlockInfo, NetworkFailure>> + Send;
    /// Read hashes at an absolute height in the node's order.
    fn blocks_at_height(
        &self,
        height: u64,
    ) -> impl Future<Output = Result<Vec<BlockHash>, NetworkFailure>> + Send;
    /// Read all block outcomes, failing the request if the stream fails.
    fn transaction_outcomes(
        &self,
        hash: BlockHash,
    ) -> impl Future<Output = Result<Vec<BlockItemSummary>, NetworkFailure>> + Send;
}

/// Build the existing block-query routes.
pub fn routes<N: BlockNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/blockInfo/{hash}", get(info::<N>))
        .route("/v0/blocksAtHeight/{height}", get(height::<N>))
        .route("/v0/blockTransactionEvents/{hash}", get(events::<N>))
        .with_state(node)
}

async fn info<N: BlockNode>(State(node): State<Arc<N>>, Path(hash): Path<String>) -> Response {
    let Ok(hash) = hash.trim_end().parse() else {
        return malformed();
    };
    match node.block_info(hash).await {
        Ok(info) => match info_value(&info) {
            Ok(value) => Json(value).into_response(),
            Err(failure) => failure.response(),
        },
        Err(failure) => block_failure(failure),
    }
}
async fn height<N: BlockNode>(State(node): State<Arc<N>>, Path(height): Path<String>) -> Response {
    let Ok(height) = height.parse() else {
        return error(StatusCode::NOT_FOUND, 1, "Not found");
    };
    match node.blocks_at_height(height).await {
        Ok(hashes) => Json(hashes).into_response(),
        Err(failure) => failure.response(),
    }
}
async fn events<N: BlockNode>(State(node): State<Arc<N>>, Path(hash): Path<String>) -> Response {
    let Ok(hash) = hash.trim_end().parse() else {
        return malformed();
    };
    match node.transaction_outcomes(hash).await {
        Ok(summaries) => match summaries
            .iter()
            .map(outcomes::value)
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(values) => Json(values).into_response(),
            Err(failure) => failure.response(),
        },
        Err(failure) => block_failure(failure),
    }
}
fn malformed() -> Response {
    error(StatusCode::BAD_REQUEST, 1, "Malformed block hash")
}
fn block_failure(failure: NetworkFailure) -> Response {
    if failure.is_not_found() {
        error(StatusCode::NOT_FOUND, 2, "Not found")
    } else {
        failure.response()
    }
}
fn info_value(info: &BlockInfo) -> Result<Value, NetworkFailure> {
    // The legacy ProtocolVersion enum ends at P10.
    let protocol = info.protocol_version.0;
    if !(1..=10).contains(&protocol) {
        return Err(NetworkFailure::InvalidResponse);
    }
    Ok(json!({
        "blockHash": info.block_hash, "blockParent": info.block_parent,
        "blockLastFinalized": info.block_last_finalized, "blockHeight": info.block_height,
        "genesisIndex": info.genesis_index, "eraBlockHeight": info.era_block_height,
        "blockReceiveTime": format_time(info.block_receive_time), "blockArriveTime": format_time(info.block_arrive_time),
        "blockSlot": info.block_slot, "blockSlotTime": format_time(info.block_slot_time), "blockBaker": info.block_baker,
        "finalized": info.finalized, "transactionCount": info.transaction_count as i64,
        "transactionEnergyCost": info.transaction_energy_cost, "transactionsSize": info.transactions_size as i64,
        "blockStateHash": info.block_state_hash, "protocolVersion": protocol,
        "round": info.round, "epoch": info.epoch
    }))
}
