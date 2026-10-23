//! This module serves validator pools, passive delegation, and chain conditions.
//! It keeps pending-change queries pinned to the pool response block.

mod parameters;
mod view;

use crate::{
    compatibility::{first_payday, format_time},
    http::error,
    network::{NetworkFailure, NetworkNode},
};
use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::{
    types::{
        BakerId, BakerPoolStatus, PassiveDelegationStatus, RewardsOverview,
        chain_parameters::ChainParameters, hashes::BlockHash,
    },
    v2::BlockIdentifier,
};
use std::{future::Future, sync::Arc};

/// This type supplies chain parameters and the legacy foundation account index.
pub struct Conditions {
    /// These parameters exclude the foundation account index in the SDK.
    pub parameters: ChainParameters,
    /// This index comes from an account query at the requested block selector.
    pub foundation_index: u64,
}

/// This interface supplies staking queries through the node.
pub trait StakingNode: NetworkNode {
    /// Read the pool at LastFinal and retain the response block hash.
    fn pool_status(
        &self,
        baker: BakerId,
    ) -> impl Future<Output = Result<(BlockHash, BakerPoolStatus), NetworkFailure>> + Send;
    /// Read passive delegation at LastFinal.
    fn passive_status(
        &self,
    ) -> impl Future<Output = Result<PassiveDelegationStatus, NetworkFailure>> + Send;
    /// Read chain parameters and resolve the foundation account at the same selector.
    fn conditions(
        &self,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<Conditions, NetworkFailure>> + Send;
    /// Read reward status at the specified block.
    fn rewards(
        &self,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<RewardsOverview, NetworkFailure>> + Send;
}

/// Check required legacy parameters before the foundation account query.
pub(crate) fn validate_parameters(parameters: &ChainParameters) -> Result<(), NetworkFailure> {
    if parameters.keys.root_keys.is_none()
        || parameters.keys.level_1_keys.is_none()
        || parameters.keys.level_2_keys.is_none()
    {
        return Err(NetworkFailure::InvalidResponse);
    }
    parameters::value(parameters, 0).map(|_| ())
}

/// Build the existing staking and chain-condition routes.
pub fn routes<N: StakingNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/bakerPool/{baker}", get(pool::<N>))
        .route("/v0/passiveDelegation", get(passive::<N>))
        .route("/v0/chainParameters", get(chain::<N>))
        .route("/v0/nextPayday", get(payday::<N>))
        .with_state(node)
}

async fn pool<N: StakingNode>(State(node): State<Arc<N>>, Path(baker): Path<String>) -> Response {
    let Ok(baker) = baker.parse::<u64>() else {
        return not_found();
    };
    // Only the initial pool query replaces NOT_FOUND with the plain 404 error.
    let (hash, status) = match node.pool_status(BakerId { id: baker.into() }).await {
        Ok(response) => response,
        Err(failure) if failure.is_not_found() => return missing_pool(),
        Err(failure) => return failure.response(),
    };
    if status.active_baker_pool_status.is_none() {
        return missing_pool();
    }
    match pool_value(node.as_ref(), &status, hash).await {
        Ok(value) => Json(value).into_response(),
        Err(failure) => failure.response(),
    }
}

async fn pool_value<N: StakingNode>(
    node: &N,
    pool: &BakerPoolStatus,
    hash: BlockHash,
) -> Result<serde_json::Value, NetworkFailure> {
    let mut value = view::pool(pool)?;
    let effective = pool
        .active_baker_pool_status
        .as_ref()
        .and_then(|active| view::effective_time(&active.baker_stake_pending_change));
    if let Some(effective) = effective {
        let conditions = node.conditions(BlockIdentifier::Given(hash)).await?;
        if let Some(epochs) = conditions.parameters.reward_period_length {
            let reward = node.rewards(BlockIdentifier::Given(hash)).await?;
            let consensus = node.consensus_info().await?;
            let RewardsOverview::V1 {
                next_payday_time, ..
            } = reward
            else {
                return Err(NetworkFailure::InvalidResponse);
            };
            let estimated = first_payday(
                next_payday_time,
                effective,
                consensus.epoch_duration,
                epochs.reward_period_epochs().epoch,
            )?;
            value["bakerStakePendingChange"]["estimatedChangeTime"] =
                serde_json::json!(format_time(estimated));
        }
    }
    Ok(value)
}

async fn passive<N: StakingNode>(State(node): State<Arc<N>>) -> Response {
    match node.passive_status().await {
        Ok(status) => Json(view::passive(&status)).into_response(),
        Err(failure) => failure.response(),
    }
}

async fn chain<N: StakingNode>(State(node): State<Arc<N>>) -> Response {
    match node
        .conditions(BlockIdentifier::LastFinal)
        .await
        .and_then(|conditions| {
            parameters::value(&conditions.parameters, conditions.foundation_index)
        }) {
        Ok(value) => Json(value).into_response(),
        Err(failure) => failure.response(),
    }
}

async fn payday<N: StakingNode>(State(node): State<Arc<N>>) -> Response {
    match node.rewards(BlockIdentifier::LastFinal).await {
        Ok(RewardsOverview::V1 {
            next_payday_time, ..
        }) => Json(serde_json::json!({"nextPaydayTime": format_time(next_payday_time)}))
            .into_response(),
        Ok(_) | Err(NetworkFailure::InvalidResponse) => {
            error(StatusCode::BAD_REQUEST, 1, "Invalid arguments")
        }
        Err(failure) => failure.response(),
    }
}

fn not_found() -> Response {
    error(StatusCode::NOT_FOUND, 1, "Not found")
}

fn missing_pool() -> Response {
    error(StatusCode::NOT_FOUND, 2, "Not found")
}
