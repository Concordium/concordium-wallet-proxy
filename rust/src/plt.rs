//! This module serves protocol-level token discovery and information.
//! Each operation selects the last finalized block independently, as in the legacy service.

use crate::network::NetworkFailure;
use axum::{
    Json, Router,
    extract::{Path, State},
    response::{IntoResponse, Response},
    routing::get,
};
use concordium_rust_sdk::{
    protocol_level_tokens::{TokenId, TokenInfo},
    v2::BlockIdentifier,
};
use std::{future::Future, sync::Arc};

/// This interface supplies token discovery and global token information.
pub trait TokenNode: Send + Sync {
    /// Read global token information at the specified block selector.
    fn token_info(
        &self,
        id: TokenId,
        block: BlockIdentifier,
    ) -> impl Future<Output = Result<TokenInfo, NetworkFailure>> + Send;
    /// Read token identifiers in the node's stream order.
    fn token_list(&self) -> impl Future<Output = Result<Vec<TokenId>, NetworkFailure>> + Send;
}

/// Build the existing protocol-level token routes.
pub fn routes<N: TokenNode + 'static>(node: Arc<N>) -> Router {
    Router::new()
        .route("/v0/plt/tokens", get(list::<N>))
        .route("/v0/plt/tokenInfo/{id}", get(info::<N>))
        .with_state(node)
}

async fn list<N: TokenNode>(State(node): State<Arc<N>>) -> Response {
    let ids = match node.token_list().await {
        Ok(ids) => ids,
        Err(failure) => return failure.response(),
    };
    let mut values = Vec::with_capacity(ids.len());
    for id in ids {
        match node.token_info(id, BlockIdentifier::LastFinal).await {
            Ok(info) => values.push(crate::balances::tokens::information(&info)),
            Err(failure) => return failure.response(),
        }
    }
    Json(values).into_response()
}

async fn info<N: TokenNode>(State(node): State<Arc<N>>, Path(id): Path<String>) -> Response {
    let Ok(id) = id.parse() else {
        // The legacy parser throws ExitFailure. runGRPC catches it as a client error.
        return NetworkFailure::Transport.response();
    };
    match node.token_info(id, BlockIdentifier::LastFinal).await {
        Ok(info) => Json(crate::balances::tokens::information(&info)).into_response(),
        Err(failure) => failure.response(),
    }
}
