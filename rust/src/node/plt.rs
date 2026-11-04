//! This module reads the protocol-level token list through the SDK.
//! Stream failures use the shared query error policy.

use super::SdkNode;
use crate::{network::NetworkFailure, plt::TokenNode};
use concordium_rust_sdk::{
    endpoints::{QueryError, RPCError},
    protocol_level_tokens::{TokenId, TokenInfo},
    v2::BlockIdentifier,
};
use futures_util::TryStreamExt;

impl TokenNode for SdkNode {
    async fn token_info(
        &self,
        id: TokenId,
        block: BlockIdentifier,
    ) -> Result<TokenInfo, NetworkFailure> {
        crate::balances::BalanceNode::token_info(self, id, block).await
    }
    async fn token_list(&self) -> Result<Vec<TokenId>, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_token_list(BlockIdentifier::LastFinal)
                .await?
                .response
                .try_collect()
                .await
                .map_err(|status| QueryError::RPCError(RPCError::CallError(status)))
        })
        .await
    }
}
