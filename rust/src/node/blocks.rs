//! This module reads blocks and complete transaction-outcome streams through the SDK.
//! The shared query policy covers the complete stream, not only its first response.

use super::SdkNode;
use crate::{blocks::BlockNode, network::NetworkFailure};
use concordium_rust_sdk::{
    endpoints::{BlocksAtHeightInput, QueryError, RPCError},
    types::{BlockItemSummary, hashes::BlockHash, queries::BlockInfo},
    v2::BlockIdentifier,
};
use futures_util::TryStreamExt;

impl BlockNode for SdkNode {
    async fn block_info(&self, hash: BlockHash) -> Result<BlockInfo, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_block_info(BlockIdentifier::Given(hash))
                .await
                .map(|response| response.response)
        })
        .await
    }
    async fn blocks_at_height(&self, height: u64) -> Result<Vec<BlockHash>, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_blocks_at_height(&BlocksAtHeightInput::Absolute {
                    height: height.into(),
                })
                .await
        })
        .await
    }
    async fn transaction_outcomes(
        &self,
        hash: BlockHash,
    ) -> Result<Vec<BlockItemSummary>, NetworkFailure> {
        self.query(move |mut client| async move {
            let response = client
                .get_block_transaction_events(BlockIdentifier::Given(hash))
                .await?;
            response
                .response
                .try_collect()
                .await
                .map_err(|status| QueryError::RPCError(RPCError::CallError(status)))
        })
        .await
    }
}
