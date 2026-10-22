//! This module supplies account and staking operations through the SDK.
//! Every query uses the shared timeout and transport-reconnection policy.

use super::SdkNode;
use crate::{balances::BalanceNode, network::NetworkFailure};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    protocol_level_tokens::{TokenId, TokenInfo},
    types::{AccountInfo, BakerId, BakerPoolStatus, RewardsOverview},
    v2::{AccountIdentifier, BlockIdentifier},
};

impl BalanceNode for SdkNode {
    async fn account_info(
        &self,
        address: AccountAddress,
        block: BlockIdentifier,
    ) -> Result<AccountInfo, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_account_info(&AccountIdentifier::Address(address), block)
                .await
                .map(|response| response.response)
        })
        .await
    }

    async fn pool_info(
        &self,
        baker: BakerId,
        block: BlockIdentifier,
    ) -> Result<BakerPoolStatus, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_pool_info(block, baker)
                .await
                .map(|response| response.response)
        })
        .await
    }

    async fn token_info(
        &self,
        id: TokenId,
        block: BlockIdentifier,
    ) -> Result<TokenInfo, NetworkFailure> {
        self.query(move |mut client| {
            let id = id.clone();
            async move {
                client
                    .get_token_info(id, block)
                    .await
                    .map(|response| response.response)
            }
        })
        .await
    }

    async fn next_payday(
        &self,
        block: BlockIdentifier,
    ) -> Result<Option<DateTime<Utc>>, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_tokenomics_info(block)
                .await
                .map(|response| match response.response {
                    RewardsOverview::V0 { .. } => None,
                    RewardsOverview::V1 {
                        next_payday_time, ..
                    } => Some(next_payday_time),
                })
        })
        .await
    }

    async fn reward_period_epochs(
        &self,
        block: BlockIdentifier,
    ) -> Result<Option<u64>, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_block_chain_parameters(block)
                .await
                .map(|response| {
                    response
                        .response
                        .reward_period_length
                        .map(|length| length.reward_period_epochs().epoch)
                })
        })
        .await
    }
}
