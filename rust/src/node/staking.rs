//! This module supplies staking and chain-condition queries through the SDK.
//! Chain-parameter queries also resolve the legacy foundation account index.

use super::SdkNode;
use crate::{
    network::NetworkFailure,
    staking::{Conditions, StakingNode},
};
use concordium_rust_sdk::{
    types::{
        BakerId, BakerPoolStatus, PassiveDelegationStatus, RewardsOverview, hashes::BlockHash,
    },
    v2::{AccountIdentifier, BlockIdentifier},
};

impl StakingNode for SdkNode {
    async fn pool_status(
        &self,
        baker: BakerId,
    ) -> Result<(BlockHash, BakerPoolStatus), NetworkFailure> {
        self.query(move |mut client| async move {
            let response = client
                .get_pool_info(BlockIdentifier::LastFinal, baker)
                .await?;
            Ok((response.block_hash, response.response))
        })
        .await
    }

    async fn passive_status(&self) -> Result<PassiveDelegationStatus, NetworkFailure> {
        self.query(|mut client| async move {
            client
                .get_passive_delegation_info(BlockIdentifier::LastFinal)
                .await
                .map(|response| response.response)
        })
        .await
    }

    async fn conditions(&self, block: BlockIdentifier) -> Result<Conditions, NetworkFailure> {
        let parameters = self
            .query(move |mut client| async move {
                client
                    .get_block_chain_parameters(block)
                    .await
                    .map(|response| response.response)
            })
            .await?;
        crate::staking::validate_parameters(&parameters)?;
        let address = parameters
            .foundation_account
            .ok_or(NetworkFailure::InvalidResponse)?;
        // Haskell repeats the original selector, even when it is LastFinal.
        let account = self
            .query(move |mut client| async move {
                client
                    .get_account_info(&AccountIdentifier::Address(address), block)
                    .await
                    .map(|response| response.response)
            })
            .await?;
        Ok(Conditions {
            parameters,
            foundation_index: account.account_index.index,
        })
    }

    async fn rewards(&self, block: BlockIdentifier) -> Result<RewardsOverview, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_tokenomics_info(block)
                .await
                .map(|response| response.response)
        })
        .await
    }
}
