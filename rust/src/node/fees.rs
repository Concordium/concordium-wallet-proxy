//! This module reads fee-estimation context through the SDK.
//! It queries consensus before chain parameters and does not resolve the foundation account.

use super::SdkNode;
use crate::{
    fees::FeeNode,
    network::{NetworkFailure, NetworkNode},
};
use concordium_rust_sdk::{
    types::{ProtocolVersion, chain_parameters::EnergyRate},
    v2::BlockIdentifier,
};

impl FeeNode for SdkNode {
    async fn fee_context(&self) -> Result<(EnergyRate, ProtocolVersion), NetworkFailure> {
        let consensus = self.consensus_info().await?;
        let parameters = self
            .query(|mut client| async move {
                client
                    .get_block_chain_parameters(BlockIdentifier::Best)
                    .await
                    .map(|response| response.response)
            })
            .await?;
        let rate = parameters
            .energy_rate()
            .map_err(|_| NetworkFailure::InvalidResponse)?;
        let protocol = consensus
            .protocol_version
            .try_into()
            .map_err(|_| NetworkFailure::InvalidResponse)?;
        Ok((rate, protocol))
    }
}
