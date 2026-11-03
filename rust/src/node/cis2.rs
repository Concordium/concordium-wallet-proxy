//! This module pins read-only CIS-2 invocations to the finalized instance-info response.
//! It uses the shared SDK timeout and reconnection policy for every operation.

use super::SdkNode;
use crate::{
    cis2::{Cis2Node, ContractSnapshot},
    network::NetworkFailure,
};
use concordium_rust_sdk::{
    types::{
        ContractAddress,
        hashes::BlockHash,
        smart_contracts::{ContractContext, InvokeContractResult},
    },
    v2::BlockIdentifier,
};

impl Cis2Node for SdkNode {
    async fn contract_snapshot(
        &self,
        address: ContractAddress,
    ) -> Result<ContractSnapshot, NetworkFailure> {
        let response = self
            .query(move |mut client| async move {
                client
                    .get_instance_info(address, BlockIdentifier::LastFinal)
                    .await
            })
            .await?;
        Ok(ContractSnapshot {
            block: response.block_hash,
            name: response.response.name().clone(),
        })
    }
    async fn cis2_invoke(
        &self,
        block: BlockHash,
        context: &ContractContext,
    ) -> Result<InvokeContractResult, NetworkFailure> {
        self.query(|mut client| async move {
            client
                .invoke_instance(BlockIdentifier::Given(block), context)
                .await
                .map(|response| response.response)
        })
        .await
    }
}
