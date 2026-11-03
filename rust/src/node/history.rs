//! This module resolves historical account aliases through the SDK.
//! It reads only the last finalized account address and leaves NOT_FOUND handling to the route.

use super::SdkNode;
use crate::{history::HistoryNode, network::NetworkFailure};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    v2::{AccountIdentifier, BlockIdentifier},
};

impl HistoryNode for SdkNode {
    async fn canonical_address(
        &self,
        address: AccountAddress,
    ) -> Result<AccountAddress, NetworkFailure> {
        self.query(move |mut client| async move {
            client
                .get_account_info(
                    &AccountIdentifier::Address(address),
                    BlockIdentifier::LastFinal,
                )
                .await
        })
        .await
        .map(|response| response.response.account_address)
    }
}
