//! This module supplies nonce and encryption-key queries through the SDK.
//! Queries use the shared timeout and reconnection policy.

use super::SdkNode;
use crate::{accounts::AccountNode, network::NetworkFailure};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    types::queries::AccountNonceResponse,
    v2::{AccountIdentifier, BlockIdentifier},
};

impl AccountNode for SdkNode {
    async fn next_nonce(
        &self,
        address: AccountAddress,
    ) -> Result<AccountNonceResponse, NetworkFailure> {
        self.query(move |mut client| async move {
            client.get_next_account_sequence_number(&address).await
        })
        .await
    }

    async fn encryption_key(&self, address: AccountAddress) -> Result<String, NetworkFailure> {
        self.query(move |mut client| async move {
            let response = client
                .get_account_info(&AccountIdentifier::Address(address), BlockIdentifier::Best)
                .await?;
            Ok(concordium_rust_sdk::common::base16_encode_string(
                &response.response.account_encryption_key,
            ))
        })
        .await
    }
}
