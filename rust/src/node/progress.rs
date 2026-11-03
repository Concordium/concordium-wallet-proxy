//! This module reads transaction lifecycle status through the SDK.
//! It reuses the existing query timeout and transport-reconnection policy.

use super::SdkNode;
use crate::{network::NetworkFailure, progress::ProgressNode};
use concordium_rust_sdk::types::{TransactionStatus, hashes::TransactionHash};

impl ProgressNode for SdkNode {
    async fn submission_status(
        &self,
        hash: TransactionHash,
    ) -> Result<TransactionStatus, NetworkFailure> {
        self.query(move |mut client| async move { client.get_block_item_status(&hash).await })
            .await
    }
}
