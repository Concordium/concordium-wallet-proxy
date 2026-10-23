//! This module sends signed block items through the supported SDK submission method.
//! It reuses transport reconnection and does not retry node rejection or timeout.

use super::SdkNode;
use crate::{
    network::NetworkFailure,
    submission::{Submission, SubmissionNode},
};
use concordium_rust_sdk::endpoints::QueryError;

impl SubmissionNode for SdkNode {
    async fn submit(&self, item: &Submission) -> Result<(), NetworkFailure> {
        self.query(|mut client| async move {
            client
                .send_block_item(item)
                .await
                .map(|_| ())
                .map_err(QueryError::from)
        })
        .await
    }
}
