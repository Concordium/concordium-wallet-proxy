//! This module supplies faucet operations through the existing SDK adapters.
//! It checks finalized outcomes without using wallet-facing status serialization.

use super::SdkNode;
use crate::{faucet::DropNode, network::NetworkFailure, submission::Submission};
use concordium_rust_sdk::{
    id::types::AccountAddress,
    smart_contracts::common::Amount,
    types::{Nonce, TransactionStatus, hashes::TransactionHash},
    v2::BlockIdentifier,
};

impl DropNode for SdkNode {
    async fn account(&self, address: AccountAddress) -> Result<(Nonce, Amount), NetworkFailure> {
        let account =
            crate::balances::BalanceNode::account_info(self, address, BlockIdentifier::LastFinal)
                .await?;
        Ok((account.account_nonce, account.account_amount))
    }
    async fn nonce(&self, address: AccountAddress) -> Result<Nonce, NetworkFailure> {
        Ok(crate::accounts::AccountNode::next_nonce(self, address)
            .await?
            .nonce)
    }
    async fn acceptable(&self, hash: TransactionHash) -> Result<bool, NetworkFailure> {
        match crate::progress::ProgressNode::submission_status(self, hash).await {
            Err(failure) if failure.is_not_found() => Ok(false),
            Err(failure) => Err(failure),
            Ok(TransactionStatus::Received | TransactionStatus::Committed(_)) => Ok(true),
            Ok(TransactionStatus::Finalized(outcomes)) => {
                if outcomes.len() != 1 {
                    return Err(NetworkFailure::InvalidResponse);
                }
                outcomes
                    .values()
                    .next()
                    .unwrap()
                    .is_success()
                    .known()
                    .ok_or(NetworkFailure::InvalidResponse)
            }
        }
    }
    async fn submit(&self, item: &Submission) -> Result<(), NetworkFailure> {
        crate::submission::SubmissionNode::submit(self, item).await
    }
}
