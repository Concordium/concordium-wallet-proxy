//! This module defines reads from the existing account-history tables.
//! These types keep database selection separate from legacy summary decoding.

use concordium_rust_sdk::{id::types::AccountAddress, types::hashes::BlockHash};
use serde_json::Value;
use std::future::Future;

/// This value selects the pre-token account-history response version.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HistoryVersion {
    /// This version omits memos and validator suspension events.
    V0,
    /// This version includes memos but omits validator suspension events.
    V1,
    /// This version includes memos and validator suspension events.
    V2,
}

/// This value selects the entry order and the exclusive cursor direction.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HistoryOrder {
    /// Read entries with increasing identifiers.
    Ascending,
    /// Read entries with decreasing identifiers.
    Descending,
}

/// This value selects the legacy reward filter.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RewardFilter {
    /// Keep all reward types.
    All,
    /// Omit finalization rewards but keep other reward types.
    AllButFinalization,
    /// Keep account transactions and version-permitted suspension events only.
    None,
}

/// These parameters select rows before legacy summary decoding.
#[derive(Clone, Debug)]
pub struct HistoryQuery {
    /// This value selects the historical inclusion rules.
    pub version: HistoryVersion,
    /// This value selects ordering and the cursor comparison.
    pub order: HistoryOrder,
    /// This identifier is an exclusive entry cursor.
    pub from: Option<i64>,
    /// This limit is capped at 1000 rows by the database adapter.
    pub limit: u16,
    /// This bound includes records at or after this millisecond timestamp.
    pub time_from: Option<u64>,
    /// This bound includes records at or before this millisecond timestamp.
    pub time_to: Option<u64>,
    /// This bound includes records at or above this absolute block height.
    pub height_from: Option<u64>,
    /// This bound includes records at or below this absolute block height.
    pub height_to: Option<u64>,
    /// This value selects the combined reward filter.
    pub rewards: RewardFilter,
    /// This flag omits block rewards.
    pub exclude_block_rewards: bool,
    /// This flag omits finalization rewards.
    pub exclude_finalization_rewards: bool,
    /// This flag omits baking rewards.
    pub exclude_baking_rewards: bool,
    /// This flag selects the four legacy encrypted transaction types only.
    pub only_encrypted: bool,
}

impl Default for HistoryQuery {
    fn default() -> Self {
        Self {
            version: HistoryVersion::V0,
            order: HistoryOrder::Ascending,
            from: None,
            limit: 20,
            time_from: None,
            time_to: None,
            height_from: None,
            height_to: None,
            rewards: RewardFilter::All,
            exclude_block_rewards: false,
            exclude_finalization_rewards: false,
            exclude_baking_rewards: false,
            only_encrypted: false,
        }
    }
}

/// This record holds one entry and its persisted legacy summary.
#[derive(Debug)]
pub struct HistoryEntry {
    /// This identifier belongs to the account entry, not the summary.
    pub id: i64,
    /// This hash identifies the block that contains the summary.
    pub block: BlockHash,
    /// This timestamp is in milliseconds since the Unix epoch.
    pub timestamp: u64,
    /// This height is absolute across genesis blocks.
    pub height: u64,
    /// This value retains the legacy Left or Right summary envelope.
    pub summary: Value,
}

/// This interface reads account history without changing external tables.
pub trait AccountHistory: Send + Sync {
    /// Read selected rows for an already resolved canonical account address.
    fn account_history(
        &self,
        account: AccountAddress,
        query: &HistoryQuery,
    ) -> impl Future<Output = anyhow::Result<Vec<HistoryEntry>>> + Send;
}
