//! This module selects legacy account-history rows with checked SQL.
//! Separate static statements keep the entry cursor and order index-compatible.

use super::PostgresHistory;
use crate::history::{
    AccountHistory, HistoryEntry, HistoryOrder, HistoryQuery, HistoryVersion, RewardFilter,
};
use concordium_rust_sdk::{id::types::AccountAddress, types::hashes::BlockHash};
use serde_json::Value;

/// This record holds the PostgreSQL representation before binary decoding.
struct StoredEntry {
    /// This identifier belongs to the account entry.
    id: i64,
    /// These bytes hold the serialized block hash.
    block: Vec<u8>,
    /// This signed value holds the persisted Word64 timestamp bits.
    timestamp: i64,
    /// This signed value holds the persisted Word64 height bits.
    height: i64,
    /// This value holds the unchanged legacy summary envelope.
    summary: Value,
}

impl TryFrom<StoredEntry> for HistoryEntry {
    type Error = anyhow::Error;

    fn try_from(row: StoredEntry) -> Result<Self, Self::Error> {
        // Cereal reads the first hash and permits trailing serialized bytes.
        let block: [u8; 32] = row
            .block
            .get(..32)
            .ok_or_else(|| {
                anyhow::anyhow!("Invalid stored block hash in history entry {}", row.id)
            })?
            .try_into()?;
        Ok(Self {
            id: row.id,
            block: BlockHash::from(block),
            // Persistent stores Word64 through the signed PostgreSQL BIGINT type.
            timestamp: row.timestamp as u64,
            height: row.height as u64,
            summary: row.summary,
        })
    }
}

impl AccountHistory for PostgresHistory {
    async fn account_history(
        &self,
        account: AccountAddress,
        query: &HistoryQuery,
    ) -> anyhow::Result<Vec<HistoryEntry>> {
        let rewards = match query.rewards {
            RewardFilter::All => 0_i32,
            RewardFilter::AllButFinalization => 1,
            RewardFilter::None => 2,
        };
        let suspension = query.version == HistoryVersion::V2;
        // Only the order and exclusive cursor operator differ. Each expansion is
        // checked by SQLx; all filter values remain bound statement parameters.
        macro_rules! fetch {
            ($path:literal) => {
                sqlx::query_file_as!(
                    StoredEntry,
                    $path,
                    &account.0[..],
                    query.from,
                    i64::from(query.limit.min(1000)),
                    query.time_from.map(|value| value as i64),
                    query.time_to.map(|value| value as i64),
                    query.height_from.map(|value| value as i64),
                    query.height_to.map(|value| value as i64),
                    query.exclude_block_rewards,
                    query.exclude_finalization_rewards,
                    query.exclude_baking_rewards,
                    query.only_encrypted,
                    rewards,
                    suspension,
                )
                .fetch_all(&self.0)
                .await?
            };
        }
        let rows = match query.order {
            HistoryOrder::Ascending => fetch!("rust/src/database/history_ascending.sql"),
            HistoryOrder::Descending => fetch!("rust/src/database/history_descending.sql"),
        };
        rows.into_iter().map(HistoryEntry::try_from).collect()
    }
}
