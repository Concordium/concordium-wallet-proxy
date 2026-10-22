//! This module checks history access and reads public-key account bindings.
//! It reads existing records and does not change the database schema.

use crate::discovery::{Binding, DiscoveryHistory};
use concordium_rust_sdk::id::types::AccountAddress;
use sqlx::PgPool;
use std::future::Future;

/// This interface checks transaction-history access for the health service.
pub trait History: Send + Sync {
    /// Check table access and decode a record if data is present.
    /// Empty tables are not an error.
    fn check(&self) -> impl Future<Output = anyhow::Result<()>> + Send;
}

/// This type supplies history and account-binding queries through PostgreSQL.
pub struct PostgresHistory(
    /// This shared pool supplies database connections.
    pub PgPool,
);

impl DiscoveryHistory for PostgresHistory {
    async fn key_accounts(&self, key: [u8; 33], only_simple: bool) -> anyhow::Result<Vec<Binding>> {
        let rows = sqlx::query!(
            "SELECT address, credential_index, key_index, is_simple_account FROM account_public_key_bindings WHERE public_key = $1 AND (NOT $2::boolean OR is_simple_account = TRUE) ORDER BY credential_index ASC",
            &key[..],
            only_simple,
        ).fetch_all(&self.0).await?;
        rows.into_iter()
            .map(|row| {
                let address = row
                    .address
                    .get(..32)
                    .ok_or_else(|| anyhow::anyhow!("Invalid stored account address"))?;
                Ok(Binding {
                    address: AccountAddress(address.try_into()?),
                    // Persistent converts SqlInt32 values with Word8's wrapping conversion.
                    credential_index: row.credential_index as u8,
                    key_index: row.key_index as u8,
                    is_simple_account: row.is_simple_account,
                })
            })
            .collect()
    }
}

impl History for PostgresHistory {
    async fn check(&self) -> anyhow::Result<()> {
        // Preserve the Haskell health probe: decode at most one row from the
        // Cartesian product, NOT a join and NOT a requirement for nonempty data.
        let row = sqlx::query!(
            "SELECT a.id AS entry_id, a.account, a.summary AS entry_summary, s.id AS summary_id, s.block, s.timestamp, s.height, s.summary AS summary_json FROM ati AS a CROSS JOIN summaries AS s LIMIT 1"
        ).fetch_optional(&self.0).await?;
        if let Some(row) = row {
            anyhow::ensure!(row.account.len() == 32, "Invalid stored account address");
            anyhow::ensure!(row.block.len() == 32, "Invalid stored block hash");
            anyhow::ensure!(
                row.timestamp >= 0 && row.height >= 0,
                "Invalid unsigned chain value"
            );
        }
        Ok(())
    }
}
