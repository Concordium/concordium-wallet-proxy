//! This module checks access to the existing transaction-history tables.
//! It reads existing records and does not change the database schema.

use async_trait::async_trait;
use sqlx::PgPool;

/// This interface checks transaction-history access for the health service.
#[async_trait]
pub trait History: Send + Sync {
    /// Check table access and decode a record if data is present.
    /// Empty tables are not an error.
    async fn check(&self) -> anyhow::Result<()>;
}

/// This type checks the transaction-history tables through PostgreSQL.
pub struct PostgresHistory(
    /// This shared pool supplies database connections.
    pub PgPool,
);

#[async_trait]
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
