//! This module stores the legacy faucet's recipient rows in PostgreSQL.
//! It does not manage transaction-history tables or alter existing recipient rows.

use super::{DropStore, Record};
use concordium_rust_sdk::id::types::AccountAddress;
use sqlx::PgPool;

/// This adapter supplies recipient-only storage through a shared pool.
pub struct PostgresDrops(
    /// This pool accesses the existing database.
    pub PgPool,
);
impl PostgresDrops {
    /// Create only the proxy-owned recipient table when the faucet is enabled.
    pub async fn initialize(&self) -> anyhow::Result<()> {
        sqlx::raw_sql(include_str!("recipient.sql"))
            .execute(&self.0)
            .await?;
        Ok(())
    }
}
impl DropStore for PostgresDrops {
    async fn lookup(&self, address: AccountAddress) -> anyhow::Result<Option<Record>> {
        Ok(sqlx::query!(
            "SELECT id, transaction FROM g_t_u_recipient WHERE account = $1",
            &address.0[..]
        )
        .fetch_optional(&self.0)
        .await?
        .map(|row| Record {
            id: row.id,
            transaction: row.transaction,
        }))
    }
    async fn insert(&self, address: AccountAddress, transaction: &[u8]) -> anyhow::Result<bool> {
        Ok(sqlx::query!("INSERT INTO g_t_u_recipient (account, transaction) VALUES ($1, $2) ON CONFLICT (account) DO NOTHING RETURNING id", &address.0[..], transaction)
            .fetch_optional(&self.0).await?.is_some())
    }
    async fn delete(&self, id: i64) -> anyhow::Result<()> {
        sqlx::query!("DELETE FROM g_t_u_recipient WHERE id = $1", id)
            .execute(&self.0)
            .await?;
        Ok(())
    }
}
