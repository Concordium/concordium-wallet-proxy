//! This module reads the externally owned CIS-2 token catalogue with checked SQL.
//! Numeric supply text is decoded without a machine-integer or decimal-precision limit.

use super::PostgresHistory;
use crate::cis2::{Cis2History, IndexedToken};
use concordium_rust_sdk::types::ContractAddress;

impl Cis2History for PostgresHistory {
    async fn contract_tokens(
        &self,
        address: ContractAddress,
        from: Option<i64>,
        limit: u16,
    ) -> anyhow::Result<Vec<IndexedToken>> {
        let rows = sqlx::query!(
            "SELECT id, token_id, total_supply::text AS \"supply!\" FROM cis2_tokens WHERE index = $1 AND subindex = $2 AND ($3::bigint IS NULL OR id > $3) ORDER BY id ASC LIMIT $4",
            address.index as i64, address.subindex as i64, from, i64::from(limit.min(1000)),
        ).fetch_all(&self.0).await?;
        rows.into_iter()
            .map(|row| {
                Ok(IndexedToken {
                    id: row.id,
                    token: row.token_id,
                    supply: row.supply.parse()?,
                })
            })
            .collect()
    }
}
