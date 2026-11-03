//! These tests check account-history selection against disposable PostgreSQL databases.
//! The synthetic legacy envelopes test SQL behavior, not accepted HTTP decoding.

mod boundaries;
mod http;
mod legacy;
mod rendering;
mod rpc;
mod token_events;
mod token_postgres;
mod token_rejections;

use concordium_rust_sdk::id::types::AccountAddress;
use concordium_wallet_proxy::{
    database::PostgresHistory,
    history::{AccountHistory, HistoryOrder, HistoryQuery, HistoryVersion, RewardFilter},
};
use serde_json::{Value, json};
use sqlx::PgPool;

async fn schema(pool: &PgPool) {
    sqlx::raw_sql(include_str!("fixtures/schema.sql"))
        .execute(pool)
        .await
        .unwrap();
}

async fn insert(pool: &PgPool, id: i64, account: AccountAddress, summary: Value) {
    // Summary identifiers deliberately run opposite to entry identifiers.
    sqlx::query(
        "INSERT INTO summaries(id, block, timestamp, height, summary) VALUES ($1, $2, $3, $4, $5)",
    )
    .bind(10_000 - id)
    .bind([id as u8; 32].as_slice())
    .bind(id * 1000)
    .bind(id)
    .bind(summary)
    .execute(pool)
    .await
    .unwrap();
    sqlx::query("INSERT INTO ati(id, account, summary) VALUES ($1, $2, $3)")
        .bind(id)
        .bind(account.0.as_slice())
        .bind(10_000 - id)
        .execute(pool)
        .await
        .unwrap();
}

async fn ids(history: &PostgresHistory, query: &HistoryQuery) -> Vec<i64> {
    history
        .account_history(AccountAddress([1; 32]), query)
        .await
        .unwrap()
        .into_iter()
        .map(|entry| entry.id)
        .collect()
}

fn transaction(kind: &str) -> Value {
    json!({"Left": {"type": {"type": "accountTransaction", "contents": kind}}})
}

fn reward(tag: &str) -> Value {
    json!({"Right": {"tag": tag}})
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn history_orders_entry_ids_and_applies_exclusive_cursors_and_inclusive_bounds(pool: PgPool) {
    schema(&pool).await;
    let history = PostgresHistory(pool.clone());
    let mut query = HistoryQuery::default();
    assert!(ids(&history, &query).await.is_empty());
    for id in 1..=5 {
        insert(&pool, id, AccountAddress([1; 32]), transaction("transfer")).await;
    }
    insert(&pool, 6, AccountAddress([2; 32]), transaction("transfer")).await;
    // An alias must be resolved by the caller; SQL matches the canonical bytes only.
    let mut alias = [1; 32];
    alias[31] = 2;
    insert(&pool, 7, AccountAddress(alias), transaction("transfer")).await;
    assert_eq!(ids(&history, &query).await, [1, 2, 3, 4, 5]);
    query.limit = 2;
    query.from = Some(2);
    assert_eq!(ids(&history, &query).await, [3, 4]);
    query.order = HistoryOrder::Descending;
    query.from = Some(4);
    assert_eq!(ids(&history, &query).await, [3, 2]);
    query.from = None;
    assert_eq!(ids(&history, &query).await, [5, 4]);
    query.limit = 1000;
    query.time_from = Some(2000);
    query.time_to = Some(4000);
    query.height_from = Some(3);
    query.height_to = Some(4);
    assert_eq!(ids(&history, &query).await, [4, 3]);
    query.order = HistoryOrder::Ascending;
    assert_eq!(ids(&history, &query).await, [3, 4]);
    query.time_from = Some(4001);
    assert!(ids(&history, &query).await.is_empty());
    query.time_from = None;
    query.time_to = None;
    query.height_from = Some(5);
    assert!(ids(&history, &query).await.is_empty());
    query.height_from = None;
    query.height_to = None;
    query.from = Some(99);
    assert!(ids(&history, &query).await.is_empty());
    query.order = HistoryOrder::Descending;
    query.from = Some(1);
    assert!(ids(&history, &query).await.is_empty());
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn history_preserves_version_reward_and_encrypted_filter_interactions(pool: PgPool) {
    schema(&pool).await;
    for (index, summary) in [
        transaction("transfer"),
        reward("BlockReward"),
        reward("FinalizationRewards"),
        reward("BakingRewards"),
        reward("PaydayAccountReward"),
        reward("ValidatorSuspended"),
        reward("ValidatorPrimedForSuspension"),
        transaction("encryptedAmountTransfer"),
        transaction("encryptedAmountTransferWithMemo"),
        transaction("transferToEncrypted"),
        transaction("transferToPublic"),
        transaction("tokenUpdate"),
        transaction("metaUpdate"),
        json!({"Left": {"type": {"type": "updateTransaction", "contents": "updateCreatePLT"}}}),
        transaction("transferWithMemo"),
    ]
    .into_iter()
    .enumerate()
    {
        insert(&pool, index as i64 + 1, AccountAddress([1; 32]), summary).await;
    }
    let history = PostgresHistory(pool);
    let transactions = [1, 8, 9, 10, 11, 15];
    for version in [
        HistoryVersion::V0,
        HistoryVersion::V1,
        HistoryVersion::V2,
        HistoryVersion::V3,
    ] {
        let mut query = HistoryQuery {
            version,
            ..Default::default()
        };
        let mut all = vec![1, 2, 3, 4, 5, 8, 9, 10, 11, 15];
        if matches!(version, HistoryVersion::V2 | HistoryVersion::V3) {
            all.extend([6, 7]);
        }
        if version == HistoryVersion::V3 {
            all.extend([12, 13, 14]);
        }
        all.sort();
        assert_eq!(ids(&history, &query).await, all);
        query.rewards = RewardFilter::AllButFinalization;
        assert_eq!(
            ids(&history, &query).await,
            all.iter()
                .copied()
                .filter(|id| *id != 3)
                .collect::<Vec<_>>()
        );
        query.rewards = RewardFilter::None;
        let mut none = transactions.to_vec();
        if matches!(version, HistoryVersion::V2 | HistoryVersion::V3) {
            none.extend([6, 7]);
        }
        if version == HistoryVersion::V3 {
            none.extend([12, 13, 14]);
        }
        none.sort();
        assert_eq!(ids(&history, &query).await, none);
        for rewards in [
            RewardFilter::All,
            RewardFilter::AllButFinalization,
            RewardFilter::None,
        ] {
            query.rewards = rewards;
            query.only_encrypted = true;
            query.exclude_block_rewards = true;
            query.exclude_finalization_rewards = true;
            query.exclude_baking_rewards = true;
            assert_eq!(ids(&history, &query).await, [8, 9, 10, 11]);
            query.order = HistoryOrder::Descending;
            assert_eq!(ids(&history, &query).await, [11, 10, 9, 8]);
            query.order = HistoryOrder::Ascending;
        }
        query.rewards = RewardFilter::All;
        query.only_encrypted = false;
        assert_eq!(
            ids(&history, &query).await,
            all.iter()
                .copied()
                .filter(|id| ![2, 3, 4].contains(id))
                .collect::<Vec<_>>()
        );
        query.exclude_block_rewards = false;
        query.exclude_baking_rewards = false;
        assert_eq!(
            ids(&history, &query).await,
            all.iter()
                .copied()
                .filter(|id| *id != 3)
                .collect::<Vec<_>>()
        );
        query.exclude_finalization_rewards = false;
        query.exclude_block_rewards = true;
        assert_eq!(
            ids(&history, &query).await,
            all.iter()
                .copied()
                .filter(|id| *id != 2)
                .collect::<Vec<_>>()
        );
        query.exclude_block_rewards = false;
        query.exclude_baking_rewards = true;
        assert_eq!(
            ids(&history, &query).await,
            all.iter()
                .copied()
                .filter(|id| *id != 4)
                .collect::<Vec<_>>()
        );
    }
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn history_preserves_json_null_selection_and_signed_storage_bits(pool: PgPool) {
    schema(&pool).await;
    for (index, summary) in [
        json!({}),
        json!({"Left": null}),
        json!({"Right": {"tag": null}}),
        reward("UnknownReward"),
    ]
    .into_iter()
    .enumerate()
    {
        insert(&pool, index as i64 + 1, AccountAddress([1; 32]), summary).await;
    }
    let history = PostgresHistory(pool.clone());
    let mut query = HistoryQuery::default();
    assert_eq!(ids(&history, &query).await, [1, 2, 3, 4]);
    query.rewards = RewardFilter::None;
    assert_eq!(ids(&history, &query).await, [2]);
    query.rewards = RewardFilter::AllButFinalization;
    assert_eq!(ids(&history, &query).await, [2, 4]);
    query.rewards = RewardFilter::All;
    query.exclude_block_rewards = true;
    assert_eq!(ids(&history, &query).await, [2, 4]);
    query.exclude_block_rewards = false;
    query.only_encrypted = true;
    assert!(ids(&history, &query).await.is_empty());
    sqlx::raw_sql("UPDATE summaries SET timestamp = -1, height = -2 WHERE id = 9998")
        .execute(&pool)
        .await
        .unwrap();
    query.only_encrypted = false;
    query.time_from = Some(u64::MAX);
    query.time_to = Some(u64::MAX);
    query.height_from = Some(u64::MAX - 1);
    query.height_to = Some(u64::MAX - 1);
    let entries = history
        .account_history(AccountAddress([1; 32]), &query)
        .await
        .unwrap();
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].id, 2);
    assert_eq!(entries[0].timestamp, u64::MAX);
    assert_eq!(entries[0].height, u64::MAX - 1);
    assert_eq!(entries[0].summary, json!({"Left": null}));
    assert_eq!(entries[0].block.as_ref(), [2; 32]);
    sqlx::raw_sql("UPDATE summaries SET block = block || decode('ffff', 'hex') WHERE id = 9998")
        .execute(&pool)
        .await
        .unwrap();
    let entries = history
        .account_history(AccountAddress([1; 32]), &query)
        .await
        .unwrap();
    assert_eq!(entries[0].block.as_ref(), [2; 32]);
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn history_caps_limits_keeps_duplicate_entries_and_fails_without_partial_rows(pool: PgPool) {
    schema(&pool).await;
    insert(&pool, 1, AccountAddress([1; 32]), transaction("transfer")).await;
    // Multiple entries can refer to one summary; never deduplicate them.
    sqlx::raw_sql("INSERT INTO ati(id, account, summary) SELECT n, decode(repeat('01',32),'hex'), 9999 FROM generate_series(2,1005) AS n")
        .execute(&pool).await.unwrap();
    let history = PostgresHistory(pool.clone());
    let mut query = HistoryQuery::default();
    assert_eq!(ids(&history, &query).await.len(), 20);
    query.limit = u16::MAX;
    assert_eq!(ids(&history, &query).await.len(), 1000);
    query.order = HistoryOrder::Descending;
    let page = ids(&history, &query).await;
    assert_eq!(page.len(), 1000);
    assert_eq!(page[0], 1005);
    assert_eq!(page[999], 6);
    query.limit = 0;
    assert!(ids(&history, &query).await.is_empty());
    query.limit = 20;
    query.order = HistoryOrder::Ascending;
    query.from = Some(1000);
    insert(
        &pool,
        1006,
        AccountAddress([1; 32]),
        transaction("transfer"),
    )
    .await;
    sqlx::raw_sql("UPDATE summaries SET block = decode('01', 'hex') WHERE id = 8994")
        .execute(&pool)
        .await
        .unwrap();
    assert!(
        history
            .account_history(AccountAddress([1; 32]), &query)
            .await
            .is_err()
    );
    sqlx::raw_sql("DROP TABLE ati")
        .execute(&pool)
        .await
        .unwrap();
    assert!(
        history
            .account_history(AccountAddress([1; 32]), &query)
            .await
            .is_err()
    );
}
