//! This module checks recipient writes and uniqueness against disposable PostgreSQL.
//! Existing signed bytes are reused without schema or data conversion.

use super::*;
use concordium_wallet_proxy::faucet::database::PostgresDrops;
use sqlx::PgPool;

/// This adapter checks committed storage before each controlled submission.
struct StoredNode {
    /// This adapter records node operations.
    node: Arc<Node>,
    /// This pool permits independent visibility checks.
    pool: PgPool,
    /// This optional barrier forces two requests to compete for one recipient.
    barrier: Option<tokio::sync::Barrier>,
}
impl DropNode for StoredNode {
    async fn account(&self, address: AccountAddress) -> Result<(Nonce, Amount), NetworkFailure> {
        self.node.account(address).await
    }
    async fn nonce(&self, address: AccountAddress) -> Result<Nonce, NetworkFailure> {
        let nonce = self.node.nonce(address).await?;
        if let Some(barrier) = &self.barrier {
            barrier.wait().await;
        }
        Ok(nonce)
    }
    async fn acceptable(&self, hash: TransactionHash) -> Result<bool, NetworkFailure> {
        self.node.acceptable(hash).await
    }
    async fn submit(&self, item: &Submission) -> Result<(), NetworkFailure> {
        let bytes = sqlx::query_scalar::<_, Vec<u8>>(
            "SELECT transaction FROM g_t_u_recipient WHERE account = $1",
        )
        .bind(&RECIPIENT.0[..])
        .fetch_one(&self.pool)
        .await
        .unwrap();
        let Submission::AccountTransaction(transaction) = item else {
            panic!("Expected account transaction");
        };
        assert_eq!(
            bytes,
            encode(transaction),
            "Submission must use the committed winning bytes"
        );
        self.node.submit(item).await
    }
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn recipient_schema_unique_insert_and_row_specific_delete(pool: PgPool) {
    sqlx::raw_sql("CREATE TABLE external_marker (value text); INSERT INTO external_marker VALUES ('unchanged')").execute(&pool).await.unwrap();
    let store = PostgresDrops(pool.clone());
    store.initialize().await.unwrap();
    store.initialize().await.unwrap();
    let bytes = include_bytes!("../fixtures/faucet/normal.bin");
    let (first, second) = tokio::join!(
        store.insert(RECIPIENT, bytes),
        store.insert(RECIPIENT, bytes)
    );
    assert_ne!(first.unwrap(), second.unwrap());
    let row = store.lookup(RECIPIENT).await.unwrap().unwrap();
    assert_eq!(row.transaction, bytes.as_slice());
    store.delete(row.id + 1).await.unwrap();
    assert!(store.lookup(RECIPIENT).await.unwrap().is_some());
    store.delete(row.id).await.unwrap();
    assert!(store.lookup(RECIPIENT).await.unwrap().is_none());
    assert_eq!(
        sqlx::query_scalar::<_, String>("SELECT value FROM external_marker")
            .fetch_one(&pool)
            .await
            .unwrap(),
        "unchanged"
    );
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn concurrent_http_requests_submit_only_the_committed_winning_transaction(pool: PgPool) {
    let store = PostgresDrops(pool.clone());
    store.initialize().await.unwrap();
    let node = Arc::new(Node::default());
    let app = Arc::new(Faucet {
        settings: Some(settings()),
        node: Arc::new(StoredNode {
            node: node.clone(),
            pool: pool.clone(),
            barrier: Some(tokio::sync::Barrier::new(2)),
        }),
        store: Arc::new(store),
        time: Time {
            now: 1000,
            count: 4,
        },
    });
    let (first, second) = tokio::time::timeout(std::time::Duration::from_secs(10), async {
        tokio::join!(
            request(app.clone(), ""),
            request(app, "{\"type\":\"scheduled\"}")
        )
    })
    .await
    .unwrap();
    assert_eq!(first, second);
    assert_eq!(first.0, 200);
    assert_eq!(node.submissions.lock().unwrap().len(), 1);
    assert_eq!(
        sqlx::query_scalar::<_, i64>("SELECT count(*) FROM g_t_u_recipient")
            .fetch_one(&pool)
            .await
            .unwrap(),
        1
    );
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn real_writes_precede_submissions_and_existing_binary_state_is_reused(pool: PgPool) {
    let store = PostgresDrops(pool.clone());
    store.initialize().await.unwrap();
    let node = Arc::new(Node::default());
    let app = Arc::new(Faucet {
        settings: Some(settings()),
        node: Arc::new(StoredNode {
            node: node.clone(),
            pool: pool.clone(),
            barrier: None,
        }),
        store: Arc::new(store),
        time: Time {
            now: 1000,
            count: 4,
        },
    });
    let first = request(app.clone(), "").await;
    assert_eq!(first.0, 200);
    let bytes = sqlx::query_scalar::<_, Vec<u8>>(
        "SELECT transaction FROM g_t_u_recipient WHERE account = $1",
    )
    .bind(&RECIPIENT.0[..])
    .fetch_one(&pool)
    .await
    .unwrap();
    assert_eq!(
        bytes,
        include_bytes!("../fixtures/faucet/normal.bin").as_slice()
    );
    assert_eq!(
        node.submissions.lock().unwrap().as_slice(),
        std::slice::from_ref(&bytes)
    );
    let second = request(app, "{\"type\":\"scheduled\"}").await;
    assert_eq!(first, second);
    assert_eq!(node.submissions.lock().unwrap().len(), 1);
    // An independently serialized scheduled transaction is also accepted as existing state.
    sqlx::query("UPDATE g_t_u_recipient SET transaction = $1")
        .bind(include_bytes!("../fixtures/faucet/scheduled.bin").as_slice())
        .execute(&pool)
        .await
        .unwrap();
    let app = Arc::new(Faucet {
        settings: Some(settings()),
        node: node.clone(),
        store: Arc::new(PostgresDrops(pool.clone())),
        time: Time {
            now: 2000,
            count: 1,
        },
    });
    let (_, value) = request(app, "").await;
    let transaction: Transaction = from_bytes(&mut std::io::Cursor::new(include_bytes!(
        "../fixtures/faucet/scheduled.bin"
    )))
    .unwrap();
    assert_eq!(
        value,
        json!({"submissionId":Submission::AccountTransaction(transaction).hash()})
    );
    sqlx::raw_sql("DROP TABLE g_t_u_recipient")
        .execute(&pool)
        .await
        .unwrap();
    let app = Arc::new(Faucet {
        settings: Some(settings()),
        node,
        store: Arc::new(PostgresDrops(pool)),
        time: Time {
            now: 1000,
            count: 1,
        },
    });
    assert_eq!(request(app, "").await.0, 500);
}
