//! These tests read the legacy CIS-2 catalogue from isolated PostgreSQL databases.
//! Supplies and token bytes remain exact, and discovery never queries the node.

use super::*;
use concordium_wallet_proxy::database::PostgresHistory;
use sqlx::PgPool;

async fn schema(pool: &PgPool) {
    sqlx::raw_sql(include_str!("../fixtures/schema.sql"))
        .execute(pool)
        .await
        .unwrap();
}
async fn insert(pool: &PgPool, id: i64, index: i64, subindex: i64, token: &[u8], supply: &str) {
    sqlx::query("INSERT INTO cis2_tokens(id,index,subindex,token_id,total_supply) VALUES ($1,$2,$3,$4,$5::text::numeric)")
        .bind(id).bind(index).bind(subindex).bind(token).bind(supply).execute(pool).await.unwrap();
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn discovery_preserves_external_schema_precision_raw_ids_cursor_bounds_and_errors(
    pool: PgPool,
) {
    schema(&pool).await;
    let history = Arc::new(PostgresHistory(pool.clone()));
    let node = node(vec![]);
    let router = cis2::routes(node.clone(), history);
    assert_eq!(
        request(router.clone(), "/v0/CIS2Tokens/42/7").await,
        (200, json!({"limit":20,"count":0,"tokens":[]}))
    );
    let huge = "9".repeat(80);
    insert(&pool, 9, 42, 7, &[], "0").await;
    insert(&pool, 5, 42, 7, &[0xde, 0xad, 0xbe, 0xef], &huge).await;
    insert(&pool, 7, 42, 7, &[0xff; 256], "-42").await;
    insert(&pool, 1, 43, 7, &[], "1").await;
    insert(&pool, 2, 42, 8, &[], "2").await;
    let expected = json!([{"id":5,"token":"deadbeef","totalSupply":huge},{"id":7,"token":"ff".repeat(256),"totalSupply":"-42"},{"id":9,"token":"","totalSupply":"0"}]);
    assert_eq!(
        request(router.clone(), "/v0/CIS2Tokens/42/7").await,
        (200, json!({"limit":20,"count":3,"tokens":expected}))
    );
    let (_, body) = request(
        router.clone(),
        "/v0/CIS2Tokens/42/7?from=5&limit=0x1&limit=2",
    )
    .await;
    assert_eq!(
        body,
        json!({"limit":1,"from":5,"count":1,"tokens":[expected[1].clone()]})
    );
    let (_, body) = request(router.clone(), "/v0/CIS2Tokens/42/7?from=9&limit=1001").await;
    assert_eq!(body, json!({"limit":1000,"from":9,"count":0,"tokens":[]}));
    for query in ["limit=-1", "limit=0"] {
        assert_eq!(
            request(router.clone(), &format!("/v0/CIS2Tokens/42/7?{query}"))
                .await
                .1["count"],
            0
        );
    }
    let (_, body) = request(
        router.clone(),
        "/v0/CIS2Tokens/42/7?from=9223372036854775808&limit=bad",
    )
    .await;
    assert_eq!(body["tokens"], expected);
    assert!(body.get("from").is_none());
    insert(&pool, 10, -1, -1, &[0], "123").await;
    assert_eq!(
        request(
            router.clone(),
            "/v0/CIS2Tokens/18446744073709551615/18446744073709551615"
        )
        .await
        .1["tokens"],
        json!([{"id":10,"token":"00","totalSupply":"123"}])
    );
    assert!(node.snapshots.lock().unwrap().is_empty());
    assert!(node.invocations.lock().unwrap().is_empty());
    insert(&pool, 11, 42, 7, &[1], "NaN").await;
    assert_eq!(request(router.clone(), "/v0/CIS2Tokens/42/7").await.0, 500);
    sqlx::raw_sql("DROP TABLE cis2_tokens")
        .execute(&pool)
        .await
        .unwrap();
    assert_eq!(
        request(router, "/v0/CIS2Tokens/42/7").await,
        (
            500,
            json!({"error":0,"errorMessage":"Internal server error"})
        )
    );
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn discovery_caps_database_results_and_retains_unique_contract_token_keys(pool: PgPool) {
    schema(&pool).await;
    sqlx::raw_sql("INSERT INTO cis2_tokens(index,subindex,token_id,total_supply) SELECT 42,7,decode(lpad(to_hex(n),4,'0'),'hex'),n FROM generate_series(1,1005) AS n")
        .execute(&pool).await.unwrap();
    let history = PostgresHistory(pool.clone());
    let address = ContractAddress {
        index: 42,
        subindex: 7,
    };
    let page = history
        .contract_tokens(address, None, u16::MAX)
        .await
        .unwrap();
    assert_eq!(page.len(), 1000);
    assert_eq!(page[0].id, 1);
    assert_eq!(page[999].id, 1000);
    let page = history
        .contract_tokens(address, Some(1000), 20)
        .await
        .unwrap();
    assert_eq!(page.len(), 5);
    assert_eq!(page[0].id, 1001);
    assert!(
        history
            .contract_tokens(address, None, 0)
            .await
            .unwrap()
            .is_empty()
    );
    assert!(sqlx::query("INSERT INTO cis2_tokens(index,subindex,token_id,total_supply) SELECT index,subindex,token_id,total_supply FROM cis2_tokens LIMIT 1").execute(&pool).await.is_err());
}
