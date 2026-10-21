//! This module checks health and startup with real PostgreSQL databases.
//! These tests require a disposable server and run only when ignored tests are enabled.

use super::{
    config::config as test_config,
    support::{self, RpcState},
};
use concordium_wallet_proxy::{
    database::{History, PostgresHistory},
    http::router,
    startup::initialize,
};
use sqlx::{ConnectOptions, PgPool};

async fn schema(pool: &PgPool) {
    sqlx::raw_sql(include_str!("fixtures/schema.sql"))
        .execute(pool)
        .await
        .unwrap();
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn health_probe_accepts_empty_tables_and_decodes_existing_rows(pool: PgPool) {
    schema(&pool).await;
    let history = PostgresHistory(pool.clone());
    history.check().await.unwrap();
    sqlx::raw_sql("INSERT INTO summaries(block,timestamp,height,summary) VALUES (decode(repeat('01',32),'hex'), 0, 0, '{}');").execute(&pool).await.unwrap();
    history.check().await.unwrap();
    sqlx::raw_sql(
        "INSERT INTO ati(account,summary) SELECT decode(repeat('02',32),'hex'),id FROM summaries;",
    )
    .execute(&pool)
    .await
    .unwrap();
    history.check().await.unwrap();
    sqlx::raw_sql("UPDATE ati SET account = decode('01','hex');")
        .execute(&pool)
        .await
        .unwrap();
    assert!(history.check().await.is_err());
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn health_probe_detects_schema_and_unsigned_decode_failures(pool: PgPool) {
    schema(&pool).await;
    sqlx::raw_sql("INSERT INTO summaries(block,timestamp,height,summary) VALUES (decode(repeat('01',32),'hex'), -1, 0, '{}'); INSERT INTO ati(account,summary) SELECT decode(repeat('02',32),'hex'),id FROM summaries;").execute(&pool).await.unwrap();
    let history = PostgresHistory(pool.clone());
    assert!(history.check().await.is_err());
    sqlx::raw_sql("DROP TABLE ati;")
        .execute(&pool)
        .await
        .unwrap();
    assert!(history.check().await.is_err());
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn configured_startup_and_http_health_use_real_postgres_and_sdk(pool: PgPool) {
    schema(&pool).await;
    let node = support::serve(RpcState::default()).await;
    let (_dir, mut config) = test_config();
    config.database.url = Some(pool.connect_options().to_url_lossy().to_string());
    config.node.endpoint = node.endpoint.clone();
    config.database.max_connections = std::num::NonZeroU32::new(1).unwrap();
    config.server.health_tolerance_seconds = 9_223_372_036_854_775;
    let application = initialize(&config).await.unwrap();
    assert_eq!(
        application.cryptographic_parameters.genesis_string,
        "test-network"
    );
    let server = support::serve_router(router(application.health, false)).await;
    let http = reqwest::Client::builder().no_proxy().build().unwrap();
    let response = http
        .get(format!("{}/v0/health", server.endpoint))
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(
        response.json::<serde_json::Value>().await.unwrap()["healthy"],
        true
    );
    sqlx::raw_sql("DROP TABLE ati;")
        .execute(&pool)
        .await
        .unwrap();
    let response = http
        .get(format!("{}/v0/health", server.endpoint))
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 500);
    assert_eq!(
        response.json::<serde_json::Value>().await.unwrap(),
        serde_json::json!({"error":0,"errorMessage":"Internal server error"})
    );
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn startup_fails_when_required_node_initialization_fails(pool: PgPool) {
    schema(&pool).await;
    let node = support::serve(RpcState {
        crypto_status: Some(tonic::Code::Unavailable),
        ..Default::default()
    })
    .await;
    let (_dir, mut config) = test_config();
    config.database.url = Some(pool.connect_options().to_url_lossy().to_string());
    config.node.endpoint = node.endpoint.clone();
    assert!(initialize(&config).await.is_err());
}
