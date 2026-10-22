//! This module checks discovery HTTP behavior against source-derived fixtures.
//! Controlled history adapters verify validation precedence and query inputs.

use axum::{Router, body::Body, http::Request};
use concordium_rust_sdk::id::types::AccountAddress;
use concordium_wallet_proxy::discovery::{self, Binding, DiscoveryHistory};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This adapter records requests and supplies a synthetic legacy binding.
#[derive(Default)]
struct TestHistory {
    /// These pairs record the exact stored key and simple-account restriction.
    calls: Mutex<Vec<([u8; 33], bool)>>,
    /// This flag supplies a controlled database failure.
    failure: bool,
    /// This flag supplies no matching bindings.
    empty: bool,
}
impl DiscoveryHistory for TestHistory {
    async fn key_accounts(&self, key: [u8; 33], only_simple: bool) -> anyhow::Result<Vec<Binding>> {
        self.calls.lock().unwrap().push((key, only_simple));
        anyhow::ensure!(!self.failure, "controlled database failure");
        Ok(if self.empty {
            vec![]
        } else {
            vec![Binding {
                address: AccountAddress([0; 32]),
                credential_index: 255,
                key_index: 0,
                is_simple_account: true,
            }]
        })
    }
}

pub(super) async fn request(router: Router, target: &str) -> (u16, Value) {
    let response = router
        .oneshot(Request::builder().uri(target).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    if status != 200 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    assert!(
        response.headers()["content-type"]
            .to_str()
            .unwrap()
            .starts_with("application/json")
    );
    let value =
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap();
    (status, value)
}

#[tokio::test]
async fn discovery_preserves_shape_case_trailing_bytes_and_first_query_value() {
    let history = Arc::new(TestHistory::default());
    let router = discovery::routes(history.clone());
    let expected = json!([{
        "public_key": {"schemeId": "Ed25519", "verifyKey": "00".repeat(32)},
        "address": "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3",
        "credential_index": 255, "key_index": 0, "is_simple_account": true
    }]);
    for (suffix, simple) in [
        ("", false),
        ("?onlySimple=n", false),
        ("?onlySimple=y", true),
        ("?onlySimple=y&onlySimple=n", true),
        ("?unknown=y", false),
    ] {
        assert_eq!(
            request(
                router.clone(),
                &format!("/v0/keyAccounts/{}{suffix}", "00".repeat(32))
            )
            .await,
            (200, expected.clone())
        );
        assert_eq!(
            history.calls.lock().unwrap().last(),
            Some(&([0; 33], simple))
        );
    }
    assert_eq!(
        request(
            router.clone(),
            &format!("/v0/keyAccounts/{}ABCD", "00".repeat(32))
        )
        .await,
        (200, expected)
    );
    assert_eq!(
        history.calls.lock().unwrap().last(),
        Some(&([0; 33], false))
    );
    let example = "bef37420b5d3a9de90abe87da23d5e109599d5778ca572249176846a26c66395";
    let (status, value) = request(
        router,
        &format!("/v0/keyAccounts/{}", example.to_uppercase()),
    )
    .await;
    assert_eq!(status, 200);
    assert_eq!(value[0]["public_key"]["verifyKey"], example);
}

#[tokio::test]
async fn discovery_validation_matches_legacy_error_text_and_precedence() {
    let history = Arc::new(TestHistory::default());
    let router = discovery::routes(history.clone());
    let fixtures: Value = serde_json::from_str(include_str!("fixtures/discovery.json")).unwrap();
    for fixture in fixtures.as_array().unwrap() {
        let target = fixture["target"].as_str().unwrap();
        assert_eq!(
            request(router.clone(), target).await,
            (400, fixture["body"].clone()),
            "{target}"
        );
    }
    assert!(history.calls.lock().unwrap().is_empty());
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn discovery_uses_legacy_bindings_without_deduplication_or_schema_changes(
    pool: sqlx::PgPool,
) {
    use concordium_wallet_proxy::{database::PostgresHistory, discovery::DiscoveryHistory};
    sqlx::raw_sql(include_str!("fixtures/schema.sql"))
        .execute(&pool)
        .await
        .unwrap();
    let history = Arc::new(PostgresHistory(pool.clone()));
    let router = discovery::routes(history.clone());
    let target = format!("/v0/keyAccounts/{}", "00".repeat(32));
    assert_eq!(request(router.clone(), &target).await, (200, json!([])));
    // The scheme tag is part of the stored key; raw 32-byte and other-scheme
    // rows must not match. Repeated bindings are intentionally not unique.
    sqlx::raw_sql("INSERT INTO account_public_key_bindings(address, public_key, credential_index, key_index, is_simple_account) VALUES
        (decode(repeat('00',32),'hex'), decode(repeat('00',33),'hex'), 255, 255, true),
        (decode(repeat('00',32),'hex'), decode(repeat('00',33),'hex'), 1, 2, false),
        (decode(repeat('00',32),'hex'), decode(repeat('00',33),'hex'), 5, 0, true),
        (decode(repeat('00',32),'hex'), decode(repeat('00',33),'hex'), 5, 0, true),
        (decode(repeat('00',32),'hex'), decode(repeat('00',32),'hex'), 0, 0, true),
        (decode(repeat('00',32),'hex'), decode('01' || repeat('00',32),'hex'), 0, 0, true);")
        .execute(&pool).await.unwrap();
    let binding = |credential, key, simple| {
        json!({
            "public_key": {"schemeId": "Ed25519", "verifyKey": "00".repeat(32)},
            "address": "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3",
            "credential_index": credential, "key_index": key, "is_simple_account": simple
        })
    };
    let all = json!([
        binding(1, 2, false),
        binding(5, 0, true),
        binding(5, 0, true),
        binding(255, 255, true)
    ]);
    assert_eq!(request(router.clone(), &target).await, (200, all.clone()));
    assert_eq!(
        request(router.clone(), &format!("{target}?onlySimple=n")).await,
        (200, all)
    );
    assert_eq!(
        request(router.clone(), &format!("{target}?onlySimple=y")).await,
        (
            200,
            json!([
                binding(5, 0, true),
                binding(5, 0, true),
                binding(255, 255, true)
            ])
        )
    );
    assert!(
        history
            .key_accounts([1; 33], false)
            .await
            .unwrap()
            .is_empty()
    );
    let count: i64 = sqlx::query_scalar("SELECT count(*) FROM account_public_key_bindings")
        .fetch_one(&pool)
        .await
        .unwrap();
    assert_eq!(count, 6);
    sqlx::raw_sql("DROP TABLE account_public_key_bindings")
        .execute(&pool)
        .await
        .unwrap();
    assert_eq!(
        request(router, &target).await,
        (
            500,
            json!({"error": 0, "errorMessage": "Internal server error"})
        )
    );
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn discovery_preserves_legacy_stored_address_and_word8_decoding(pool: sqlx::PgPool) {
    use concordium_wallet_proxy::{database::PostgresHistory, discovery::DiscoveryHistory};
    sqlx::raw_sql(include_str!("fixtures/schema.sql"))
        .execute(&pool)
        .await
        .unwrap();
    sqlx::raw_sql("INSERT INTO account_public_key_bindings(address, public_key, credential_index, key_index, is_simple_account) VALUES (decode(repeat('00',33),'hex'), decode(repeat('00',33),'hex'), -1, 256, true)").execute(&pool).await.unwrap();
    let history = PostgresHistory(pool.clone());
    let rows = history.key_accounts([0; 33], false).await.unwrap();
    assert_eq!(rows[0].address, AccountAddress([0; 32]));
    assert_eq!((rows[0].credential_index, rows[0].key_index), (255, 0));
    sqlx::raw_sql("UPDATE account_public_key_bindings SET address = decode('00', 'hex')")
        .execute(&pool)
        .await
        .unwrap();
    assert!(history.key_accounts([0; 33], false).await.is_err());
    assert_eq!(
        request(
            discovery::routes(Arc::new(history)),
            &format!("/v0/keyAccounts/{}", "00".repeat(32))
        )
        .await,
        (
            500,
            json!({"error": 0, "errorMessage": "Internal server error"})
        )
    );
}

#[tokio::test]
async fn discovery_empty_results_and_database_failures_keep_http_contract() {
    for (failure, status, body) in [
        (false, 200, json!([])),
        (
            true,
            500,
            json!({"error": 0, "errorMessage": "Internal server error"}),
        ),
    ] {
        let history = Arc::new(TestHistory {
            failure,
            empty: true,
            ..Default::default()
        });
        assert_eq!(
            request(
                discovery::routes(history.clone()),
                &format!("/v0/keyAccounts/{}", "00".repeat(32))
            )
            .await,
            (status, body)
        );
        assert_eq!(history.calls.lock().unwrap().len(), 1);
    }
}
