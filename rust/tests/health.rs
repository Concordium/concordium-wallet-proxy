//! This module checks health HTTP responses against source-derived fixtures.
//! It also checks errors, language headers, path redirects, and public CORS handling.

use async_trait::async_trait;
use axum::{body::Body, http::Request};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::types::CryptographicParameters;
use concordium_wallet_proxy::{
    database::History,
    health::{Clock, Health},
    http::router,
    node::{Node, NodeFailure},
};
use http_body_util::BodyExt;
use serde_json::Value;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};
use tower::ServiceExt;

/// This test node returns the result selected by a fixture.
struct TestNode(
    /// This string supplies a UTC block time or selects a failure case.
    String,
);
#[async_trait]
impl Node for TestNode {
    async fn cryptographic_parameters(&self) -> anyhow::Result<CryptographicParameters> {
        anyhow::bail!("Unexpected startup query")
    }
    async fn last_final_time(&self) -> Result<Option<DateTime<Utc>>, NodeFailure> {
        match self.0.as_str() {
            "missing" => Ok(None),
            "transport" => Err(NodeFailure::Transport),
            "invalid" => Err(NodeFailure::InvalidResponse),
            time => Ok(Some(time.parse().unwrap())),
        }
    }
}
/// This test database records calls and can return a controlled failure.
struct TestHistory {
    /// This flag makes each check fail when true.
    fail: bool,
    /// This shared counter records the number of checks.
    calls: Arc<AtomicUsize>,
}
#[async_trait]
impl History for TestHistory {
    async fn check(&self) -> anyhow::Result<()> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        anyhow::ensure!(!self.fail, "Synthetic database failure");
        Ok(())
    }
}
/// This test clock always returns the same time.
struct TestClock(
    /// This UTC time sets the current time for each check.
    DateTime<Utc>,
);
impl Clock for TestClock {
    fn now(&self) -> DateTime<Utc> {
        self.0
    }
}

fn fixture() -> Value {
    serde_json::from_str(include_str!("fixtures/health.json")).unwrap()
}
fn app(node: &str, fail: bool, cors: bool) -> (axum::Router, Arc<AtomicUsize>) {
    let calls = Arc::new(AtomicUsize::new(0));
    let health = Health {
        node: Arc::new(TestNode(node.into())),
        history: Arc::new(TestHistory {
            fail,
            calls: calls.clone(),
        }),
        clock: Arc::new(TestClock(
            fixture()["now"].as_str().unwrap().parse().unwrap(),
        )),
        tolerance_seconds: 300,
    };
    (router(health, cors), calls)
}

#[tokio::test]
async fn source_derived_health_contracts() {
    for case in fixture()["cases"].as_array().unwrap() {
        let node = case["node"].as_str().unwrap();
        let (app, calls) = app(
            node,
            case["databaseFailure"].as_bool().unwrap_or(false),
            false,
        );
        let response = app
            .oneshot(Request::get("/v0/health").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(
            response.status().as_u16(),
            case["status"].as_u64().unwrap() as u16,
            "{}",
            case["name"]
        );
        assert_eq!(
            response.headers()["content-type"],
            "application/json; charset=utf-8"
        );
        assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
        assert_eq!(
            response.headers().contains_key("content-language"),
            case["status"] != 200
        );
        let body: Value =
            serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes())
                .unwrap();
        assert_eq!(body, case["body"], "{}", case["name"]);
        assert_eq!(
            calls.load(Ordering::SeqCst),
            usize::from(node.starts_with("202"))
        );
    }
}

#[tokio::test]
async fn errors_methods_language_and_canonical_paths() {
    for (method, path, status, message) in [
        ("GET", "/unknown", 404, "Not found"),
        ("POST", "/v0/health", 405, "Bad method"),
        ("OPTIONS", "/v0/health", 405, "Bad method"),
    ] {
        let (app, _) = app("missing", false, false);
        let response = app
            .oneshot(
                Request::builder()
                    .method(method)
                    .uri(path)
                    .header("accept-language", "fr, en;q=0.5")
                    .header("cookie", "_LANG=de")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(response.status().as_u16(), status);
        assert_eq!(response.headers()["content-language"], "en");
        let value: Value =
            serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes())
                .unwrap();
        assert_eq!(value, serde_json::json!({"error":1,"errorMessage":message}));
    }
    let (app, calls) = app("missing", false, false);
    let response = app
        .oneshot(
            Request::get("/v0//health/?_LANG=fr")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), 301);
    assert_eq!(response.headers()["location"], "/v0/health?_LANG=fr");
    assert_eq!(calls.load(Ordering::SeqCst), 0);
}

#[tokio::test]
async fn public_cors_is_optional_and_does_not_cover_transak() {
    for enabled in [false, true] {
        let (app, _) = app("missing", false, enabled);
        let response = app
            .oneshot(
                Request::get("/v0/health")
                    .header("origin", "https://wallet.example")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(
            response
                .headers()
                .contains_key("access-control-allow-origin"),
            enabled
        );
    }
    let (app, _) = app("missing", false, true);
    let response = app
        .oneshot(
            Request::builder()
                .method("OPTIONS")
                .uri("/v0/health")
                .header("origin", "https://wallet.example")
                .header("access-control-request-method", "PUT")
                .header("access-control-request-headers", "Content-Type,X-Test")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["access-control-allow-origin"], "*");
    assert_eq!(
        response.headers()["access-control-allow-headers"],
        "Content-Type,X-Test"
    );
    let (app, _) = self::app("missing", false, true);
    let response = app
        .oneshot(
            Request::get("/v0/transakOnRamp")
                .header("origin", "https://wallet.example")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert!(!response
        .headers()
        .contains_key("access-control-allow-origin"));
}

#[tokio::test]
async fn head_has_no_response_body() {
    let (app, _) = app("missing", false, false);
    let response = app
        .oneshot(
            Request::builder()
                .method("HEAD")
                .uri("/v0/health")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert!(response
        .into_body()
        .collect()
        .await
        .unwrap()
        .to_bytes()
        .is_empty());
}
