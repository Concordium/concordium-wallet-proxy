//! This module checks catalogue files and their versioned HTTP responses.
//! Tests reject requests to external dependencies and use source-derived JSON fixtures.

use async_trait::async_trait;
use axum::{body::Body, http::Request, response::Response, Router};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::types::CryptographicParameters;
use concordium_wallet_proxy::{
    config::load_catalogs,
    database::History,
    health::{Clock, Health},
    http::router,
    node::{Node, NodeFailure},
    startup::initialize,
};
use http_body_util::BodyExt;
use serde_json::{json, Value};
use std::sync::Arc;
use tower::ServiceExt;

/// This type stops a test if a catalogue request uses an external dependency or the clock.
struct UnusedDependencies;

#[async_trait]
impl Node for UnusedDependencies {
    async fn cryptographic_parameters(&self) -> anyhow::Result<CryptographicParameters> {
        panic!("Catalogue requests must not query node parameters");
    }

    async fn last_final_time(&self) -> Result<Option<DateTime<Utc>>, NodeFailure> {
        panic!("Catalogue requests must not query the node");
    }
}

#[async_trait]
impl History for UnusedDependencies {
    async fn check(&self) -> anyhow::Result<()> {
        panic!("Catalogue requests must not query PostgreSQL");
    }
}

impl Clock for UnusedDependencies {
    fn now(&self) -> DateTime<Utc> {
        panic!("Catalogue requests must not read the clock");
    }
}

fn fixtures() -> [Value; 3] {
    [
        include_str!("fixtures/catalogs/v0.json"),
        include_str!("fixtures/catalogs/v1.json"),
        include_str!("fixtures/catalogs/v2.json"),
    ]
    .map(|text| serde_json::from_str(text).unwrap())
}

fn configured_app(catalogs: &[Value; 3], cors: bool) -> (tempfile::TempDir, Router) {
    let (dir, config) = super::config::config();
    let paths = [
        &config.catalogs.v0,
        &config.catalogs.v1,
        &config.catalogs.v2,
    ];
    for (path, value) in paths.into_iter().zip(catalogs) {
        std::fs::write(path, serde_json::to_vec(value).unwrap()).unwrap();
    }
    let loaded = load_catalogs(&config.catalogs).unwrap();
    let dependency = Arc::new(UnusedDependencies);
    let health = Health {
        node: dependency.clone(),
        history: dependency.clone(),
        clock: dependency,
        tolerance_seconds: 300,
    };
    (dir, router(health, loaded, cors))
}

async fn body(response: Response) -> Value {
    serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap()
}

#[tokio::test]
async fn source_derived_request_response_contracts() {
    let contracts: Value =
        serde_json::from_str(include_str!("fixtures/catalogs/http.json")).unwrap();
    let catalogs = fixtures();
    for case in contracts["cases"].as_array().unwrap() {
        let (_dir, app) = configured_app(&catalogs, case["publicCors"].as_bool().unwrap_or(false));
        let input = &case["request"];
        let mut request = Request::builder()
            .method(input["method"].as_str().unwrap())
            .uri(input["uri"].as_str().unwrap());
        for (name, value) in input["headers"].as_object().unwrap() {
            request = request.header(name.as_str(), value.as_str().unwrap());
        }
        let response = app
            .oneshot(request.body(Body::empty()).unwrap())
            .await
            .unwrap();
        let expected = &case["response"];
        assert_eq!(
            response.status().as_u16(),
            expected["status"].as_u64().unwrap() as u16,
            "{}",
            case["name"]
        );
        for (name, value) in expected["headers"].as_object().unwrap() {
            assert_eq!(
                response.headers().get(name.as_str()).unwrap(),
                value.as_str().unwrap(),
                "{}: {name}",
                case["name"]
            );
        }
        if let Some(headers) = expected["absentHeaders"].as_array() {
            for name in headers {
                assert!(
                    !response.headers().contains_key(name.as_str().unwrap()),
                    "{}: {name}",
                    case["name"]
                );
            }
        }
        if expected["emptyBody"].as_bool().unwrap_or(false) {
            assert!(response
                .into_body()
                .collect()
                .await
                .unwrap()
                .to_bytes()
                .is_empty());
        } else if let Some(version) = expected["bodyCatalog"].as_u64() {
            assert_eq!(
                body(response).await,
                catalogs[version as usize],
                "{}",
                case["name"]
            );
        } else {
            assert_eq!(body(response).await, expected["body"], "{}", case["name"]);
        }
    }
}

#[tokio::test]
async fn every_version_accepts_head_and_rejects_writes() {
    let (_dir, app) = configured_app(&fixtures(), false);
    for version in 0..3 {
        let uri = format!("/v{version}/ip_info");
        let response = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("HEAD")
                    .uri(&uri)
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        assert_eq!(
            response.headers()["content-type"],
            "application/json; charset=utf-8"
        );
        assert!(response
            .into_body()
            .collect()
            .await
            .unwrap()
            .to_bytes()
            .is_empty());
        for method in ["POST", "PUT", "DELETE"] {
            let response = app
                .clone()
                .oneshot(
                    Request::builder()
                        .method(method)
                        .uri(&uri)
                        .body(Body::empty())
                        .unwrap(),
                )
                .await
                .unwrap();
            assert_eq!(response.status(), 405);
            assert_eq!(response.headers()["content-language"], "en");
            assert_eq!(
                body(response).await,
                json!({"error": 1, "errorMessage": "Bad method"})
            );
        }
    }
}

#[tokio::test]
async fn catalogue_files_are_loaded_once_before_serving() {
    let catalogs = fixtures();
    let (dir, app) = configured_app(&catalogs, false);
    std::fs::write(dir.path().join("v0.json"), "invalid JSON").unwrap();
    std::fs::remove_file(dir.path().join("v1.json")).unwrap();
    std::fs::write(dir.path().join("v2.json"), "[]").unwrap();
    for (version, expected) in catalogs.iter().enumerate() {
        let response = app
            .clone()
            .oneshot(
                Request::get(format!("/v{version}/ip_info"))
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        assert_eq!(body(response).await, *expected);
    }
}

#[tokio::test]
async fn valid_json_has_no_extra_schema_validation_or_provider_filtering() {
    let mut values = vec![
        json!([]),
        json!({}),
        Value::Null,
        json!(false),
        json!(42),
        json!("opaque catalogue"),
    ];
    // A company catalogue in v0 or v1 must not trigger an added runtime filter.
    values.push(fixtures()[2].clone());
    for value in values {
        let catalogs = std::array::from_fn(|_| value.clone());
        let (_dir, app) = configured_app(&catalogs, false);
        for version in 0..3 {
            let response = app
                .clone()
                .oneshot(
                    Request::get(format!("/v{version}/ip_info"))
                        .body(Body::empty())
                        .unwrap(),
                )
                .await
                .unwrap();
            assert_eq!(response.status(), 200);
            assert_eq!(body(response).await, value);
        }
    }
}

#[tokio::test]
async fn concurrent_requests_keep_versions_separate() {
    let catalogs = fixtures();
    let (_dir, app) = configured_app(&catalogs, false);
    let request = |version| {
        app.clone().oneshot(
            Request::get(format!("/v{version}/ip_info"))
                .body(Body::empty())
                .unwrap(),
        )
    };
    let (v0, v1, v2) = tokio::join!(request(0), request(1), request(2));
    for (response, expected) in [v0, v1, v2].into_iter().zip(catalogs) {
        assert_eq!(body(response.unwrap()).await, expected);
    }
}

#[tokio::test]
async fn malformed_or_missing_catalogues_stop_startup_before_dependency_access() {
    let invalid: &[&[u8]] = &[b"", b" \n", b"not JSON", b"{", b"[]{}", b"\xff"];
    for version in 0..3 {
        for bytes in invalid {
            let (_dir, config) = super::config::config();
            let path = [
                &config.catalogs.v0,
                &config.catalogs.v1,
                &config.catalogs.v2,
            ][version];
            std::fs::write(path, bytes).unwrap();
            let error = initialize(&config).await.err().unwrap();
            assert!(
                format!("{error:#}").contains("Invalid catalogue JSON"),
                "{error:#}"
            );
            assert!(error.to_string().contains(path.to_str().unwrap()));
        }
        let (_dir, config) = super::config::config();
        let path = [
            &config.catalogs.v0,
            &config.catalogs.v1,
            &config.catalogs.v2,
        ][version];
        std::fs::remove_file(path).unwrap();
        let error = initialize(&config).await.err().unwrap();
        assert!(
            format!("{error:#}").contains("Cannot read catalogue"),
            "{error:#}"
        );
        assert!(error.to_string().contains(path.to_str().unwrap()));
    }
}
