//! This module checks network HTTP contracts and controlled SDK queries.
//! Expected fields come from the Haskell handlers, not SDK serialization.

use super::support::{self, RpcState};
use axum::{body::Body, http::Request};
use concordium_rust_sdk::{
    common::to_bytes,
    types::{queries::ConsensusInfo, CryptographicParameters},
};
use concordium_wallet_proxy::{
    config::NodeConfig,
    network::{self, Network, NetworkFailure, NetworkNode},
    node::SdkNode,
};
use http_body_util::BodyExt;
use serde_json::{json, Value};
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};
use tower::ServiceExt;

/// This adapter records live calls and returns controlled protocol variants.
#[derive(Clone)]
struct TestNode {
    /// This flag selects BFT consensus.
    bft: bool,
    /// This counter records consensus queries.
    calls: Arc<AtomicUsize>,
    /// This optional code selects an RPC failure.
    failure: Option<tonic::Code>,
}
impl NetworkNode for TestNode {
    async fn consensus_info(&self) -> Result<ConsensusInfo, NetworkFailure> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        if let Some(code) = self.failure {
            return Err(NetworkFailure::Status(tonic::Status::new(
                code,
                "controlled",
            )));
        }
        Ok(support::consensus_info(self.bft).try_into().unwrap())
    }
}

/// This type supplies network operations without health, history, or clock capabilities.
struct TestDependencies;

impl concordium_wallet_proxy::dependencies::Dependencies for TestDependencies {
    type Node = TestNode;
    type History = ();
    type Clock = ();
}

fn expected(bft: bool) -> Value {
    let mut value: Value = serde_json::from_str(include_str!("fixtures/network.json")).unwrap();
    if bft {
        value.as_object_mut().unwrap().remove("slotDuration");
        value["protocolVersion"] = json!(6);
        value["concordiumBFTStatus"] = json!({
            "currentTimeoutDuration": 1250,
            "currentRound": 9007199254740995_u64,
            "currentEpoch": 8,
            "triggerBlockTime": "2026-01-01T00:00:00.500Z"
        });
    }
    value
}

fn full_app(network: Network<TestDependencies>) -> axum::Router {
    let unused = Arc::new(super::catalogs::UnusedDependencies);
    let health = concordium_wallet_proxy::health::Health::<super::catalogs::TestDependencies> {
        node: super::catalogs::UnusedDependencies,
        history: unused.clone(),
        clock: unused,
        tolerance_seconds: 300,
    };
    concordium_wallet_proxy::http::router_with_network(
        health,
        std::array::from_fn(|_| json!([])),
        Default::default(),
        true,
        network::routes(network),
    )
}

async fn response(app: axum::Router, path: &str) -> (u16, Value) {
    let response = app
        .oneshot(Request::get(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
    assert_eq!(
        response.headers()["content-type"],
        "application/json; charset=utf-8"
    );
    if status >= 400 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    let body = response.into_body().collect().await.unwrap().to_bytes();
    (status, serde_json::from_slice(&body).unwrap())
}

#[tokio::test]
async fn source_derived_network_contracts_and_cached_global() {
    for bft in [false, true] {
        let node = TestNode {
            bft,
            calls: Arc::new(AtomicUsize::new(0)),
            failure: None,
        };
        let parameters = Arc::new(CryptographicParameters::generate_size(
            "test-network".into(),
            2,
        ));
        let app = full_app(Network {
            node: node.clone(),
            parameters: parameters.clone(),
        });
        for _ in 0..2 {
            let (status, global) = response(app.clone(), "/v0/global").await;
            assert_eq!(status, 200);
            // Haskell Versioned wraps the three hex-encoded global-context fields.
            let hex = |bytes: Vec<u8>| bytes.iter().map(|b| format!("{b:02x}")).collect::<String>();
            assert_eq!(
                global,
                json!({"v": 0, "value": {
                    "genesisString": "test-network",
                    "onChainCommitmentKey": hex(to_bytes(&parameters.on_chain_commitment_key)),
                    "bulletproofGenerators": hex(to_bytes(&parameters.bulletproof_generators))
                }})
            );
        }
        assert_eq!(node.calls.load(Ordering::SeqCst), 0);
        assert_eq!(
            response(app.clone(), "/v0/consensusInfo").await,
            (200, expected(bft))
        );
        assert_eq!(
            response(app.clone(), "/v0/epochLength").await,
            (200, json!({"epochLength": 3600000}))
        );
        assert_eq!(
            response(app, "/v0/genesisHash").await,
            (200, json!({"genesis_block": "02".repeat(32)}))
        );
        assert_eq!(node.calls.load(Ordering::SeqCst), 3);
    }
}

#[tokio::test]
async fn rpc_failures_keep_http_codes_and_messages() {
    for (code, status, error, message) in [
        (
            tonic::Code::NotFound,
            404,
            2,
            "Requested object was not found: controlled",
        ),
        (
            tonic::Code::Cancelled,
            503,
            3,
            "The node is overloaded so the request was cancelled: controlled",
        ),
        (
            tonic::Code::ResourceExhausted,
            503,
            3,
            "The node is overloaded so the request was cancelled: controlled",
        ),
        (
            tonic::Code::DeadlineExceeded,
            503,
            3,
            "The node is overloaded so the request was cancelled: controlled",
        ),
        (
            tonic::Code::Unavailable,
            502,
            0,
            "Non-OK GRPC status code 'UNAVAILABLE'.",
        ),
    ] {
        let app = full_app(Network {
            node: TestNode {
                bft: false,
                calls: Arc::new(AtomicUsize::new(0)),
                failure: Some(code),
            },
            parameters: Arc::new(CryptographicParameters::generate_size("test".into(), 0)),
        });
        for path in ["/v0/consensusInfo", "/v0/epochLength", "/v0/genesisHash"] {
            assert_eq!(
                response(app.clone(), path).await,
                (
                    status,
                    json!({"error": error, "errorMessage": format!("GRPC call failed: {message}")})
                )
            );
        }
        assert_eq!(response(app, "/v0/global").await.0, 200);
    }
}

#[tokio::test]
async fn sdk_decode_provenance_is_a_documented_compatibility_blocker() {
    let server = support::serve(RpcState {
        consensus: Default::default(),
        ..Default::default()
    })
    .await;
    let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    // The SDK reports local conversion failures as RPC statuses. Do not guess
    // from message text whether a node or the SDK produced this status.
    let result = node.consensus_info().await;
    assert!(
        matches!(&result,
        Err(NetworkFailure::Status(status)) if status.code() == tonic::Code::InvalidArgument),
        "{result:?}"
    );
}

#[tokio::test]
async fn sdk_consensus_deadline_is_bounded() {
    let server = support::serve(RpcState {
        delay: std::time::Duration::from_secs(2),
        ..Default::default()
    })
    .await;
    let node = SdkNode::connect(&NodeConfig {
        timeout_seconds: 1,
        ..NodeConfig::new(server.endpoint.clone())
    })
    .await
    .unwrap();
    assert!(matches!(node.consensus_info().await,
        Err(NetworkFailure::Status(status)) if status.code() == tonic::Code::DeadlineExceeded));
}

#[tokio::test]
async fn network_routes_keep_shared_http_behavior() {
    let app = full_app(Network {
        node: TestNode {
            bft: false,
            calls: Arc::new(AtomicUsize::new(0)),
            failure: None,
        },
        parameters: Arc::new(CryptographicParameters::generate_size("test".into(), 0)),
    });
    let response = app
        .clone()
        .oneshot(
            Request::get("/v0/consensusInfo/?unused=yes")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), 301);
    assert_eq!(
        response.headers()["location"],
        "/v0/consensusInfo?unused=yes"
    );
    let response = app
        .clone()
        .oneshot(
            Request::get("/v0/epochLength?unused=yes")
                .header("origin", "https://wallet.example")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["access-control-allow-origin"], "*");
    let response = app
        .clone()
        .oneshot(Request::post("/v0/global").body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(response.status(), 405);
    let response = app
        .oneshot(Request::head("/v0/global").body(Body::empty()).unwrap())
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

#[tokio::test]
async fn finalized_timestamp_is_present_when_the_node_supplies_it() {
    let mut proto = support::consensus_info(false);
    proto.last_finalized_time = Some(concordium_rust_sdk::v2::generated::Timestamp {
        value: 1_767_225_600_001,
    });
    let info = proto.try_into().unwrap();
    let mut expected = expected(false);
    expected["lastFinalizedTime"] = json!("2026-01-01T00:00:00.001Z");
    assert_eq!(network::consensus_value(&info), expected);
}

#[tokio::test]
async fn sdk_consensus_protocol_variants_and_failures() {
    for protocol in 1..=10 {
        let bft = protocol >= 6;
        let mut consensus = support::consensus_info(bft);
        consensus.protocol_version = protocol - 1;
        let server = support::serve(RpcState {
            consensus,
            ..Default::default()
        })
        .await;
        let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap();
        let info = node.consensus_info().await.unwrap();
        let mut expected = expected(bft);
        expected["protocolVersion"] = json!(protocol);
        assert_eq!(network::consensus_value(&info), expected);
    }
    let server = support::serve(RpcState {
        consensus_status: Some(tonic::Code::NotFound),
        ..Default::default()
    })
    .await;
    let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    assert!(
        matches!(node.consensus_info().await, Err(NetworkFailure::Status(status)) if status.code() == tonic::Code::NotFound)
    );
}
