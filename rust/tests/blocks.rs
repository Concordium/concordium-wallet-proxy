//! This module checks block routes against source-derived HTTP expectations.
//! Controlled nodes keep block selection, ordering, and failures deterministic.

pub(super) mod fixtures;
mod outcomes;
mod rpc;

use axum::{Router, body::Body, http::Request};
use concordium_rust_sdk::{
    types::{BlockItemSummary, hashes::BlockHash, queries::BlockInfo},
    v2::generated as p,
};
use concordium_wallet_proxy::{
    blocks::{self, BlockNode},
    network::NetworkFailure,
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This timestamp fixes each block's slot time without wall-clock access.
const NEXT: u64 = 1_767_225_600_000;
/// This value identifies the requested block, regardless of returned metadata.
const HASH: &str = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

/// This adapter supplies independently selected block-query results.
#[derive(Default)]
struct TestNode {
    /// This flag selects the slot-based metadata variant.
    old: bool,
    /// This flag removes the block baker for a genesis block.
    genesis: bool,
    /// This flag supplies a newer protocol version rejected by the legacy client.
    future_protocol: bool,
    /// These hashes retain the node's order and duplicates.
    hashes: Vec<BlockHash>,
    /// These summaries retain the node's stream order.
    summaries: Vec<BlockItemSummary>,
    /// This code supplies a controlled node failure.
    failure: Option<tonic::Code>,
    /// These labels record selected block references and heights.
    calls: Mutex<Vec<String>>,
}
impl TestNode {
    fn record(&self, operation: String) -> Result<(), NetworkFailure> {
        self.calls.lock().unwrap().push(operation);
        match self.failure {
            Some(code) => Err(NetworkFailure::Status(
                tonic::Status::new(code, "controlled").into(),
            )),
            None => Ok(()),
        }
    }
}
impl BlockNode for TestNode {
    async fn block_info(&self, hash: BlockHash) -> Result<BlockInfo, NetworkFailure> {
        self.record(format!("info:{hash}"))?;
        let mut info = fixtures::info(!self.old);
        if self.genesis {
            info.baker = None;
        }
        if self.future_protocol {
            info.protocol_version = p::ProtocolVersion::ProtocolVersion11 as i32;
        }
        Ok(info.try_into().unwrap())
    }
    async fn blocks_at_height(&self, height: u64) -> Result<Vec<BlockHash>, NetworkFailure> {
        self.record(format!("height:{height}"))?;
        Ok(self.hashes.clone())
    }
    async fn transaction_outcomes(
        &self,
        hash: BlockHash,
    ) -> Result<Vec<BlockItemSummary>, NetworkFailure> {
        self.record(format!("events:{hash}"))?;
        Ok(self.summaries.clone())
    }
}
async fn request(router: Router, target: &str) -> (u16, Value) {
    let response = router
        .oneshot(Request::builder().uri(target).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    if status != 200 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    (
        status,
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap(),
    )
}
fn expected(name: &str) -> Value {
    serde_json::from_str::<Value>(include_str!("fixtures/blocks.json")).unwrap()[name].clone()
}
fn success_summary(kind: &str, events: Value) -> Value {
    let mut value = expected("summary");
    value["type"] = json!({"type": "accountTransaction", "contents": kind});
    value["result"] = json!({"outcome": "success", "events": events});
    value
}

#[tokio::test]
async fn block_metadata_preserves_protocol_fields_nulls_and_exact_timestamps() {
    for old in [false, true] {
        for genesis in [false, true] {
            let node = Arc::new(TestNode {
                old,
                genesis,
                ..Default::default()
            });
            let mut value = expected("info");
            if old {
                value["protocolVersion"] = json!(5);
                value["blockSlot"] = json!(2);
                value["round"] = Value::Null;
                value["epoch"] = Value::Null;
            }
            if genesis {
                value["blockBaker"] = Value::Null;
            }
            assert_eq!(
                request(
                    blocks::routes(node.clone()),
                    &format!("/v0/blockInfo/{HASH}")
                )
                .await,
                (200, value)
            );
            assert_eq!(*node.calls.lock().unwrap(), [format!("info:{HASH}")]);
        }
    }
    let node = Arc::new(TestNode {
        future_protocol: true,
        ..Default::default()
    });
    assert_eq!(
        request(blocks::routes(node), &format!("/v0/blockInfo/{HASH}")).await,
        (
            500,
            json!({"error": 0, "errorMessage": "Internal server error"})
        )
    );
}

#[tokio::test]
async fn height_queries_keep_absolute_boundaries_order_duplicates_and_empty_results() {
    for hashes in [vec![], vec![[2; 32].into(), [1; 32].into(), [2; 32].into()]] {
        let node = Arc::new(TestNode {
            hashes: hashes.clone(),
            ..Default::default()
        });
        for height in ["0", "+1", "01", "18446744073709551615"] {
            let value = json!(hashes.iter().map(ToString::to_string).collect::<Vec<_>>());
            assert_eq!(
                request(
                    blocks::routes(node.clone()),
                    &format!("/v0/blocksAtHeight/{height}")
                )
                .await,
                (200, value)
            );
        }
        assert_eq!(
            *node.calls.lock().unwrap(),
            [
                "height:0",
                "height:1",
                "height:1",
                "height:18446744073709551615"
            ]
        );
    }
}

#[tokio::test]
async fn validation_and_missing_blocks_preserve_route_specific_errors() {
    for route in ["blockInfo", "blockTransactionEvents"] {
        let node = Arc::new(TestNode {
            failure: Some(tonic::Code::NotFound),
            ..Default::default()
        });
        for hash in [
            "bad",
            "0xabc",
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaag",
            "%20aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        ] {
            assert_eq!(
                request(blocks::routes(node.clone()), &format!("/v0/{route}/{hash}")).await,
                (
                    400,
                    json!({"error": 1, "errorMessage": "Malformed block hash"})
                )
            );
        }
        assert!(node.calls.lock().unwrap().is_empty());
        assert_eq!(
            request(blocks::routes(node.clone()), &format!("/v0/{route}/{HASH}")).await,
            (404, json!({"error": 2, "errorMessage": "Not found"}))
        );
    }
    let node = Arc::new(TestNode::default());
    for height in ["-1", "bad", "18446744073709551616"] {
        assert_eq!(
            request(
                blocks::routes(node.clone()),
                &format!("/v0/blocksAtHeight/{height}")
            )
            .await,
            (404, json!({"error": 1, "errorMessage": "Not found"}))
        );
    }
    assert!(node.calls.lock().unwrap().is_empty());
    for route in ["blockInfo", "blockTransactionEvents"] {
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(
                blocks::routes(node.clone()),
                &format!("/v0/{route}/{}%20", HASH.to_uppercase())
            )
            .await
            .0,
            200
        );
        assert_eq!(node.calls.lock().unwrap().len(), 1);
    }
}

#[tokio::test]
async fn node_status_errors_keep_the_shared_contract_and_height_not_found_is_not_customized() {
    for path in [
        format!("/v0/blockInfo/{HASH}"),
        format!("/v0/blockTransactionEvents/{HASH}"),
        "/v0/blocksAtHeight/1".into(),
    ] {
        let node = Arc::new(TestNode {
            failure: Some(tonic::Code::DeadlineExceeded),
            ..Default::default()
        });
        assert_eq!(
            request(blocks::routes(node), &path).await,
            (
                503,
                json!({"error": 3, "errorMessage": "GRPC call failed: The node is overloaded so the request was cancelled: controlled"})
            )
        );
    }
    let node = Arc::new(TestNode {
        failure: Some(tonic::Code::NotFound),
        ..Default::default()
    });
    assert_eq!(
        request(blocks::routes(node), "/v0/blocksAtHeight/1").await,
        (
            404,
            json!({"error": 2, "errorMessage": "GRPC call failed: Requested object was not found: controlled"})
        )
    );
}
