//! This module checks token discovery order, explicit JSON fields, and failures.
//! Expectations follow the legacy handlers and token JSON instances.

mod rpc;

use axum::{
    body::{Body, to_bytes},
    http::Request,
};
use concordium_rust_sdk::{
    protocol_level_tokens::{TokenId, TokenInfo},
    v2::{BlockIdentifier, generated as p},
};
use concordium_wallet_proxy::{
    network::NetworkFailure,
    plt::{self, TokenNode},
};
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This adapter records token operations and injects failures.
#[derive(Default)]
struct Node {
    /// These identifiers define node stream order, including duplicates.
    ids: Vec<&'static str>,
    /// This operation fails with a controlled node status.
    fail: Option<&'static str>,
    /// These calls record sequencing and early termination.
    calls: Mutex<Vec<String>>,
}
impl Node {
    fn record(&self, operation: &str) -> Result<(), NetworkFailure> {
        self.calls.lock().unwrap().push(operation.into());
        if self.fail == Some(operation) {
            Err(NetworkFailure::Status(Box::new(tonic::Status::not_found(
                "missing",
            ))))
        } else {
            Ok(())
        }
    }
}
impl TokenNode for Node {
    async fn token_list(&self) -> Result<Vec<TokenId>, NetworkFailure> {
        self.record("list")?;
        Ok(self.ids.iter().map(|id| id.parse().unwrap()).collect())
    }
    async fn token_info(
        &self,
        id: TokenId,
        block: BlockIdentifier,
    ) -> Result<TokenInfo, NetworkFailure> {
        assert!(matches!(block, BlockIdentifier::LastFinal));
        self.record(id.as_ref())?;
        TokenInfo::try_from(p::TokenInfo {
            token_id: Some(p::plt::TokenId {
                value: id.to_string(),
            }),
            token_state: Some(p::plt::TokenState {
                token_module_ref: Some(p::plt::TokenModuleRef { value: vec![2; 32] }),
                decimals: 255,
                total_supply: Some(p::plt::TokenAmount {
                    value: u64::MAX,
                    decimals: 255,
                }),
                module_state: Some(p::plt::Cbor {
                    value: if id.as_ref() == "RAW" {
                        vec![0xff]
                    } else {
                        vec![0xa0]
                    },
                }),
            }),
        })
        .map_err(|_| NetworkFailure::InvalidResponse)
    }
}
fn expected(id: &str) -> Value {
    json!({"tokenId":id,"tokenState":{"tokenModuleRef":"02".repeat(32),"decimals":255,"totalSupply":{"value":"18446744073709551615","decimals":255},"moduleState":if id == "RAW" { json!("ff") } else { json!({}) }}})
}
async fn request(node: Arc<Node>, path: &str) -> (u16, Value) {
    let response = plt::routes(node)
        .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    let value =
        serde_json::from_slice(&to_bytes(response.into_body(), usize::MAX).await.unwrap()).unwrap();
    (status, value)
}

#[tokio::test]
async fn list_preserves_order_duplicates_and_explicit_fields() {
    let node = Arc::new(Node {
        ids: vec!["ZED", "RAW", "ZED"],
        ..Node::default()
    });
    assert_eq!(
        request(node.clone(), "/v0/plt/tokens").await,
        (
            200,
            json!([expected("ZED"), expected("RAW"), expected("ZED")])
        )
    );
    assert_eq!(*node.calls.lock().unwrap(), ["list", "ZED", "RAW", "ZED"]);
}

#[tokio::test]
async fn empty_list_does_not_query_information() {
    let node = Arc::new(Node::default());
    assert_eq!(
        request(node.clone(), "/v0/plt/tokens").await,
        (200, json!([]))
    );
    assert_eq!(*node.calls.lock().unwrap(), ["list"]);
}

#[tokio::test]
async fn information_uses_last_final_and_global_representation() {
    let node = Arc::new(Node::default());
    assert_eq!(
        request(node.clone(), "/v0/plt/tokenInfo/ZED").await,
        (200, expected("ZED"))
    );
    assert_eq!(*node.calls.lock().unwrap(), ["ZED"]);
}

#[tokio::test]
async fn invalid_identifier_preserves_legacy_caught_client_error() {
    let node = Arc::new(Node::default());
    for id in ["bad%20id", "a".repeat(129).as_str()] {
        assert_eq!(
            request(node.clone(), &format!("/v0/plt/tokenInfo/{id}")).await,
            (
                502,
                json!({"error":0,"errorMessage":"Cannot communicate with node via GRPC endpoint."})
            )
        );
    }
    assert!(node.calls.lock().unwrap().is_empty());
}

#[tokio::test]
async fn list_failures_abort_without_partial_results() {
    for (fail, calls) in [("list", vec!["list"]), ("RAW", vec!["list", "ZED", "RAW"])] {
        let node = Arc::new(Node {
            ids: vec!["ZED", "RAW", "END"],
            fail: Some(fail),
            ..Node::default()
        });
        assert_eq!(
            request(node.clone(), "/v0/plt/tokens").await,
            (
                404,
                json!({"error":2,"errorMessage":"GRPC call failed: Requested object was not found: missing"})
            )
        );
        assert_eq!(*node.calls.lock().unwrap(), calls);
    }
}

#[tokio::test]
async fn unknown_identifier_uses_generic_node_error() {
    let node = Arc::new(Node {
        fail: Some("MISSING"),
        ..Node::default()
    });
    assert_eq!(
        request(node, "/v0/plt/tokenInfo/MISSING").await,
        (
            404,
            json!({"error":2,"errorMessage":"GRPC call failed: Requested object was not found: missing"})
        )
    );
}
