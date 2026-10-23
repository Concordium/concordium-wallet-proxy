//! This module checks credential and account submission without production effects.
//! Source-derived byte fixtures verify accepted contents and locally calculated identifiers.

mod extended;
mod fixtures;
mod http_contract;
mod raw;
mod rpc;

use axum::{Router, body::Body};
use concordium_rust_sdk::common::to_bytes;
use concordium_wallet_proxy::{
    network::NetworkFailure,
    submission::{self, Submission, SubmissionNode},
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This adapter records exact serialized block items instead of submitting to a chain.
#[derive(Default)]
struct TestNode {
    /// These bytes retain every attempted block item in order.
    items: Mutex<Vec<Vec<u8>>>,
    /// This flag selects rejection after recording the submitted contents.
    reject: bool,
}
impl SubmissionNode for TestNode {
    async fn submit(&self, item: &Submission) -> Result<(), NetworkFailure> {
        self.items.lock().unwrap().push(to_bytes(item));
        if self.reject {
            Err(NetworkFailure::Transport)
        } else {
            Ok(())
        }
    }
}
async fn request(router: Router, route: &str, body: impl Into<Body>) -> (u16, Value) {
    let response = router
        .oneshot(
            http::Request::builder()
                .method("PUT")
                .uri(format!("/v0/{route}"))
                .body(body.into())
                .unwrap(),
        )
        .await
        .unwrap();
    let status = response.status().as_u16();
    if status != 200 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    let body = response.into_body().collect().await.unwrap().to_bytes();
    (status, serde_json::from_slice(&body).unwrap())
}

#[tokio::test]
async fn both_credential_types_preserve_source_bytes_and_golden_submission_identifiers() {
    for (normal, hash) in [
        (
            false,
            "67fde09681c15241f40a045f31afe5659318d5f87044d15240b682d48de0851a",
        ),
        (
            true,
            "af8d639191fa6194c0ff285bcedb1f47b8b05ca477aae68dae26e1c524e06888",
        ),
    ] {
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitCredential",
                fixtures::credential(normal).to_string()
            )
            .await,
            (200, json!({"submissionId":hash}))
        );
        assert_eq!(
            *node.items.lock().unwrap(),
            [fixtures::credential_bytes(normal)]
        );
    }
}

#[tokio::test]
async fn standard_transaction_keeps_signatures_header_payload_and_golden_identifier() {
    let node = Arc::new(TestNode::default());
    assert_eq!(
        request(
            submission::routes(node.clone()),
            "submitTransfer",
            fixtures::transaction().to_string()
        )
        .await,
        (
            200,
            json!({"submissionId":"cf836d6b87d0bcb141d85e73b1b33814871253843044509fd14b2ea4478e6768"})
        )
    );
    assert_eq!(*node.items.lock().unwrap(), [fixtures::transaction_bytes()]);
}

#[tokio::test]
async fn approved_malformed_credentials_and_versions_fail_locally_without_submission() {
    let node = Arc::new(TestNode::default());
    let mut cases = vec![
        json!({}),
        json!({"v":1,"value":{}}),
        json!({"v":0}),
        Value::Null,
    ];
    for normal in [false, true] {
        let mut version = fixtures::credential(normal);
        version["v"] = 1.into();
        cases.push(version);
        let mut malformed = fixtures::credential(normal);
        malformed["value"]["credential"]["contents"][if normal { "proofs" } else { "sig" }] =
            "".into();
        cases.push(malformed);
        let mut unknown = fixtures::credential(normal);
        unknown["value"]["credential"]["type"] = "other".into();
        cases.push(unknown);
    }
    for body in cases {
        let (status, value) = request(
            submission::routes(node.clone()),
            "submitCredential",
            body.to_string(),
        )
        .await;
        assert_eq!(status, 400);
        assert!(value.get("submissionId").is_none());
    }
    assert!(node.items.lock().unwrap().is_empty());
}

#[tokio::test]
async fn malformed_account_envelopes_and_truncated_binary_never_reach_the_node() {
    let node = Arc::new(TestNode::default());
    let fixture = fixtures::transaction();
    let mut cases = vec![
        json!({}),
        json!({"signatures":{},"transaction":""}),
        json!({"signatures":{"0":{}},"transaction":""}),
        json!({"signatures":fixture["signatures"],"transaction":"zz"}),
    ];
    for bytes in [0, 1, 31, 59, 60, 100] {
        let mut value = fixture.clone();
        value["transaction"] = value["transaction"].as_str().unwrap()[..bytes * 2].into();
        cases.push(value);
    }
    for body in cases {
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitTransfer",
                body.to_string()
            )
            .await
            .0,
            400
        );
    }
    for route in ["submitCredential", "submitTransfer"] {
        for body in ["", "{", "[]", "true", "{\"signatures\":"] {
            assert_eq!(
                request(submission::routes(node.clone()), route, body)
                    .await
                    .0,
                400
            );
        }
    }
    assert!(node.items.lock().unwrap().is_empty());
}

#[tokio::test]
async fn node_failures_return_rejection_without_exposing_a_submission_identifier() {
    let node = Arc::new(TestNode {
        reject: true,
        ..Default::default()
    });
    for (route, body, message) in [
        (
            "submitCredential",
            fixtures::credential(true),
            "Credential rejected by the node",
        ),
        (
            "submitTransfer",
            fixtures::transaction(),
            "Transaction rejected by the node",
        ),
    ] {
        assert_eq!(
            request(submission::routes(node.clone()), route, body.to_string()).await,
            (400, json!({"error":1,"errorMessage":message}))
        );
    }
    assert_eq!(node.items.lock().unwrap().len(), 2);
}

#[tokio::test]
async fn request_prefix_trailing_binary_and_first_duplicate_values_keep_success_contents() {
    let node = Arc::new(TestNode::default());
    let mut fixture = fixtures::transaction();
    let body = format!("{}aabb", fixture["transaction"].as_str().unwrap());
    fixture["transaction"] = body.clone().into();
    let raw = format!(
        "{{\"signatures\":{},\"signatures\":{{}},\"transaction\":\"{body}\"}} ignored trailing input",
        fixture["signatures"]
    );
    assert_eq!(
        request(submission::routes(node.clone()), "submitTransfer", raw)
            .await
            .0,
        200
    );
    assert_eq!(*node.items.lock().unwrap(), [fixtures::transaction_bytes()]);
}

#[tokio::test]
async fn integral_scientific_numbers_and_numeric_key_aliases_preserve_success() {
    let node = Arc::new(TestNode::default());
    let credential = fixtures::credential(false)
        .to_string()
        .replace("\"v\":0", "\"v\":0.0")
        .replace("1619173438", "1619173438.0")
        .replace("\"threshold\":2", "\"threshold\":2e0");
    assert_eq!(
        request(
            submission::routes(node.clone()),
            "submitCredential",
            credential
        )
        .await
        .0,
        200
    );
    let transaction = fixtures::transaction()
        .to_string()
        .replace("\"0\":", "\"0e0\":");
    assert_eq!(
        request(
            submission::routes(node.clone()),
            "submitTransfer",
            transaction
        )
        .await
        .0,
        200
    );
    assert_eq!(
        *node.items.lock().unwrap(),
        [
            fixtures::credential_bytes(false),
            fixtures::transaction_bytes()
        ]
    );
}

#[tokio::test]
async fn account_payload_remains_opaque_until_node_validation() {
    let node = Arc::new(TestNode::default());
    let mut fixture = fixtures::transaction();
    let mut bytes = fixtures::hex(fixture["transaction"].as_str().unwrap());
    bytes[60] = 255;
    fixture["transaction"] = fixtures::encoded(&bytes).into();
    assert_eq!(
        request(
            submission::routes(node.clone()),
            "submitTransfer",
            fixture.to_string()
        )
        .await
        .0,
        200
    );
    let submitted = &node.items.lock().unwrap()[0];
    assert_eq!(submitted[submitted.len() - 41], 255);
}
