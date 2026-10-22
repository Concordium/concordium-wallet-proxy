//! This module checks source-derived nonce and encryption-key HTTP contracts.
//! Controlled adapters record validation and account lookup behavior.

use axum::{body::Body, http::Request};
use concordium_rust_sdk::{id::types::AccountAddress, types::queries::AccountNonceResponse};
use concordium_wallet_proxy::{
    accounts::{self, AccountNode},
    network::NetworkFailure,
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};
use tower::ServiceExt;

/// This adapter supplies controlled account results.
struct TestNode {
    /// This counter records calls after address validation.
    calls: AtomicUsize,
    /// This code selects a dependency failure.
    failure: Option<tonic::Code>,
    /// This flag reports whether all transactions are finalized.
    finalized: bool,
}

impl TestNode {
    fn check(&self) -> Result<(), NetworkFailure> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        match self.failure {
            Some(code) => Err(NetworkFailure::Status(
                tonic::Status::new(code, "controlled").into(),
            )),
            None => Ok(()),
        }
    }
}

impl AccountNode for TestNode {
    async fn next_nonce(
        &self,
        address: AccountAddress,
    ) -> Result<AccountNonceResponse, NetworkFailure> {
        assert_eq!(address, AccountAddress([0; 32]));
        self.check()?;
        Ok(AccountNonceResponse {
            nonce: u64::MAX.into(),
            all_final: self.finalized,
        })
    }
    async fn encryption_key(&self, address: AccountAddress) -> Result<String, NetworkFailure> {
        assert_eq!(address, AccountAddress([0; 32]));
        self.check()?;
        Ok("ab".repeat(96))
    }
}

async fn request(node: Arc<TestNode>, route: &str, address: &str) -> (u16, Value) {
    request_router(accounts::routes(node), route, address).await
}

pub(super) async fn request_router(
    router: axum::Router,
    route: &str,
    address: &str,
) -> (u16, Value) {
    let response = router
        .oneshot(
            Request::builder()
                .uri(format!("/v0/{route}/{address}"))
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    (
        response.status().as_u16(),
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap(),
    )
}

#[tokio::test]
async fn account_queries_preserve_shapes_and_pending_reliability() {
    for finalized in [true, false] {
        let node = Arc::new(TestNode {
            calls: AtomicUsize::new(0),
            failure: None,
            finalized,
        });
        let address = AccountAddress([0; 32]).to_string();
        assert_eq!(
            request(node.clone(), "accNonce", &address).await,
            (200, json!({"nonce": u64::MAX, "allFinal": finalized}))
        );
        assert_eq!(
            request(node.clone(), "accEncryptionKey", &address).await,
            (200, json!({"accountEncryptionKey": "ab".repeat(96)}))
        );
        assert_eq!(node.calls.load(Ordering::SeqCst), 2);
    }
}

#[tokio::test]
async fn validation_precedes_node_access_and_missing_accounts_have_custom_errors() {
    for route in ["accNonce", "accEncryptionKey"] {
        let node = Arc::new(TestNode {
            calls: AtomicUsize::new(0),
            failure: Some(tonic::Code::NotFound),
            finalized: false,
        });
        assert_eq!(
            request(node.clone(), route, "invalid").await,
            (
                400,
                json!({"error": 1, "errorMessage": "Malformed account address"})
            )
        );
        assert_eq!(node.calls.load(Ordering::SeqCst), 0);
        assert_eq!(
            request(node.clone(), route, &AccountAddress([0; 32]).to_string()).await,
            (
                404,
                json!({"error": 2, "errorMessage": "Account does not exist"})
            )
        );
        assert_eq!(node.calls.load(Ordering::SeqCst), 1);
    }
}

#[tokio::test]
async fn dependency_failures_keep_the_shared_rpc_contract() {
    for route in ["accNonce", "accEncryptionKey"] {
        let node = Arc::new(TestNode {
            calls: AtomicUsize::new(0),
            failure: Some(tonic::Code::DeadlineExceeded),
            finalized: false,
        });
        assert_eq!(
            request(node, route, &AccountAddress([0; 32]).to_string()).await,
            (
                503,
                json!({"error": 3, "errorMessage": "GRPC call failed: The node is overloaded so the request was cancelled: controlled"})
            )
        );
    }
}
