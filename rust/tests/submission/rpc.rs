//! This module checks submission through the real SDK and local gRPC transport.
//! It verifies every transmitted field and prevents retries on rejection or timeout.

mod extended;

use super::*;
use crate::support;
use axum::{
    extract::{Request, State},
    response::Response,
    routing::post,
};
use concordium_rust_sdk::v2::generated as p;
use concordium_wallet_proxy::{config::NodeConfig, node::SdkNode};
use std::{
    future::{Ready, ready},
    sync::atomic::{AtomicUsize, Ordering},
};

/// This state controls one isolated mock submission endpoint.
#[derive(Default)]
struct RpcState {
    /// These requests retain complete submitted protobuf operations.
    requests: Mutex<Vec<p::SendBlockItemRequest>>,
    /// This status rejects submissions without a transport interruption.
    status: Option<tonic::Code>,
    /// This flag delays the reply beyond the client's timeout.
    delayed: bool,
    /// This count resets the first response streams to exercise reconnect limits.
    resets: usize,
    /// This counter records every arriving RPC before a response delay.
    arrivals: AtomicUsize,
    /// This flag selects an invalid node response hash.
    malformed_hash: bool,
}
/// This service supplies a controlled submission response.
struct Submit(
    /// This state retains requests and chooses their outcomes.
    Arc<RpcState>,
);
impl tonic::server::UnaryService<p::SendBlockItemRequest> for Submit {
    type Response = p::TransactionHash;
    type Future = Ready<Result<tonic::Response<Self::Response>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<p::SendBlockItemRequest>) -> Self::Future {
        self.0.requests.lock().unwrap().push(request.into_inner());
        ready(match self.0.status {
            Some(code) => Err(tonic::Status::new(code, "controlled")),
            // Deliberately unrelated to the locally calculated submission identifier.
            None => Ok(tonic::Response::new(p::TransactionHash {
                value: if self.0.malformed_hash {
                    vec![]
                } else {
                    vec![0x55; 32]
                },
            })),
        })
    }
}
async fn dispatch(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    state.arrivals.fetch_add(1, Ordering::SeqCst);
    if state.delayed {
        tokio::time::sleep(std::time::Duration::from_millis(1100)).await;
    }
    let response = tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Submit(state.clone()), request)
        .await;
    if state.requests.lock().unwrap().len() <= state.resets {
        return http::Response::builder()
            .header("content-type", "application/grpc")
            .body(Body::from_stream(futures_util::stream::once(async {
                Err::<axum::body::Bytes, _>(std::io::Error::from(
                    std::io::ErrorKind::ConnectionReset,
                ))
            })))
            .unwrap();
    }
    response.map(Body::new)
}
async fn server(state: Arc<RpcState>) -> support::Server {
    support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/SendBlockItem", post(dispatch))
            .with_state(state),
    )
    .await
}
async fn sdk(server: &support::Server) -> Arc<SdkNode> {
    let mut config = NodeConfig::new(server.endpoint.clone());
    config.connect_retries = 3;
    config.timeout_seconds = 1;
    Arc::new(SdkNode::connect(&config).await.unwrap())
}

#[tokio::test]
async fn sdk_submits_both_credential_types_with_exact_source_payloads_and_ignores_response_hash() {
    let state = Arc::new(RpcState::default());
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
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
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitCredential",
                fixtures::credential(normal).to_string()
            )
            .await,
            (200, json!({"submissionId":hash}))
        );
    }
    let calls = state.requests.lock().unwrap();
    assert_eq!(calls.len(), 2);
    for (index, normal) in [false, true].into_iter().enumerate() {
        let Some(p::send_block_item_request::BlockItem::CredentialDeployment(item)) =
            &calls[index].block_item
        else {
            panic!("Wrong submitted item kind");
        };
        let expected = fixtures::credential_bytes(normal);
        assert_eq!(
            item.message_expiry.as_ref().unwrap().value,
            fixtures::credential(normal)["value"]["messageExpiry"]
                .as_u64()
                .unwrap()
        );
        assert_eq!(
            item.payload,
            Some(p::credential_deployment::Payload::RawPayload(
                expected[9..].to_vec()
            ))
        );
    }
}

#[tokio::test]
async fn sdk_standard_submission_preserves_all_signatures_header_and_opaque_payload() {
    let state = Arc::new(RpcState::default());
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
    let http = support::serve_router(submission::routes(node)).await;
    let client = reqwest::Client::builder().no_proxy().build().unwrap();
    let response = client
        .put(format!("{}/v0/submitTransfer", http.endpoint))
        .header("content-type", "text/plain")
        .body(fixtures::transaction().to_string())
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(
        response.json::<Value>().await.unwrap(),
        json!({"submissionId":"cf836d6b87d0bcb141d85e73b1b33814871253843044509fd14b2ea4478e6768"})
    );
    let calls = state.requests.lock().unwrap();
    assert_eq!(calls.len(), 1);
    let Some(p::send_block_item_request::BlockItem::AccountTransaction(item)) =
        &calls[0].block_item
    else {
        panic!("Wrong submitted item kind");
    };
    let header = item.header.as_ref().unwrap();
    assert_eq!(header.sender.as_ref().unwrap().value, [1; 32]);
    assert_eq!(header.sequence_number.as_ref().unwrap().value, u64::MAX);
    assert_eq!(header.energy_amount.as_ref().unwrap().value, 45_000);
    assert_eq!(header.expiry.as_ref().unwrap().value, u64::MAX);
    let signatures = &item.signature.as_ref().unwrap().signatures;
    assert_eq!(signatures.len(), 2);
    assert_eq!(signatures[&0].signatures.len(), 2);
    assert_eq!(signatures[&0].signatures[&0].value, [0xab; 64]);
    assert_eq!(signatures[&0].signatures[&2].value, [0xcd; 64]);
    assert_eq!(signatures[&3].signatures.len(), 1);
    assert_eq!(signatures[&3].signatures[&1].value, [0xef; 64]);
    let body = fixtures::hex(fixtures::transaction()["transaction"].as_str().unwrap());
    assert_eq!(
        item.payload.as_ref().unwrap().payload,
        Some(p::account_transaction_payload::Payload::RawPayload(
            body[60..].to_vec()
        ))
    );
}

#[tokio::test]
async fn sdk_remote_rejections_are_never_retried_even_with_connection_retries_enabled() {
    for code in [
        tonic::Code::InvalidArgument,
        tonic::Code::AlreadyExists,
        tonic::Code::Unavailable,
        tonic::Code::Internal,
    ] {
        let state = Arc::new(RpcState {
            status: Some(code),
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let node = sdk(&server).await;
        assert_eq!(
            request(
                submission::routes(node),
                "submitTransfer",
                fixtures::transaction().to_string()
            )
            .await,
            (
                400,
                json!({"error":1,"errorMessage":"Transaction rejected by the node"})
            )
        );
        assert_eq!(state.requests.lock().unwrap().len(), 1);
    }
}

#[tokio::test]
async fn sdk_reconnect_replays_the_same_submission_at_most_once_after_transport_reset() {
    for resets in [1, 2] {
        let state = Arc::new(RpcState {
            resets,
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let node = sdk(&server).await;
        assert_eq!(
            request(
                submission::routes(node),
                "submitTransfer",
                fixtures::transaction().to_string()
            )
            .await
            .0,
            if resets == 1 { 200 } else { 400 }
        );
        assert_eq!(state.arrivals.load(Ordering::SeqCst), 2);
        let calls = state.requests.lock().unwrap();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0], calls[1]);
    }
}

#[tokio::test]
async fn malformed_response_hash_exposes_the_shared_sdk_provenance_limit_without_retry() {
    let state = Arc::new(RpcState {
        malformed_hash: true,
        ..Default::default()
    });
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
    assert_eq!(
        request(
            submission::routes(node),
            "submitTransfer",
            fixtures::transaction().to_string()
        )
        .await
        .0,
        400
    );
    assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
    assert_eq!(state.requests.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn sdk_submission_timeout_never_returns_success_or_replays_the_transaction() {
    let state = Arc::new(RpcState {
        delayed: true,
        ..Default::default()
    });
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
    assert_eq!(
        request(
            submission::routes(node),
            "submitCredential",
            fixtures::credential(false).to_string()
        )
        .await
        .0,
        400
    );
    tokio::time::sleep(std::time::Duration::from_millis(150)).await;
    assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
    assert!(state.requests.lock().unwrap().len() <= 1);
}
