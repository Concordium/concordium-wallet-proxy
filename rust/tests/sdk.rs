//! This module checks SDK operations through a local gRPC server.
//! It tests concurrent queries, RPC errors, timeouts, and startup failures.

use super::support::{self, RpcState};
use concordium_wallet_proxy::{
    config::NodeConfig,
    node::{Node, SdkNode},
};
use std::{sync::atomic::Ordering, time::Duration};

#[tokio::test]
async fn sdk_queries_real_grpc_transport_and_can_be_used_concurrently() {
    let state = RpcState::default();
    let calls = state.calls.clone();
    let server = support::serve(state).await;
    let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    let parameters = node.cryptographic_parameters().await.unwrap();
    assert_eq!(parameters.genesis_string, "test-network");
    let (a, b) = tokio::join!(node.last_final_time(), node.last_final_time());
    assert_eq!(a.unwrap().unwrap().timestamp_millis(), 1_767_225_899_000);
    assert!(b.unwrap().is_some());
    assert_eq!(calls.load(Ordering::SeqCst), 2);
}

#[tokio::test]
async fn rpc_error_status_is_an_unhealthy_result_not_an_http_gateway_error() {
    for code in [
        tonic::Code::NotFound,
        tonic::Code::Unavailable,
        tonic::Code::ResourceExhausted,
        tonic::Code::Internal,
    ] {
        let server = support::serve(RpcState {
            block_status: Some(code),
            ..Default::default()
        })
        .await;
        let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap();
        assert!(node.last_final_time().await.unwrap().is_none(), "{code:?}");
    }
}

#[tokio::test]
async fn startup_parameter_failure_is_fatal() {
    let server = support::serve(RpcState {
        crypto_status: Some(tonic::Code::Unavailable),
        ..Default::default()
    })
    .await;
    let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    assert!(node.cryptographic_parameters().await.is_err());
}

#[tokio::test]
async fn health_timeout_is_an_unhealthy_missing_result() {
    let server = support::serve(RpcState {
        delay: Duration::from_secs(2),
        ..Default::default()
    })
    .await;
    let node = SdkNode::connect(&NodeConfig {
        timeout_seconds: 1,
        ..NodeConfig::new(server.endpoint.clone())
    })
    .await
    .unwrap();
    assert!(node.last_final_time().await.unwrap().is_none());
}

#[tokio::test]
async fn unreachable_node_prevents_initialization() {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let endpoint = format!("http://{}", listener.local_addr().unwrap());
    drop(listener);
    assert!(SdkNode::connect(&NodeConfig {
        timeout_seconds: 1,
        ..NodeConfig::new(endpoint)
    })
    .await
    .is_err());
}
