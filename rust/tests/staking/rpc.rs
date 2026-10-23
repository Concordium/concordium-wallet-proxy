//! This module checks staking routes through the actual SDK transport.
//! Local gRPC handlers verify selectors, foundation lookup, and query order.

use super::*;
use axum::{
    extract::{Request as RpcRequest, State},
    response::Response,
    routing::post,
};
use concordium_wallet_proxy::{config::NodeConfig, node::SdkNode};
use std::future::{Ready, ready};

/// This alias keeps large controlled RPC failures outside response values.
type RpcResult<T> = Result<T, Box<tonic::Status>>;

/// This service validates one request and supplies a controlled response.
struct Unary<Q, R> {
    /// This adapter records requests and selects responses.
    node: Arc<TestNode>,
    /// This function validates the request fields.
    operation: fn(&TestNode, Q) -> RpcResult<R>,
}
impl<Q: Send + 'static, R: Send + 'static> tonic::server::UnaryService<Q> for Unary<Q, R> {
    type Response = R;
    type Future = Ready<Result<tonic::Response<R>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<Q>) -> Self::Future {
        ready(
            (self.operation)(&self.node, request.into_inner())
                .map(|value| {
                    let mut response = tonic::Response::new(value);
                    response
                        .metadata_mut()
                        .insert("blockhash", "03".repeat(32).parse().unwrap());
                    response
                })
                .map_err(|status| *status),
        )
    }
}
async fn dispatch<Q, R>(
    node: Arc<TestNode>,
    request: RpcRequest,
    operation: fn(&TestNode, Q) -> RpcResult<R>,
) -> Response
where
    Q: prost::Message + Default + Send + 'static,
    R: prost::Message + Default + Send + 'static,
{
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Unary { node, operation }, request)
        .await
        .map(Body::new)
}
fn record(node: &TestNode, operation: &str) -> RpcResult<()> {
    node.record(operation).map_err(|failure| match failure {
        NetworkFailure::Status(status) => status,
        _ => panic!("Expected RPC status in transport test"),
    })
}
fn selector(input: p::BlockHashInput) -> &'static str {
    match input.block_hash_input.unwrap() {
        p::block_hash_input::BlockHashInput::LastFinal(_) => "lastFinal",
        p::block_hash_input::BlockHashInput::Given(hash) => {
            assert_eq!(hash.value, [3; 32]);
            "given"
        }
        _ => panic!("Unexpected block selector"),
    }
}
async fn pool(State(node): State<Arc<TestNode>>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::PoolInfoRequest| {
        assert_eq!(input.baker.unwrap().value, 42);
        assert_eq!(selector(input.block_hash.unwrap()), "lastFinal");
        record(node, "pool:42")?;
        Ok(node.pool.clone())
    })
    .await
}
async fn passive(State(node): State<Arc<TestNode>>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::BlockHashInput| {
        assert_eq!(selector(input), "lastFinal");
        record(node, "passive")?;
        Ok(passive_proto())
    })
    .await
}
async fn parameters(State(node): State<Arc<TestNode>>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::BlockHashInput| {
        record(node, &format!("chain:{}", selector(input)))?;
        Ok(fixtures::chain(node.version))
    })
    .await
}
async fn foundation(State(node): State<Arc<TestNode>>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::AccountInfoRequest| {
        assert_eq!(
            input.account_identifier.unwrap().account_identifier_input,
            Some(
                p::account_identifier_input::AccountIdentifierInput::Address(p::AccountAddress {
                    value: vec![1; 32]
                })
            )
        );
        record(
            node,
            &format!("foundation:{}", selector(input.block_hash.unwrap())),
        )?;
        Ok(balances::account())
    })
    .await
}
async fn rewards(State(node): State<Arc<TestNode>>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::BlockHashInput| {
        record(node, &format!("rewards:{}", selector(input)))?;
        Ok(balances::tokenomics(
            node.payday,
            if node.payday { 4 } else { 3 },
        ))
    })
    .await
}
async fn consensus(State(node): State<Arc<TestNode>>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, _: p::Empty| {
        record(node, "consensus")?;
        let mut info = support::consensus_info(false);
        info.epoch_duration = Some(p::Duration { value: 1000 });
        Ok(info)
    })
    .await
}
async fn server(node: Arc<TestNode>) -> support::Server {
    support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetPoolInfo", post(pool))
            .route(
                "/concordium.v2.Queries/GetPassiveDelegationInfo",
                post(passive),
            )
            .route(
                "/concordium.v2.Queries/GetBlockChainParameters",
                post(parameters),
            )
            .route("/concordium.v2.Queries/GetAccountInfo", post(foundation))
            .route("/concordium.v2.Queries/GetTokenomicsInfo", post(rewards))
            .route("/concordium.v2.Queries/GetConsensusInfo", post(consensus))
            .with_state(node),
    )
    .await
}

#[tokio::test]
async fn sdk_chain_parameters_resolve_foundation_at_the_original_last_final_selector() {
    for version in 0..=3 {
        let node = Arc::new(TestNode {
            version,
            ..Default::default()
        });
        let server = server(node.clone()).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap(),
        );
        assert_eq!(
            request(staking::routes(sdk), "/v0/chainParameters").await,
            (200, expected_chain(version))
        );
        assert_eq!(
            *node.calls.lock().unwrap(),
            ["chain:lastFinal", "foundation:lastFinal"]
        );
    }
}

#[tokio::test]
async fn sdk_pending_pool_estimate_uses_response_hash_and_preserves_missed_rounds() {
    let mut node = TestNode::default();
    node.pool.equity_pending_change = Some(fixtures::pending(true, balances::NEXT + 2001));
    node.pool
        .current_payday_info
        .as_mut()
        .unwrap()
        .missed_rounds = Some(u64::MAX);
    let node = Arc::new(node);
    let server = server(node.clone()).await;
    let sdk = Arc::new(
        SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap(),
    );
    let http = support::serve_router(staking::routes(sdk)).await;
    let response = reqwest::Client::builder()
        .no_proxy()
        .build()
        .unwrap()
        .get(format!("{}/v0/bakerPool/42", http.endpoint))
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    let value: Value = response.json().await.unwrap();
    let mut expected = expected("pool");
    expected["bakerStakePendingChange"] = json!({"pendingChangeType": "RemovePool", "effectiveTime": "2026-01-01T00:00:02.001Z", "estimatedChangeTime": "2026-01-01T00:00:04Z"});
    expected["currentPaydayStatus"]["missedRounds"] = json!(u64::MAX);
    assert_eq!(value, expected);
    assert_eq!(
        *node.calls.lock().unwrap(),
        [
            "pool:42",
            "chain:given",
            "foundation:given",
            "rewards:given",
            "consensus"
        ]
    );
}

#[tokio::test]
async fn sdk_passive_payday_and_foundation_failure_preserve_outbound_operations() {
    let node = Arc::new(TestNode {
        fail_at: Some("foundation:lastFinal"),
        failure: Failure::Status(tonic::Code::Unavailable),
        ..Default::default()
    });
    let server = server(node.clone()).await;
    let sdk = Arc::new(
        SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap(),
    );
    let router = staking::routes(sdk);
    assert_eq!(
        request(router.clone(), "/v0/passiveDelegation").await,
        (200, expected("passive"))
    );
    assert_eq!(
        request(router.clone(), "/v0/nextPayday").await,
        (200, json!({"nextPaydayTime": "2026-01-01T00:00:00Z"}))
    );
    assert_eq!(
        request(router, "/v0/chainParameters").await,
        (
            502,
            json!({"error": 0, "errorMessage": "GRPC call failed: Non-OK GRPC status code 'UNAVAILABLE'."})
        )
    );
    assert_eq!(
        *node.calls.lock().unwrap(),
        [
            "passive",
            "rewards:lastFinal",
            "chain:lastFinal",
            "foundation:lastFinal"
        ]
    );
}
