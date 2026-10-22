//! This module exercises balance routes through local SDK gRPC transport.
//! The server verifies every account, pool, and pinned-block request.

use super::*;
use axum::{
    Router,
    extract::{Request as RpcRequest, State},
    response::Response,
    routing::post,
};
use concordium_wallet_proxy::{config::NodeConfig, dependencies::Production, node::SdkNode};
use std::future::{Ready, ready};

/// This result keeps the large RPC status out of test response values.
type RpcResult<T> = Result<T, Box<tonic::Status>>;

/// This service returns a controlled unary protobuf response.
struct Unary<Q, R> {
    /// This state supplies responses and records operations.
    node: TestNode,
    /// This function validates the request and supplies its response.
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
    node: TestNode,
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
        _ => panic!("Unexpected controlled failure"),
    })
}
fn block(input: p::BlockHashInput) -> &'static str {
    match input.block_hash_input.unwrap() {
        p::block_hash_input::BlockHashInput::Given(hash) => {
            assert_eq!(hash.value, vec![3; 32]);
            "finalized"
        }
        p::block_hash_input::BlockHashInput::Best(_) => "best",
        _ => panic!("Expected pinned finalized hash or best block"),
    }
}

async fn consensus(State(node): State<TestNode>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, _: p::Empty| {
        record(node, "consensus")?;
        let mut info = crate::support::consensus_info(node.protocol >= 5);
        info.protocol_version = node.protocol;
        info.epoch_duration = Some(p::Duration { value: 1000 });
        Ok(info)
    })
    .await
}
async fn account(State(node): State<TestNode>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::AccountInfoRequest| {
        assert_eq!(
            input.account_identifier.unwrap().account_identifier_input,
            Some(
                p::account_identifier_input::AccountIdentifierInput::Address(p::AccountAddress {
                    value: fixtures::ADDRESS.to_vec()
                })
            )
        );
        let best = block(input.block_hash.unwrap()) == "best";
        record(
            node,
            if best {
                "account:best"
            } else {
                "account:finalized"
            },
        )?;
        (if best { &node.best } else { &node.finalized })
            .clone()
            .ok_or_else(|| tonic::Status::not_found("controlled").into())
    })
    .await
}
async fn pool(State(node): State<TestNode>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::PoolInfoRequest| {
        assert_eq!(input.baker.unwrap().value, 42);
        record(node, &format!("pool:{}", block(input.block_hash.unwrap())))?;
        node.pool
            .clone()
            .ok_or_else(|| tonic::Status::not_found("controlled").into())
    })
    .await
}
async fn tokenomics(State(node): State<TestNode>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::BlockHashInput| {
        assert_eq!(block(input), "finalized");
        record(node, "tokenomics")?;
        Ok(fixtures::tokenomics(node.payday, node.protocol))
    })
    .await
}
async fn parameters(State(node): State<TestNode>, request: RpcRequest) -> Response {
    dispatch(node, request, |node, input: p::BlockHashInput| {
        assert_eq!(block(input), "finalized");
        record(node, "parameters")?;
        Ok(p::ChainParameters {
            parameters: Some(p::chain_parameters::Parameters::V1(p::ChainParametersV1 {
                time_parameters: Some(p::TimeParametersCpv1 {
                    reward_period_length: node.epochs.map(|value| p::RewardPeriodLength {
                        value: Some(p::Epoch { value }),
                    }),
                    mint_per_payday: None,
                }),
                ..Default::default()
            })),
        })
    })
    .await
}
async fn server(node: TestNode) -> crate::support::Server {
    crate::support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetConsensusInfo", post(consensus))
            .route("/concordium.v2.Queries/GetAccountInfo", post(account))
            .route("/concordium.v2.Queries/GetPoolInfo", post(pool))
            .route("/concordium.v2.Queries/GetTokenomicsInfo", post(tokenomics))
            .route(
                "/concordium.v2.Queries/GetBlockChainParameters",
                post(parameters),
            )
            .with_state(node),
    )
    .await
}

#[tokio::test]
async fn sdk_transport_preserves_pool_flags_pending_changes_and_block_selection() {
    // P5 uses pending stake changes. P11 instead has cooldowns and suspension.
    for (protocol, pending) in [(4, true), (10, false)] {
        for version in [0, 1] {
            let mut controlled = TestNode {
                protocol,
                ..Default::default()
            };
            let change = pending.then(|| fixtures::pending(true, fixtures::NEXT + 2001));
            controlled.finalized.as_mut().unwrap().stake =
                Some(fixtures::delegation(Some(42), change));
            let mut validator = fixtures::validator(None);
            if pending {
                controlled.finalized.as_mut().unwrap().cooldowns.clear();
                controlled.best.as_mut().unwrap().cooldowns.clear();
                controlled.pool = Some(fixtures::pool(None, None));
                let Some(p::account_staking_info::StakingInfo::Baker(baker)) =
                    validator.staking_info.as_mut()
                else {
                    unreachable!()
                };
                baker.is_suspended = false;
            }
            controlled.best.as_mut().unwrap().stake = Some(validator);
            let server = server(controlled.clone()).await;
            let sdk = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap();
            let response = body(
                app::<Production>(sdk)
                    .oneshot(Request::get(path(version)).body(Body::empty()).unwrap())
                    .await
                    .unwrap(),
            )
            .await;
            assert_eq!(response.0, 200);
            let mut delegation = json!({"stakedAmount":"2000", "restakeEarnings":true, "delegationTarget":{"delegateType":"Baker","bakerId":42}});
            if pending {
                delegation["pendingChange"] = json!({"change":"RemoveStake","effectiveTime":"2026-01-01T00:00:02.001Z","estimatedChangeTime":"2026-01-01T00:00:04Z"});
            } else {
                delegation["isSuspended"] = json!(false);
                delegation["isPrimedForSuspension"] = json!(true);
            }
            assert_eq!(
                response.1["finalizedBalance"]["accountDelegation"],
                delegation
            );
            if version == 0 {
                assert_eq!(
                    response.1["currentBalance"]["accountBaker"]["isSuspended"],
                    !pending
                );
            }
            let mut calls = vec!["consensus", "account:finalized", "pool:finalized"];
            if version == 0 {
                calls.extend(["account:best", "pool:best"]);
            }
            calls.push("tokenomics");
            if pending {
                calls.push("parameters");
            }
            assert_eq!(controlled.calls(), calls);
        }
    }
}

#[tokio::test]
async fn sdk_transport_preserves_missing_accounts_and_rpc_errors() {
    for (missing, failure, status) in [
        (true, None, 200),
        (
            false,
            Some(("account:finalized", tonic::Code::Cancelled)),
            503,
        ),
        (false, Some(("tokenomics", tonic::Code::Unavailable)), 502),
    ] {
        let controlled = TestNode {
            finalized: if missing {
                None
            } else {
                Some(fixtures::account())
            },
            failure,
            ..Default::default()
        };
        let server = server(controlled.clone()).await;
        let sdk = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap();
        let response = body(
            app::<Production>(sdk)
                .oneshot(Request::get(path(1)).body(Body::empty()).unwrap())
                .await
                .unwrap(),
        )
        .await;
        assert_eq!(response.0, status);
        if missing {
            assert_eq!(response.1, json!({}));
            assert_eq!(controlled.calls(), ["consensus", "account:finalized"]);
        }
    }
}

#[tokio::test]
async fn concurrent_http_requests_use_the_sdk_without_public_dependencies() {
    let controlled = TestNode::default();
    let node_server = server(controlled.clone()).await;
    let sdk = SdkNode::connect(&NodeConfig::new(node_server.endpoint.clone()))
        .await
        .unwrap();
    let http = crate::support::serve_router(app::<Production>(sdk)).await;
    let client = reqwest::Client::new();
    let (legacy, current) = tokio::join!(
        client.get(format!("{}{}", http.endpoint, path(0))).send(),
        client.get(format!("{}{}", http.endpoint, path(1))).send()
    );
    let legacy = legacy.unwrap();
    let current = current.unwrap();
    assert_eq!(legacy.status(), 200);
    assert_eq!(current.status(), 200);
    assert_eq!(
        legacy.json::<Value>().await.unwrap(),
        json!({"finalizedBalance":expected(0), "currentBalance":expected(0)})
    );
    assert_eq!(
        current.json::<Value>().await.unwrap(),
        json!({"finalizedBalance":expected(1)})
    );
    let calls = controlled.calls();
    assert_eq!(
        calls
            .iter()
            .filter(|call| call.as_str() == "account:finalized")
            .count(),
        2
    );
    assert_eq!(
        calls
            .iter()
            .filter(|call| call.as_str() == "account:best")
            .count(),
        1
    );
    assert_eq!(
        calls
            .iter()
            .filter(|call| call.as_str() == "tokenomics")
            .count(),
        2
    );
}

#[tokio::test]
async fn sdk_transport_preserves_legacy_reward_status_and_available_balance_fallback() {
    let mut controlled = TestNode {
        protocol: 2,
        payday: false,
        epochs: None,
        ..Default::default()
    };
    let account = controlled.finalized.as_mut().unwrap();
    account.cooldowns.clear();
    account.available_balance = None;
    let server = server(controlled.clone()).await;
    let sdk = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    let (status, value) = body(
        app::<Production>(sdk)
            .oneshot(Request::get(path(1)).body(Body::empty()).unwrap())
            .await
            .unwrap(),
    )
    .await;
    assert_eq!(status, 200);
    assert_eq!(
        value["finalizedBalance"]["accountAtDisposal"],
        "9007199254737993"
    );
    assert!(value["finalizedBalance"].get("accountDelegation").is_none());
    assert_eq!(
        controlled.calls(),
        ["consensus", "account:finalized", "tokenomics"]
    );
}
