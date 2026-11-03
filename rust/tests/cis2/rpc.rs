//! These tests exercise CIS-2 queries through the SDK and local gRPC and HTTP listeners.
//! The mock node accepts only instance lookup and read-only invocation RPCs.

use super::*;
use crate::{health, support};
use axum::{
    extract::{Request, State},
    response::Response,
    routing::post,
};
use concordium_rust_sdk::v2::generated as p;
use concordium_wallet_proxy::{config::NodeConfig, node::SdkNode};
use std::future::{Ready, ready};

/// This state selects transport replies and records complete protobuf inputs.
struct RpcState {
    /// This result supplies V0 or V1 instance information.
    info: p::InstanceInfo,
    /// This code fails instance lookup before invocation.
    info_error: Option<tonic::Code>,
    /// This flag omits the required instance-info block hash.
    missing_hash: bool,
    /// This flag simulates a malformed success without its required block hash.
    missing_invoke_hash: bool,
    /// This flag delays invocation beyond the shared SDK timeout.
    delayed: bool,
    /// These invocation results are consumed in order.
    replies: Mutex<VecDeque<Result<p::InvokeInstanceResponse, tonic::Code>>>,
    /// These requests retain LastFinal instance selectors.
    lookups: Mutex<Vec<p::InstanceInfoRequest>>,
    /// These requests retain pinned blocks and contract parameters.
    invokes: Mutex<Vec<p::InvokeInstanceRequest>>,
}
fn state(v0: bool, replies: Vec<Result<p::InvokeInstanceResponse, tonic::Code>>) -> Arc<RpcState> {
    let owner = Some(p::AccountAddress { value: vec![1; 32] });
    let amount = Some(p::Amount { value: 0 });
    let name = Some(p::InitName {
        value: "init_token".into(),
    });
    let source_module = Some(p::ModuleRef { value: vec![1; 32] });
    let version = if v0 {
        p::instance_info::Version::V0(p::instance_info::V0 {
            model: Some(p::ContractStateV0 { value: vec![] }),
            owner,
            amount,
            methods: vec![],
            name,
            source_module,
        })
    } else {
        p::instance_info::Version::V1(p::instance_info::V1 {
            owner,
            amount,
            methods: vec![],
            name,
            source_module,
        })
    };
    Arc::new(RpcState {
        info: p::InstanceInfo {
            version: Some(version),
        },
        info_error: None,
        missing_hash: false,
        missing_invoke_hash: false,
        delayed: false,
        replies: Mutex::new(replies.into()),
        lookups: Mutex::new(vec![]),
        invokes: Mutex::new(vec![]),
    })
}
fn reply(bytes: Option<Vec<u8>>) -> p::InvokeInstanceResponse {
    p::InvokeInstanceResponse {
        result: Some(p::invoke_instance_response::Result::Success(
            p::invoke_instance_response::Success {
                return_value: bytes,
                used_energy: Some(p::Energy { value: 1 }),
                effects: vec![],
            },
        )),
    }
}

/// This unary service validates and records requests before returning controlled data.
struct Unary<Q, R> {
    /// This state supplies replies and request logs.
    state: Arc<RpcState>,
    /// This callback selects the response for the operation.
    call: fn(&RpcState, Q) -> Result<R, tonic::Code>,
    /// This flag selects the instance-info response header.
    info: bool,
}
impl<Q: Send + 'static, R: Send + 'static> tonic::server::UnaryService<Q> for Unary<Q, R> {
    type Response = R;
    type Future = Ready<Result<tonic::Response<R>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<Q>) -> Self::Future {
        ready(
            (self.call)(&self.state, request.into_inner())
                .map(|result| {
                    let mut response = tonic::Response::new(result);
                    let missing_hash = if self.info {
                        self.state.missing_hash
                    } else {
                        self.state.missing_invoke_hash
                    };
                    if !missing_hash {
                        response.metadata_mut().insert(
                            "blockhash",
                            if self.info { "09" } else { "08" }
                                .repeat(32)
                                .parse()
                                .unwrap(),
                        );
                    }
                    response
                })
                .map_err(|code| tonic::Status::new(code, "controlled")),
        )
    }
}
async fn dispatch<Q, R>(
    state: Arc<RpcState>,
    request: Request,
    info: bool,
    call: fn(&RpcState, Q) -> Result<R, tonic::Code>,
) -> Response
where
    Q: prost::Message + Default + Send + 'static,
    R: prost::Message + Default + Send + 'static,
{
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Unary { state, call, info }, request)
        .await
        .map(Body::new)
}
async fn info(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    dispatch(
        state,
        request,
        true,
        |state, input: p::InstanceInfoRequest| {
            state.lookups.lock().unwrap().push(input);
            match state.info_error {
                Some(code) => Err(code),
                None => Ok(state.info.clone()),
            }
        },
    )
    .await
}
async fn invoke(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    if state.delayed {
        tokio::time::sleep(std::time::Duration::from_millis(1100)).await;
    }
    dispatch(
        state,
        request,
        false,
        |state, input: p::InvokeInstanceRequest| {
            state.invokes.lock().unwrap().push(input);
            state
                .replies
                .lock()
                .unwrap()
                .pop_front()
                .expect("Unexpected invocation")
        },
    )
    .await
}
async fn sdk(state: Arc<RpcState>) -> (support::Server, Arc<SdkNode>) {
    let server = support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetInstanceInfo", post(info))
            .route("/concordium.v2.Queries/InvokeInstance", post(invoke))
            .with_state(state),
    )
    .await;
    let mut config = NodeConfig::new(server.endpoint.clone());
    config.timeout_seconds = 1;
    let node = Arc::new(SdkNode::connect(&config).await.unwrap());
    (server, node)
}

#[tokio::test]
async fn sdk_pins_every_query_to_info_header_for_both_instance_and_endpoint_versions() {
    for v0 in [false, true] {
        for partial in [false, true] {
            for balance in [false, true] {
                let mut bytes = if balance {
                    vec![1, 0, 5]
                } else {
                    metadata_result("https://example.org")
                };
                if !partial {
                    bytes[0] = 2;
                    if balance {
                        bytes.push(6);
                    } else {
                        bytes.extend(metadata("ipfs://second", Some([0xab; 32])));
                    }
                }
                let replies = if partial {
                    vec![Ok(reply(Some(bytes.clone()))), Ok(reply(Some(bytes)))]
                } else {
                    vec![Ok(reply(Some(bytes)))]
                };
                let state = state(v0, replies);
                let (_server, node) = sdk(state.clone()).await;
                let path = if balance {
                    format!(
                        "/v{}/CIS2TokenBalance/42/7/{ACCOUNT}?tokenId=,AA",
                        u8::from(partial)
                    )
                } else {
                    format!("/v{}/CIS2TokenMetadata/42/7?tokenId=,AA", u8::from(partial))
                };
                let (status, body) = request(cis2::routes(node, Arc::new(NoHistory)), &path).await;
                assert_eq!(status, 200, "{body}");
                assert_eq!(
                    if balance {
                        body.as_array().unwrap().len()
                    } else {
                        body["metadata"].as_array().unwrap().len()
                    },
                    2
                );
                let lookups = state.lookups.lock().unwrap();
                assert_eq!(lookups.len(), 1);
                assert!(matches!(
                    lookups[0].block_hash.as_ref().unwrap().block_hash_input,
                    Some(p::block_hash_input::BlockHashInput::LastFinal(_))
                ));
                assert_eq!(
                    lookups[0].address,
                    Some(p::ContractAddress {
                        index: 42,
                        subindex: 7
                    })
                );
                let calls = state.invokes.lock().unwrap();
                assert_eq!(calls.len(), if partial { 2 } else { 1 });
                for (index, call) in calls.iter().enumerate() {
                    assert_eq!(
                        call.block_hash.as_ref().unwrap().block_hash_input,
                        Some(p::block_hash_input::BlockHashInput::Given(p::BlockHash {
                            value: vec![9; 32]
                        }))
                    );
                    assert!(call.invoker.is_none());
                    assert_eq!(call.amount, Some(p::Amount { value: 0 }));
                    assert_eq!(
                        call.instance,
                        Some(p::ContractAddress {
                            index: 42,
                            subindex: 7
                        })
                    );
                    assert_eq!(
                        call.entrypoint.as_ref().unwrap().value,
                        if balance {
                            "token.balanceOf"
                        } else {
                            "token.tokenMetadata"
                        }
                    );
                    assert_eq!(
                        call.energy,
                        Some(p::Energy {
                            value: if partial { 50_000 } else { 500_000 }
                        })
                    );
                    let mut expected = if partial { vec![1, 0] } else { vec![2, 0] };
                    for token in if partial { vec![index] } else { vec![0, 1] } {
                        if token == 0 {
                            expected.push(0);
                        } else {
                            expected.extend([1, 170]);
                        }
                        if balance {
                            expected.push(0);
                            expected.extend([1; 32]);
                        }
                    }
                    assert_eq!(call.parameter.as_ref().unwrap().value, expected);
                }
            }
        }
    }
}

#[tokio::test]
async fn sdk_errors_missing_headers_and_invalid_responses_never_become_omitted_tokens() {
    for (info_error, missing_hash, malformed, expected) in [
        (Some(tonic::Code::NotFound), false, false, 404),
        (Some(tonic::Code::Unavailable), false, false, 502),
        (None, true, false, 502),
        (None, false, true, 502),
    ] {
        let mut state = state(false, vec![]);
        let settings = Arc::get_mut(&mut state).unwrap();
        settings.info_error = info_error;
        settings.missing_hash = missing_hash;
        if malformed {
            settings.info = p::InstanceInfo::default();
        }
        let (_server, node) = sdk(state.clone()).await;
        assert_eq!(
            request(
                cis2::routes(node, Arc::new(NoHistory)),
                "/v1/CIS2TokenMetadata/42/7?tokenId="
            )
            .await
            .0,
            expected
        );
        assert!(state.invokes.lock().unwrap().is_empty());
    }
    for (reply, expected) in [
        (Err(tonic::Code::NotFound), 404),
        (Err(tonic::Code::ResourceExhausted), 503),
        (Ok(p::InvokeInstanceResponse::default()), 502),
    ] {
        let state = state(
            false,
            vec![Ok(self::reply(Some(metadata_result("first")))), reply],
        );
        let (_server, node) = sdk(state.clone()).await;
        let (status, body) = request(
            cis2::routes(node, Arc::new(NoHistory)),
            "/v1/CIS2TokenMetadata/42/7?tokenId=aa,bb",
        )
        .await;
        assert_eq!(status, expected);
        assert!(body.get("metadata").is_none());
        if expected == 404 {
            // An unknown-block RPC error carries no success blockhash header.
            assert_eq!(body, json!({"error":2,"errorMessage":"Not found"}));
        }
        assert_eq!(state.invokes.lock().unwrap().len(), 2);
    }
}

#[tokio::test]
async fn malformed_success_without_required_invocation_header_fails_defensively() {
    let mut state = state(
        false,
        vec![Ok(reply(Some(metadata_result("otherwise valid"))))],
    );
    Arc::get_mut(&mut state).unwrap().missing_invoke_hash = true;
    let (_server, node) = sdk(state.clone()).await;
    let (status, body) = request(
        cis2::routes(node, Arc::new(NoHistory)),
        "/v1/CIS2TokenMetadata/42/7?tokenId=aa",
    )
    .await;
    // The node contract requires this header on successful invocation responses.
    // This deliberately malformed response is not a supported legacy success case.
    assert_eq!(status, 502);
    assert!(body.get("metadata").is_none());
    assert_eq!(state.invokes.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn sdk_timeout_and_absent_v0_return_keep_versioned_failure_behavior() {
    for partial in [false, true] {
        let state = state(true, vec![Ok(reply(None))]);
        let (_server, node) = sdk(state).await;
        let (status, body) = request(
            cis2::routes(node, Arc::new(NoHistory)),
            &format!("/v{}/CIS2TokenMetadata/42/7?tokenId=", u8::from(partial)),
        )
        .await;
        if partial {
            assert_eq!(
                (status, body),
                (200, json!({"contractName":"token","metadata":[]}))
            );
        } else {
            assert_eq!(
                (status, body),
                (
                    400,
                    json!({"error":1,"errorMessage":"Invoking a V0 contract is not supported."})
                )
            );
        }
    }
    let mut state = state(false, vec![Ok(reply(Some(metadata_result("late"))))]);
    Arc::get_mut(&mut state).unwrap().delayed = true;
    let (_server, node) = sdk(state).await;
    assert_eq!(
        request(
            cis2::routes(node, Arc::new(NoHistory)),
            "/v1/CIS2TokenMetadata/42/7?tokenId="
        )
        .await
        .0,
        503
    );
}

#[tokio::test]
async fn real_http_preserves_success_language_absence_headers_methods_and_read_only_calls() {
    let state = state(
        false,
        vec![
            Ok(reply(Some(metadata_result("ipfs://é")))),
            Ok(reply(Some(metadata_result("ipfs://é")))),
        ],
    );
    let (_rpc, node) = sdk(state.clone()).await;
    let (app, database_calls) = health::app_with_routes(
        "missing",
        false,
        true,
        cis2::routes(node, Arc::new(NoHistory)),
    );
    let server = support::serve_router(app).await;
    let client = reqwest::Client::builder()
        .no_proxy()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .unwrap();
    let url = format!(
        "{}/v0/CIS2TokenMetadata/42/7?tokenId=&lang=da",
        server.endpoint
    );
    let response = client
        .get(&url)
        .header("accept-language", "fr")
        .header("cookie", "_LANG=zh")
        .header("origin", "https://wallet.example")
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert!(!response.headers().contains_key("content-language"));
    assert_eq!(
        response.headers()["content-type"],
        "application/json; charset=utf-8"
    );
    assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
    assert_eq!(response.headers()["access-control-allow-origin"], "*");
    assert_eq!(
        response.json::<Value>().await.unwrap(),
        json!({"contractName":"token","metadata":[{"tokenId":"","metadataURL":"ipfs://é","metadataChecksum":null}]})
    );
    let response = client.head(&url).send().await.unwrap();
    assert_eq!(response.status(), 200);
    assert!(response.bytes().await.unwrap().is_empty());
    assert_eq!(client.post(&url).send().await.unwrap().status(), 405);
    assert_eq!(
        client
            .get(format!(
                "{}/v0/CIS2TokenMetadata/42/7/?tokenId=",
                server.endpoint
            ))
            .send()
            .await
            .unwrap()
            .status(),
        301
    );
    let response = client
        .get(format!(
            "{}/v0/CIS2TokenMetadata/42/7?tokenId=gg",
            server.endpoint
        ))
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 400);
    assert_eq!(response.headers()["content-language"], "en");
    assert_eq!(state.invokes.lock().unwrap().len(), 2);
    assert_eq!(state.lookups.lock().unwrap().len(), 2);
    assert_eq!(database_calls.load(std::sync::atomic::Ordering::SeqCst), 0);
}
