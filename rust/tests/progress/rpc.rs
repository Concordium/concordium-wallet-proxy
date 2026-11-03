//! This module checks lifecycle lookup through the real SDK and local gRPC transport.
//! It verifies request hashes, response conversion, language headers, and bounded failures.

use super::*;
use crate::{health, support};
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

/// This state selects one local node response and records every request.
#[derive(Default)]
struct RpcState {
    /// This protobuf result is decoded by the actual SDK.
    response: p::BlockItemStatus,
    /// This status fails the operation before a result is returned.
    status: Option<tonic::Code>,
    /// This flag delays the response beyond the configured timeout.
    delayed: bool,
    /// These bytes retain every decoded transaction-hash request.
    hashes: Mutex<Vec<Vec<u8>>>,
    /// This counter records arrivals before any delayed response.
    arrivals: AtomicUsize,
}
/// This service supplies the selected unary lifecycle response.
struct Status(
    /// These settings select results and retain input hashes.
    Arc<RpcState>,
);
impl tonic::server::UnaryService<p::TransactionHash> for Status {
    type Response = p::BlockItemStatus;
    type Future = Ready<Result<tonic::Response<Self::Response>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<p::TransactionHash>) -> Self::Future {
        self.0
            .hashes
            .lock()
            .unwrap()
            .push(request.into_inner().value);
        ready(match self.0.status {
            Some(code) => Err(tonic::Status::new(code, "controlled")),
            None => Ok(tonic::Response::new(self.0.response.clone())),
        })
    }
}
async fn dispatch(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    state.arrivals.fetch_add(1, Ordering::SeqCst);
    if state.delayed {
        tokio::time::sleep(std::time::Duration::from_millis(1100)).await;
    }
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Status(state), request)
        .await
        .map(Body::new)
}
async fn server(state: Arc<RpcState>) -> support::Server {
    support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetBlockItemStatus", post(dispatch))
            .with_state(state),
    )
    .await
}
async fn sdk(server: &support::Server) -> Arc<SdkNode> {
    let mut config = NodeConfig::new(server.endpoint.clone());
    config.timeout_seconds = 1;
    Arc::new(SdkNode::connect(&config).await.unwrap())
}
fn finalized_response() -> p::BlockItemStatus {
    p::BlockItemStatus {
        status: Some(p::block_item_status::Status::Finalized(
            p::block_item_status::Finalized {
                outcome: Some(p::BlockItemSummaryInBlock {
                    block_hash: Some(p::BlockHash { value: vec![3; 32] }),
                    outcome: Some(fixtures::raw_transfer()),
                }),
            },
        )),
    }
}
fn sponsored_expected() -> Value {
    expected(
        json!({"outcome":"success","to":RECEIVER,"amount":"18446744073709551615","memo":"00ff","sponsor":{"sponsor":RECEIVER,"cost":"18446744073709551615"}}),
    )
}

#[tokio::test]
async fn sdk_status_query_uses_hash_bytes_and_preserves_sponsored_finalized_outcome() {
    let state = Arc::new(RpcState {
        response: finalized_response(),
        ..Default::default()
    });
    let server = server(state.clone()).await;
    assert_eq!(
        request(
            progress::routes(sdk(&server).await),
            &format!("/v0/submissionStatus/{}", HASH.to_uppercase())
        )
        .await,
        (200, sponsored_expected())
    );
    assert_eq!(*state.hashes.lock().unwrap(), [vec![0xaa; 32]]);
    assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
}

#[tokio::test]
async fn sdk_received_not_found_and_committed_maps_keep_lifecycle_semantics() {
    for absent in [false, true] {
        let state = Arc::new(RpcState {
            response: p::BlockItemStatus {
                status: Some(p::block_item_status::Status::Received(p::Empty {})),
            },
            status: absent.then_some(tonic::Code::NotFound),
            ..Default::default()
        });
        let server = server(state.clone()).await;
        assert_eq!(
            request(
                progress::routes(sdk(&server).await),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await,
            (
                200,
                json!({"status":if absent {"absent"} else {"received"}})
            )
        );
        assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
    }
    let mut one = fixtures::raw_transfer();
    let mut two = one.clone();
    one.index = Some(p::block_item_summary::TransactionIndex { value: 100 });
    two.index = Some(p::block_item_summary::TransactionIndex { value: 200 });
    let outcomes = vec![(9, one), (1, two)]
        .into_iter()
        .map(|(byte, summary)| p::BlockItemSummaryInBlock {
            block_hash: Some(p::BlockHash {
                value: vec![byte; 32],
            }),
            outcome: Some(summary),
        })
        .collect();
    let state = Arc::new(RpcState {
        response: p::BlockItemStatus {
            status: Some(p::block_item_status::Status::Committed(
                p::block_item_status::Committed { outcomes },
            )),
        },
        ..Default::default()
    });
    let server = server(state).await;
    let mut value = sponsored_expected();
    value["status"] = "committed".into();
    value["blockHashes"] = json!(["01".repeat(32), "09".repeat(32)]);
    assert_eq!(
        request(
            progress::routes(sdk(&server).await),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await,
        (200, value)
    );
}

#[tokio::test]
async fn sdk_differing_committed_outcomes_are_ambiguous_and_duplicate_block_entries_keep_the_last()
{
    for duplicate in [false, true] {
        let mut first = fixtures::raw_transfer();
        if let Some(p::block_item_summary::Details::AccountTransaction(account)) =
            &mut first.details
        {
            account.cost = Some(p::Amount { value: 11 });
        }
        let outcomes = vec![
            p::BlockItemSummaryInBlock {
                block_hash: Some(p::BlockHash { value: vec![9; 32] }),
                outcome: Some(first),
            },
            p::BlockItemSummaryInBlock {
                block_hash: Some(p::BlockHash {
                    value: vec![if duplicate { 9 } else { 1 }; 32],
                }),
                outcome: Some(fixtures::raw_transfer()),
            },
        ];
        let state = Arc::new(RpcState {
            response: p::BlockItemStatus {
                status: Some(p::block_item_status::Status::Committed(
                    p::block_item_status::Committed { outcomes },
                )),
            },
            ..Default::default()
        });
        let server = server(state).await;
        let expected = if duplicate {
            let mut expected = sponsored_expected();
            expected["status"] = "committed".into();
            expected["blockHashes"] = json!(["09".repeat(32)]);
            expected
        } else {
            json!({"status":"committed","blockHashes":["01".repeat(32),"09".repeat(32)],"outcome":"ambiguous"})
        };
        assert_eq!(
            request(
                progress::routes(sdk(&server).await),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await,
            (200, expected)
        );
    }
}

#[tokio::test]
async fn sdk_missing_summary_or_status_fails_without_partial_committed_information() {
    for response in [
        p::BlockItemStatus::default(),
        p::BlockItemStatus {
            status: Some(p::block_item_status::Status::Finalized(
                p::block_item_status::Finalized { outcome: None },
            )),
        },
        p::BlockItemStatus {
            status: Some(p::block_item_status::Status::Committed(
                p::block_item_status::Committed {
                    outcomes: vec![
                        p::BlockItemSummaryInBlock {
                            block_hash: Some(p::BlockHash { value: vec![1; 32] }),
                            outcome: Some(fixtures::raw_transfer()),
                        },
                        p::BlockItemSummaryInBlock {
                            block_hash: Some(p::BlockHash { value: vec![2; 32] }),
                            outcome: None,
                        },
                    ],
                },
            )),
        },
    ] {
        let state = Arc::new(RpcState {
            response,
            ..Default::default()
        });
        let server = server(state).await;
        let (status, body) = request(
            progress::routes(sdk(&server).await),
            &format!("/v0/submissionStatus/{HASH}"),
        )
        .await;
        assert_eq!(status, 502);
        assert_eq!(body["error"], 0);
        assert!(body.get("blockHashes").is_none());
    }
}

#[tokio::test]
async fn sdk_remote_errors_and_timeout_never_become_absent_or_success() {
    for (code, expected_status) in [
        (tonic::Code::Unavailable, 502),
        (tonic::Code::Internal, 502),
        (tonic::Code::ResourceExhausted, 503),
    ] {
        let state = Arc::new(RpcState {
            status: Some(code),
            ..Default::default()
        });
        let server = server(state.clone()).await;
        assert_eq!(
            request(
                progress::routes(sdk(&server).await),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await
            .0,
            expected_status
        );
        assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
    }
    let state = Arc::new(RpcState {
        response: finalized_response(),
        delayed: true,
        ..Default::default()
    });
    let server = server(state.clone()).await;
    let (status, body) = request(
        progress::routes(sdk(&server).await),
        &format!("/v0/submissionStatus/{HASH}"),
    )
    .await;
    assert_eq!(status, 503);
    assert!(body.get("status").is_none());
    assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
}

#[tokio::test]
async fn real_http_status_preserves_language_precedence_metadata_head_and_no_side_effects() {
    let state = Arc::new(RpcState {
        response: finalized_response(),
        ..Default::default()
    });
    let server = server(state.clone()).await;
    let (app, database_calls) =
        health::app_with_routes("missing", false, true, progress::routes(sdk(&server).await));
    let http = support::serve_router(app).await;
    let client = reqwest::Client::builder()
        .no_proxy()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .unwrap();
    let url = format!("{}/v0/submissionStatus/{HASH}", http.endpoint);
    let response = client
        .get(format!("{url}?lang=da"))
        .header("accept-language", "fr")
        .header("cookie", "_LANG=zh")
        .header("origin", "https://wallet.example")
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["content-language"], "en");
    assert_eq!(
        response.headers()["content-type"],
        "application/json; charset=utf-8"
    );
    assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
    assert_eq!(response.headers()["access-control-allow-origin"], "*");
    assert_eq!(
        response.json::<Value>().await.unwrap(),
        sponsored_expected()
    );
    let response = client.head(&url).send().await.unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["content-language"], "en");
    assert!(response.bytes().await.unwrap().is_empty());
    assert_eq!(client.post(&url).send().await.unwrap().status(), 405);
    assert_eq!(
        client.get(format!("{url}/")).send().await.unwrap().status(),
        301
    );
    assert_eq!(state.arrivals.load(Ordering::SeqCst), 2);
    assert_eq!(database_calls.load(Ordering::SeqCst), 0);
}
