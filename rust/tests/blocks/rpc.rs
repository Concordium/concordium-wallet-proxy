//! This module checks real SDK block requests and server-streaming behavior.
//! Local servers verify selectors and prevent partial HTTP success after stream failures.

use super::super::support;
use super::*;
use axum::{
    extract::{Request as RpcRequest, State},
    response::Response,
    routing::post,
};
use concordium_wallet_proxy::{config::NodeConfig, node::SdkNode};
use futures_util::{Stream, StreamExt};
use std::{
    future::{Ready, ready},
    pin::Pin,
    time::Duration,
};

/// This state controls the local node's unary and streaming responses.
#[derive(Default)]
struct RpcState {
    /// This status fails the request before response metadata is sent.
    initial_status: Option<tonic::Code>,
    /// This status terminates a stream after its summary frames.
    stream_status: Option<tonic::Code>,
    /// These raw summaries are decoded by the actual SDK.
    summaries: Vec<p::BlockItemSummary>,
    /// This flag delays the second summary beyond the query timeout.
    delayed: bool,
    /// These labels record node requests and their inputs.
    calls: Mutex<Vec<String>>,
}
/// This alias keeps large RPC errors outside ordinary response values.
type RpcResult<T> = Result<T, Box<tonic::Status>>;
/// This service validates one unary request.
struct Unary<Q, R> {
    /// These settings select controlled node results.
    state: Arc<RpcState>,
    /// This function validates protobuf inputs.
    operation: fn(&RpcState, Q) -> RpcResult<R>,
}
impl<Q: Send + 'static, R: Send + 'static> tonic::server::UnaryService<Q> for Unary<Q, R> {
    type Response = R;
    type Future = Ready<Result<tonic::Response<R>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<Q>) -> Self::Future {
        ready(if let Some(code) = self.state.initial_status {
            Err(tonic::Status::new(code, "controlled"))
        } else {
            (self.operation)(&self.state, request.into_inner())
                .map(metadata)
                .map_err(|status| *status)
        })
    }
}
fn metadata<T>(value: T) -> tonic::Response<T> {
    let mut response = tonic::Response::new(value);
    response
        .metadata_mut()
        .insert("blockhash", HASH.parse().unwrap());
    response
}
fn hash(input: p::BlockHashInput) {
    assert_eq!(
        input.block_hash_input,
        Some(p::block_hash_input::BlockHashInput::Given(p::BlockHash {
            value: vec![0xaa; 32]
        }))
    );
}
async fn dispatch<Q, R>(
    state: Arc<RpcState>,
    request: RpcRequest,
    operation: fn(&RpcState, Q) -> RpcResult<R>,
) -> Response
where
    Q: prost::Message + Default + Send + 'static,
    R: prost::Message + Default + Send + 'static,
{
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Unary { state, operation }, request)
        .await
        .map(Body::new)
}
async fn info(State(state): State<Arc<RpcState>>, request: RpcRequest) -> Response {
    dispatch(state, request, |state, input: p::BlockHashInput| {
        hash(input);
        state.calls.lock().unwrap().push("info:given".into());
        Ok(fixtures::info(true))
    })
    .await
}
async fn height(State(state): State<Arc<RpcState>>, request: RpcRequest) -> Response {
    dispatch(state, request, |state, input: p::BlocksAtHeightRequest| {
        assert_eq!(
            input.blocks_at_height,
            Some(p::blocks_at_height_request::BlocksAtHeight::Absolute(
                p::blocks_at_height_request::Absolute {
                    height: Some(p::AbsoluteBlockHeight { value: u64::MAX })
                }
            ))
        );
        state
            .calls
            .lock()
            .unwrap()
            .push("height:absolute:max".into());
        Ok(p::BlocksAtHeightResponse {
            blocks: vec![
                p::BlockHash { value: vec![2; 32] },
                p::BlockHash { value: vec![1; 32] },
                p::BlockHash { value: vec![2; 32] },
            ],
        })
    })
    .await
}
/// This service supplies a finite stream of raw protobuf summaries.
struct Events(
    /// These settings control frame order, delays, and failures.
    Arc<RpcState>,
);
impl tonic::server::ServerStreamingService<p::BlockHashInput> for Events {
    type Response = p::BlockItemSummary;
    type ResponseStream =
        Pin<Box<dyn Stream<Item = Result<p::BlockItemSummary, tonic::Status>> + Send>>;
    type Future = Ready<Result<tonic::Response<Self::ResponseStream>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<p::BlockHashInput>) -> Self::Future {
        hash(request.into_inner());
        self.0.calls.lock().unwrap().push("events:given".into());
        if let Some(code) = self.0.initial_status {
            return ready(Err(tonic::Status::new(code, "controlled")));
        }
        let mut frames = self
            .0
            .summaries
            .clone()
            .into_iter()
            .map(Ok)
            .collect::<Vec<_>>();
        if let Some(code) = self.0.stream_status {
            frames.push(Err(tonic::Status::new(code, "controlled")));
        }
        let delayed = self.0.delayed;
        let late_error = self.0.stream_status.is_some();
        let stream = futures_util::stream::iter(frames.into_iter().enumerate()).then(
            move |(index, frame)| async move {
                if delayed && index > 0 {
                    tokio::time::sleep(Duration::from_millis(1100)).await;
                } else if late_error && index > 0 {
                    // Yield Pending so Tonic flushes the successful frame before the error.
                    tokio::time::sleep(Duration::from_millis(20)).await;
                }
                frame
            },
        );
        ready(Ok(metadata(Box::pin(stream) as Self::ResponseStream)))
    }
}
async fn events(State(state): State<Arc<RpcState>>, request: RpcRequest) -> Response {
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .server_streaming(Events(state), request)
        .await
        .map(Body::new)
}
async fn server(state: Arc<RpcState>) -> support::Server {
    support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetBlockInfo", post(info))
            .route("/concordium.v2.Queries/GetBlocksAtHeight", post(height))
            .route(
                "/concordium.v2.Queries/GetBlockTransactionEvents",
                post(events),
            )
            .with_state(state),
    )
    .await
}
fn transfer_summary(index: u64) -> Value {
    let mut transfer = expected("transfer");
    transfer.as_array_mut().unwrap().push(expected("memo"));
    let mut value = success_summary("transferWithMemo", transfer);
    value["index"] = json!(index);
    value["sponsorDetails"] = json!({"sponsor":"2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3","cost":"18446744073709551615"});
    value
}

#[tokio::test]
async fn sdk_block_queries_and_streams_preserve_hashes_absolute_height_order_and_sponsorship() {
    let first = fixtures::raw_transfer();
    let mut second = first.clone();
    second.index = Some(p::block_item_summary::TransactionIndex { value: 3 });
    let state = Arc::new(RpcState {
        summaries: vec![first, second],
        ..Default::default()
    });
    let node = server(state.clone()).await;
    let sdk = Arc::new(
        SdkNode::connect(&NodeConfig::new(node.endpoint.clone()))
            .await
            .unwrap(),
    );
    let http = support::serve_router(blocks::routes(sdk)).await;
    let client = reqwest::Client::builder().no_proxy().build().unwrap();
    for (path, value) in [
        (format!("/v0/blockInfo/{HASH}"), expected("info")),
        (
            "/v0/blocksAtHeight/18446744073709551615".into(),
            json!(["02".repeat(32), "01".repeat(32), "02".repeat(32)]),
        ),
        (
            format!("/v0/blockTransactionEvents/{HASH}"),
            json!([transfer_summary(9), transfer_summary(3)]),
        ),
    ] {
        let response = client
            .get(format!("{}{path}", http.endpoint))
            .send()
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        assert_eq!(response.json::<Value>().await.unwrap(), value);
    }
    assert_eq!(
        *state.calls.lock().unwrap(),
        ["info:given", "height:absolute:max", "events:given"]
    );
}

#[tokio::test]
async fn sdk_empty_stream_initial_not_found_and_late_status_do_not_report_partial_success() {
    for (initial_status, stream_status, summaries, status, expected_body) in [
        (None, None, vec![], 200, json!([])),
        (
            Some(tonic::Code::NotFound),
            None,
            vec![],
            404,
            json!({"error":2,"errorMessage":"Not found"}),
        ),
        (
            None,
            Some(tonic::Code::ResourceExhausted),
            vec![fixtures::raw_transfer()],
            503,
            json!({"error":3,"errorMessage":"GRPC call failed: The node is overloaded so the request was cancelled: controlled"}),
        ),
    ] {
        let state = Arc::new(RpcState {
            initial_status,
            stream_status,
            summaries,
            ..Default::default()
        });
        let node = server(state).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(node.endpoint.clone()))
                .await
                .unwrap(),
        );
        assert_eq!(
            request(
                blocks::routes(sdk),
                &format!("/v0/blockTransactionEvents/{HASH}")
            )
            .await,
            (status, expected_body)
        );
    }
}

#[tokio::test]
async fn sdk_timeout_covers_stream_consumption_and_unknown_summary_aborts_the_whole_response() {
    let first = fixtures::raw_transfer();
    let mut unknown = first.clone();
    unknown.details = None;
    let state = Arc::new(RpcState {
        summaries: vec![first.clone(), unknown],
        ..Default::default()
    });
    let node = server(state).await;
    let sdk = Arc::new(
        SdkNode::connect(&NodeConfig::new(node.endpoint.clone()))
            .await
            .unwrap(),
    );
    assert_eq!(
        request(
            blocks::routes(sdk),
            &format!("/v0/blockTransactionEvents/{HASH}")
        )
        .await,
        (
            500,
            json!({"error":0,"errorMessage":"Internal server error"})
        )
    );
    let state = Arc::new(RpcState {
        summaries: vec![first.clone(), first],
        delayed: true,
        ..Default::default()
    });
    let node = server(state).await;
    let sdk = Arc::new(
        SdkNode::connect(&NodeConfig {
            timeout_seconds: 1,
            ..NodeConfig::new(node.endpoint.clone())
        })
        .await
        .unwrap(),
    );
    assert_eq!(
        request(
            blocks::routes(sdk),
            &format!("/v0/blockTransactionEvents/{HASH}")
        )
        .await,
        (
            503,
            json!({"error":3,"errorMessage":"GRPC call failed: The node is overloaded so the request was cancelled: "})
        )
    );
}
