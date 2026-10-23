//! This module checks fee context through the real SDK transport.
//! Local handlers verify query order and the Best selector without account lookup.

mod invocation;

use super::*;
use crate::{staking::fixtures, support};
use axum::{
    extract::{Request, State},
    response::Response,
    routing::post,
};
use concordium_rust_sdk::v2::generated as p;
use concordium_wallet_proxy::{config::NodeConfig, node::SdkNode};
use std::{
    future::{Ready, ready},
    sync::Mutex,
};

/// This state selects controlled node responses.
#[derive(Default)]
struct RpcState {
    /// These labels record the order of SDK requests.
    calls: Mutex<Vec<&'static str>>,
    /// This operation returns a non-OK status when selected.
    fail: Option<&'static str>,
    /// This flag selects malformed chain parameters.
    malformed: bool,
    /// This number selects the chain-parameter version.
    version: u8,
    /// This flag selects an invocation rejection.
    reject: bool,
    /// This flag selects a response with no invocation result.
    malformed_invoke: bool,
    /// This flag delays invocation beyond the node timeout.
    delayed_invoke: bool,
}
/// This alias keeps large RPC failures outside response values.
type RpcResult<T> = Result<T, Box<tonic::Status>>;
/// This service validates one unary SDK request.
struct Unary<Q, R> {
    /// This state records the operation and selects responses.
    state: Arc<RpcState>,
    /// This function validates the protobuf request.
    operation: fn(&RpcState, Q) -> RpcResult<R>,
}
impl<Q: Send + 'static, R: Send + 'static> tonic::server::UnaryService<Q> for Unary<Q, R> {
    type Response = R;
    type Future = Ready<Result<tonic::Response<R>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<Q>) -> Self::Future {
        ready(
            (self.operation)(&self.state, request.into_inner())
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
fn record(state: &RpcState, name: &'static str) -> RpcResult<()> {
    state.calls.lock().unwrap().push(name);
    if state.fail == Some(name) {
        Err(Box::new(tonic::Status::unavailable("controlled")))
    } else {
        Ok(())
    }
}
async fn dispatch<Q, R>(
    state: Arc<RpcState>,
    request: Request,
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
async fn consensus(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    dispatch(state, request, |state, _: p::Empty| {
        record(state, "consensus")?;
        Ok(support::consensus_info(false))
    })
    .await
}
async fn parameters(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    dispatch(state, request, |state, input: p::BlockHashInput| {
        assert!(matches!(
            input.block_hash_input,
            Some(p::block_hash_input::BlockHashInput::Best(_))
        ));
        record(state, "parameters:best")?;
        Ok(if state.malformed {
            p::ChainParameters::default()
        } else {
            fixtures::chain(state.version)
        })
    })
    .await
}
async fn server(state: Arc<RpcState>) -> support::Server {
    support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetConsensusInfo", post(consensus))
            .route(
                "/concordium.v2.Queries/GetBlockChainParameters",
                post(parameters),
            )
            .route(
                "/concordium.v2.Queries/InvokeInstance",
                post(invocation::invoke),
            )
            .with_state(state),
    )
    .await
}

#[tokio::test]
async fn sdk_fee_queries_use_best_in_order_and_do_not_query_foundation_account() {
    for version in 0..=3 {
        let state = Arc::new(RpcState {
            version,
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap(),
        );
        let (rate, _) = sdk.fee_context().await.unwrap();
        // The fixture defines 1/10 EUR per energy and 500 micro-CCD per EUR.
        assert_eq!(
            rate.micro_ccd_per_energy,
            num::rational::Ratio::from_integer(50)
        );
        assert_eq!(
            request(fees::routes(sdk.clone()), "type=simpleTransfer").await,
            (200, json!({"energy":501,"cost":"25050"}))
        );
        assert_eq!(
            *state.calls.lock().unwrap(),
            [
                "consensus",
                "parameters:best",
                "consensus",
                "parameters:best"
            ]
        );
        assert_eq!(request(fees::routes(sdk), "numSignatures=bad").await.0, 400);
        assert_eq!(
            *state.calls.lock().unwrap(),
            [
                "consensus",
                "parameters:best",
                "consensus",
                "parameters:best",
                "consensus",
                "parameters:best"
            ]
        );
    }
}

#[tokio::test]
async fn sdk_fee_context_failures_precede_validation_and_stop_query_sequence() {
    for (fail, malformed, calls) in [
        (Some("consensus"), false, vec!["consensus"]),
        (
            Some("parameters:best"),
            false,
            vec!["consensus", "parameters:best"],
        ),
        (None, true, vec!["consensus", "parameters:best"]),
    ] {
        let state = Arc::new(RpcState {
            fail,
            malformed,
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap(),
        );
        let (status, value) = request(fees::routes(sdk), "numSignatures=bad").await;
        assert_eq!(
            (status, value),
            if malformed {
                (
                    502,
                    json!({"error":0,"errorMessage":"GRPC call failed: Non-OK GRPC status code 'INVALID_ARGUMENT'."}),
                )
            } else {
                (
                    502,
                    json!({"error":0,"errorMessage":"GRPC call failed: Non-OK GRPC status code 'UNAVAILABLE'."}),
                )
            }
        );
        assert_eq!(*state.calls.lock().unwrap(), calls);
    }
}
