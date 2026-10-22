//! This module verifies account queries through local SDK transport.
//! The server checks addresses and the best-block selector.

use super::{accounts::request_router as request, balances::fixtures, support};
use axum::{Router, body::Body, extract::Request, response::Response, routing::post};
use concordium_rust_sdk::{id::types::AccountAddress, v2::generated as p};
use concordium_wallet_proxy::{accounts, config::NodeConfig, node::SdkNode};
use serde_json::json;
use std::{
    future::{Ready, ready},
    sync::Arc,
};

/// This service validates protobuf requests and returns controlled responses.
struct Unary<Q, R>(
    /// This function checks the request and builds its response.
    fn(Q) -> R,
);
impl<Q: Send + 'static, R: Send + 'static> tonic::server::UnaryService<Q> for Unary<Q, R> {
    type Response = R;
    type Future = Ready<Result<tonic::Response<R>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<Q>) -> Self::Future {
        let mut response = tonic::Response::new((self.0)(request.into_inner()));
        response
            .metadata_mut()
            .insert("blockhash", "03".repeat(32).parse().unwrap());
        ready(Ok(response))
    }
}

async fn dispatch<Q, R>(request: Request, operation: fn(Q) -> R) -> Response
where
    Q: prost::Message + Default + Send + 'static,
    R: prost::Message + Default + Send + 'static,
{
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Unary(operation), request)
        .await
        .map(Body::new)
}

async fn nonce(request: Request) -> Response {
    dispatch(request, |input: p::AccountAddress| {
        assert_eq!(input.value, fixtures::ADDRESS);
        p::NextAccountSequenceNumber {
            sequence_number: Some(p::SequenceNumber { value: 0 }),
            all_final: false,
        }
    })
    .await
}
async fn key(request: Request) -> Response {
    dispatch(request, |input: p::AccountInfoRequest| {
        assert_eq!(
            input.account_identifier.unwrap().account_identifier_input,
            Some(
                p::account_identifier_input::AccountIdentifierInput::Address(p::AccountAddress {
                    value: fixtures::ADDRESS.to_vec()
                })
            )
        );
        assert!(matches!(
            input.block_hash.unwrap().block_hash_input,
            Some(p::block_hash_input::BlockHashInput::Best(_))
        ));
        fixtures::account()
    })
    .await
}

#[tokio::test]
async fn sdk_queries_preserve_nonce_and_best_block_key_encoding() {
    let server = support::serve_router(
        Router::new()
            .route(
                "/concordium.v2.Queries/GetNextAccountSequenceNumber",
                post(nonce),
            )
            .route("/concordium.v2.Queries/GetAccountInfo", post(key)),
    )
    .await;
    let node = Arc::new(
        SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap(),
    );
    let address = AccountAddress(fixtures::ADDRESS).to_string();
    let app = accounts::routes(node);
    let (status, value) = request(app.clone(), "accNonce", &address).await;
    assert_eq!(
        (status, value),
        (200, json!({"nonce": 0, "allFinal": false}))
    );
    let expected_key = format!("c0{}c0{}", "00".repeat(47), "00".repeat(47));
    assert_eq!(
        request(app, "accEncryptionKey", &address).await,
        (200, json!({"accountEncryptionKey": expected_key}))
    );
}
