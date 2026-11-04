//! This module checks token-list streaming through local SDK transport.
//! The server validates last-final selection and injects stream decoding failures.

use super::*;
use axum::{
    Router,
    extract::{Request as RpcRequest, State},
    response::Response,
    routing::post,
};
use concordium_wallet_proxy::{config::NodeConfig, node::SdkNode};
use futures_util::{stream, stream::Iter};
use std::future::{Ready, ready};

/// This service supplies ordered token identifiers or a controlled stream error.
struct List {
    /// This mode selects normal, invalid, or failed stream output.
    mode: u8,
}
impl tonic::server::ServerStreamingService<p::BlockHashInput> for List {
    type Response = p::plt::TokenId;
    type ResponseStream = Iter<std::vec::IntoIter<Result<Self::Response, tonic::Status>>>;
    type Future = Ready<Result<tonic::Response<Self::ResponseStream>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<p::BlockHashInput>) -> Self::Future {
        assert!(matches!(
            request.into_inner().block_hash_input,
            Some(p::block_hash_input::BlockHashInput::LastFinal(_))
        ));
        let values = match self.mode {
            1 => vec![Ok(p::plt::TokenId {
                value: "bad id".into(),
            })],
            2 => vec![
                Ok(p::plt::TokenId {
                    value: "ZED".into(),
                }),
                Err(tonic::Status::unavailable("stream failed")),
            ],
            _ => vec![
                Ok(p::plt::TokenId {
                    value: "ZED".into(),
                }),
                Ok(p::plt::TokenId {
                    value: "AAA".into(),
                }),
            ],
        };
        let mut response = tonic::Response::new(stream::iter(values));
        response
            .metadata_mut()
            .insert("blockhash", "03".repeat(32).parse().unwrap());
        ready(Ok(response))
    }
}
async fn list(State(mode): State<u8>, request: RpcRequest) -> Response {
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .server_streaming(List { mode }, request)
        .await
        .map(Body::new)
}

/// This service checks token information requests and supplies global state.
struct Info;
impl tonic::server::UnaryService<p::TokenInfoRequest> for Info {
    type Response = p::TokenInfo;
    type Future = Ready<Result<tonic::Response<Self::Response>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<p::TokenInfoRequest>) -> Self::Future {
        let input = request.into_inner();
        assert!(matches!(
            input.block_hash.unwrap().block_hash_input,
            Some(p::block_hash_input::BlockHashInput::LastFinal(_))
        ));
        let mut response = tonic::Response::new(p::TokenInfo {
            token_id: input.token_id,
            token_state: Some(p::plt::TokenState {
                token_module_ref: Some(p::plt::TokenModuleRef { value: vec![2; 32] }),
                decimals: 255,
                total_supply: Some(p::plt::TokenAmount {
                    value: u64::MAX,
                    decimals: 255,
                }),
                module_state: Some(p::plt::Cbor { value: vec![0xa0] }),
            }),
        });
        response
            .metadata_mut()
            .insert("blockhash", "03".repeat(32).parse().unwrap());
        ready(Ok(response))
    }
}
async fn info(request: RpcRequest) -> Response {
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Info, request)
        .await
        .map(Body::new)
}

#[tokio::test]
async fn sdk_information_and_list_routes_use_independent_last_final_queries() {
    let server = crate::support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetTokenList", post(list))
            .route("/concordium.v2.Queries/GetTokenInfo", post(info))
            .with_state(0),
    )
    .await;
    let node = Arc::new(
        SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap(),
    );
    let app = plt::routes(node);
    for (path, expected) in [
        ("/v0/plt/tokens", json!([expected("ZED"), expected("AAA")])),
        ("/v0/plt/tokenInfo/ZED", expected("ZED")),
    ] {
        let response = app
            .clone()
            .oneshot(Request::builder().uri(path).body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        let actual: Value =
            serde_json::from_slice(&to_bytes(response.into_body(), usize::MAX).await.unwrap())
                .unwrap();
        assert_eq!(actual, expected);
    }
}

#[tokio::test]
async fn sdk_collects_ordered_stream_and_rejects_partial_or_invalid_results() {
    for mode in 0..3 {
        let server = crate::support::serve_router(
            Router::new()
                .route("/concordium.v2.Queries/GetTokenList", post(list))
                .with_state(mode),
        )
        .await;
        let node = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap();
        let result = node.token_list().await;
        if mode == 0 {
            assert_eq!(
                result
                    .unwrap()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>(),
                ["ZED", "AAA"]
            );
        } else {
            assert!(
                result.is_err(),
                "No partial or invalid token list may succeed"
            );
        }
    }
}
