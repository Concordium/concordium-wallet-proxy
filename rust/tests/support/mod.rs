//! This module supplies local HTTP and gRPC servers for integration tests.
//! The gRPC server returns controlled responses through the actual SDK transport.

use axum::{
    Router,
    body::Body,
    extract::{Request, State},
    response::Response,
    routing::post,
};
use concordium_rust_sdk::{
    common::to_bytes, types::CryptographicParameters, v2::generated as proto,
};
use std::{
    future::Future,
    pin::Pin,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
    time::Duration,
};
use tonic::{Code, Status};

/// This type represents an RPC operation with one response or an error status.
type RpcFuture<T> = Pin<Box<dyn Future<Output = Result<tonic::Response<T>, Status>> + Send>>;

/// This type controls the responses from the test gRPC server.
#[derive(Clone)]
pub struct RpcState {
    /// This optional status replaces the block response with an RPC error.
    pub block_status: Option<Code>,
    /// This optional status replaces the cryptographic-parameter response with an RPC error.
    pub crypto_status: Option<Code>,
    /// This optional status replaces the consensus response.
    pub consensus_status: Option<Code>,
    /// This response supplies the consensus protocol variant.
    pub consensus: proto::ConsensusInfo,
    /// This duration delays each block response.
    pub delay: Duration,
    /// This shared counter records block queries.
    pub calls: Arc<AtomicUsize>,
    /// This value sets the block time in milliseconds since the Unix epoch.
    pub slot_millis: u64,
}
impl Default for RpcState {
    fn default() -> Self {
        Self {
            block_status: None,
            crypto_status: None,
            consensus_status: None,
            consensus: consensus_info(false),
            delay: Duration::ZERO,
            calls: Arc::new(AtomicUsize::new(0)),
            slot_millis: 1_767_225_899_000,
        }
    }
}

/// This type owns a local test server.
/// The server task stops when this value is dropped.
pub struct Server {
    /// This HTTP URL identifies the local listener.
    pub endpoint: String,
    /// This task runs the server until the test stops it.
    task: tokio::task::JoinHandle<()>,
}
impl Drop for Server {
    fn drop(&mut self) {
        self.task.abort();
    }
}

/// Start a local gRPC server with controlled node responses.
pub async fn serve(state: RpcState) -> Server {
    let app = Router::new()
        .route("/concordium.v2.Queries/GetBlockInfo", post(block))
        .route(
            "/concordium.v2.Queries/GetCryptographicParameters",
            post(crypto),
        )
        .route("/concordium.v2.Queries/GetConsensusInfo", post(consensus))
        .with_state(state);
    serve_router(app).await
}

/// Serve an HTTP router on an available loopback port.
pub async fn serve_router(app: Router) -> Server {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let endpoint = format!("http://{}", listener.local_addr().unwrap());
    let task = tokio::spawn(async move {
        axum::serve(listener, app).await.unwrap();
    });
    Server { endpoint, task }
}

/// This type handles a block-information request for the test server.
struct BlockRpc(
    /// These settings control the block response and record calls.
    RpcState,
);
impl tonic::server::UnaryService<proto::BlockHashInput> for BlockRpc {
    type Response = proto::BlockInfo;
    type Future = RpcFuture<Self::Response>;
    fn call(&mut self, request: tonic::Request<proto::BlockHashInput>) -> Self::Future {
        let state = self.0.clone();
        Box::pin(async move {
            assert!(matches!(
                request.into_inner().block_hash_input,
                Some(proto::block_hash_input::BlockHashInput::LastFinal(_))
            ));
            state.calls.fetch_add(1, Ordering::SeqCst);
            tokio::time::sleep(state.delay).await;
            if let Some(code) = state.block_status {
                return Err(Status::new(code, "Controlled node status"));
            }
            let hash = proto::BlockHash { value: vec![1; 32] };
            let time = proto::Timestamp {
                value: state.slot_millis,
            };
            let value = proto::BlockInfo {
                hash: Some(hash.clone()),
                parent_block: Some(hash.clone()),
                last_finalized_block: Some(hash),
                state_hash: Some(proto::StateHash { value: vec![2; 32] }),
                height: Some(proto::AbsoluteBlockHeight { value: 1 }),
                era_block_height: Some(proto::BlockHeight { value: 1 }),
                genesis_index: Some(proto::GenesisIndex { value: 0 }),
                receive_time: Some(time),
                arrive_time: Some(time),
                slot_time: Some(time),
                protocol_version: proto::ProtocolVersion::ProtocolVersion6 as i32,
                round: Some(proto::Round { value: 1 }),
                epoch: Some(proto::Epoch { value: 1 }),
                transactions_energy_cost: Some(proto::Energy { value: 0 }),
                finalized: true,
                ..Default::default()
            };
            let mut response = tonic::Response::new(value);
            response
                .metadata_mut()
                .insert("blockhash", "01".repeat(32).parse().unwrap());
            Ok(response)
        })
    }
}

/// This type handles a cryptographic-parameter request for the test server.
struct CryptoRpc(
    /// These settings can make the startup query fail.
    RpcState,
);
impl tonic::server::UnaryService<proto::BlockHashInput> for CryptoRpc {
    type Response = proto::CryptographicParameters;
    type Future = RpcFuture<Self::Response>;
    fn call(&mut self, request: tonic::Request<proto::BlockHashInput>) -> Self::Future {
        let state = self.0.clone();
        Box::pin(async move {
            assert!(matches!(
                request.into_inner().block_hash_input,
                Some(proto::block_hash_input::BlockHashInput::LastFinal(_))
            ));
            if let Some(code) = state.crypto_status {
                return Err(Status::new(code, "Controlled startup failure"));
            }
            let parameters = CryptographicParameters::generate_size("test-network".into(), 2);
            let mut response = tonic::Response::new(proto::CryptographicParameters {
                genesis_string: parameters.genesis_string,
                on_chain_commitment_key: to_bytes(&parameters.on_chain_commitment_key),
                bulletproof_generators: to_bytes(&parameters.bulletproof_generators),
            });
            response
                .metadata_mut()
                .insert("blockhash", "01".repeat(32).parse().unwrap());
            Ok(response)
        })
    }
}

async fn block(State(state): State<RpcState>, request: Request) -> Response {
    let response = tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(BlockRpc(state), request)
        .await;
    response.map(Body::new)
}
/// Build a controlled consensus response with distinct hashes and exact integers.
pub fn consensus_info(bft: bool) -> proto::ConsensusInfo {
    proto::ConsensusInfo {
        best_block: Some(proto::BlockHash { value: vec![1; 32] }),
        genesis_block: Some(proto::BlockHash { value: vec![2; 32] }),
        last_finalized_block: Some(proto::BlockHash { value: vec![3; 32] }),
        current_era_genesis_block: Some(proto::BlockHash { value: vec![4; 32] }),
        genesis_time: Some(proto::Timestamp {
            value: 1_767_225_600_123,
        }),
        current_era_genesis_time: Some(proto::Timestamp {
            value: 1_767_225_600_000,
        }),
        best_block_height: Some(proto::AbsoluteBlockHeight {
            value: 9_007_199_254_740_993,
        }),
        last_finalized_block_height: Some(proto::AbsoluteBlockHeight { value: 42 }),
        epoch_duration: Some(proto::Duration { value: 3_600_000 }),
        slot_duration: (!bft).then_some(proto::Duration { value: 250 }),
        protocol_version: if bft { 5 } else { 4 },
        genesis_index: Some(proto::GenesisIndex { value: 2 }),
        current_timeout_duration: bft.then_some(proto::Duration { value: 1250 }),
        current_round: bft.then_some(proto::Round {
            value: 9_007_199_254_740_995,
        }),
        current_epoch: bft.then_some(proto::Epoch { value: 8 }),
        trigger_block_time: bft.then_some(proto::Timestamp {
            value: 1_767_225_600_500,
        }),
        ..Default::default()
    }
}

/// This type serves a controlled consensus response through the SDK transport.
struct ConsensusRpc(
    /// These settings select the response or failure.
    RpcState,
);
impl tonic::server::UnaryService<proto::Empty> for ConsensusRpc {
    type Response = proto::ConsensusInfo;
    type Future = RpcFuture<Self::Response>;
    fn call(&mut self, _: tonic::Request<proto::Empty>) -> Self::Future {
        let state = self.0.clone();
        Box::pin(async move {
            tokio::time::sleep(state.delay).await;
            match state.consensus_status {
                Some(code) => Err(Status::new(code, "Controlled consensus failure")),
                None => Ok(tonic::Response::new(state.consensus)),
            }
        })
    }
}

async fn consensus(State(state): State<RpcState>, request: Request) -> Response {
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(ConsensusRpc(state), request)
        .await
        .map(Body::new)
}

async fn crypto(State(state): State<RpcState>, request: Request) -> Response {
    let response = tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(CryptoRpc(state), request)
        .await;
    response.map(Body::new)
}
