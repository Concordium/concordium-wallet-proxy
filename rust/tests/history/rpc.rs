//! These tests exercise canonical lookup through local SDK transport and real PostgreSQL.
//! Only account lookup is registered on the node; no submission or unrelated query can succeed.

use super::http::*;
use crate::{balances::fixtures, health, support};
use axum::{
    Router,
    body::Body,
    extract::{Request, State},
    response::Response,
    routing::post,
};
use concordium_rust_sdk::{id::types::AccountAddress, v2::generated as p};
use concordium_wallet_proxy::{
    config::NodeConfig,
    database::PostgresHistory,
    history::{self, HistoryNode},
    node::SdkNode,
};
use serde_json::{Value, json};
use sqlx::PgPool;
use std::{
    future::{Ready, ready},
    sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

/// This state records node queries and selects the canonical-address response.
#[derive(Default)]
struct RpcState {
    /// This code can fail account lookup before any database read.
    failure: Option<tonic::Code>,
    /// This flag delays the node beyond the shared query deadline.
    delayed: bool,
    /// This flag removes required account information from the response.
    malformed: bool,
    /// These protobuf requests retain account and block selectors.
    requests: Mutex<Vec<p::AccountInfoRequest>>,
    /// This counter records requests even when the response is delayed.
    arrivals: AtomicUsize,
}
/// This service handles one account-information RPC.
struct Account(
    /// This shared state selects the response and records input.
    Arc<RpcState>,
);
impl tonic::server::UnaryService<p::AccountInfoRequest> for Account {
    type Response = p::AccountInfo;
    type Future = Ready<Result<tonic::Response<Self::Response>, tonic::Status>>;
    fn call(&mut self, request: tonic::Request<p::AccountInfoRequest>) -> Self::Future {
        self.0.requests.lock().unwrap().push(request.into_inner());
        ready(match self.0.failure {
            Some(code) => Err(tonic::Status::new(code, "controlled")),
            None => {
                let mut response = tonic::Response::new(if self.0.malformed {
                    p::AccountInfo::default()
                } else {
                    fixtures::account()
                });
                response
                    .metadata_mut()
                    .insert("blockhash", "03".repeat(32).parse().unwrap());
                Ok(response)
            }
        })
    }
}
async fn dispatch(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    state.arrivals.fetch_add(1, Ordering::SeqCst);
    if state.delayed {
        tokio::time::sleep(std::time::Duration::from_millis(1100)).await;
    }
    tonic::server::Grpc::new(tonic::codec::ProstCodec::default())
        .unary(Account(state), request)
        .await
        .map(Body::new)
}
async fn sdk(state: Arc<RpcState>) -> (support::Server, Arc<SdkNode>) {
    let server = support::serve_router(
        Router::new()
            .route("/concordium.v2.Queries/GetAccountInfo", post(dispatch))
            .with_state(state),
    )
    .await;
    let mut config = NodeConfig::new(server.endpoint.clone());
    config.timeout_seconds = 1;
    let node = Arc::new(SdkNode::connect(&config).await.unwrap());
    (server, node)
}

#[tokio::test]
async fn sdk_history_uses_last_final_and_keeps_absence_transport_conversion_and_timeout_distinct() {
    for (code, malformed, delayed) in [
        (None, false, false),
        (Some(tonic::Code::NotFound), false, false),
        (Some(tonic::Code::Unavailable), false, false),
        (None, true, false),
        (None, false, true),
    ] {
        let state = Arc::new(RpcState {
            failure: code,
            malformed,
            delayed,
            ..Default::default()
        });
        let (_server, node) = sdk(state.clone()).await;
        let result = node.canonical_address(AccountAddress([0; 32])).await;
        match code {
            Some(code) => assert!(
                matches!(result.unwrap_err(), concordium_wallet_proxy::network::NetworkFailure::Status(status) if status.code() == code)
            ),
            None if malformed => assert!(
                matches!(result.unwrap_err(), concordium_wallet_proxy::network::NetworkFailure::Status(status) if status.code() != tonic::Code::NotFound)
            ),
            None if delayed => assert!(
                matches!(result.unwrap_err(), concordium_wallet_proxy::network::NetworkFailure::Status(status) if status.code() == tonic::Code::DeadlineExceeded)
            ),
            _ => assert_eq!(result.unwrap(), AccountAddress([1; 32])),
        }
        assert_eq!(state.arrivals.load(Ordering::SeqCst), 1);
        if !delayed {
            let requests = state.requests.lock().unwrap();
            let request = &requests[0];
            assert!(matches!(
                request.block_hash.as_ref().unwrap().block_hash_input,
                Some(p::block_hash_input::BlockHashInput::LastFinal(_))
            ));
            assert_eq!(
                request
                    .account_identifier
                    .as_ref()
                    .unwrap()
                    .account_identifier_input,
                Some(
                    p::account_identifier_input::AccountIdentifierInput::Address(
                        p::AccountAddress { value: vec![0; 32] }
                    )
                )
            );
        }
    }
}

#[sqlx::test]
#[ignore = "requires disposable PostgreSQL and a DATABASE_URL with CREATE DATABASE permission"]
async fn real_http_history_joins_legacy_rows_after_sdk_alias_lookup_and_preserves_metadata(
    pool: PgPool,
) {
    super::schema(&pool).await;
    super::insert(
        &pool,
        1,
        AccountAddress([1; 32]),
        summary("transfer", vec![transfer(SENDER, SENDER, "100")]),
    )
    .await;
    let mut reject = summary("transfer", vec![]);
    reject["Left"]["result"] = json!({"outcome":"reject","rejectReason":{"tag":"OutOfEnergy"}});
    reject["Left"]["sponsorDetails"] = json!({"sponsor":SENDER,"cost":"7"});
    super::insert(&pool, 2, AccountAddress([1; 32]), reject).await;
    super::insert(
        &pool,
        3,
        AccountAddress([1; 32]),
        json!({"Right":{"tag":"ValidatorSuspended","bakerId":42,"account":SENDER}}),
    )
    .await;
    super::insert(
        &pool,
        4,
        AccountAddress([1; 32]),
        summary("tokenUpdate", vec![]),
    )
    .await;
    let state = Arc::new(RpcState::default());
    let (_rpc, node) = sdk(state.clone()).await;
    let routes = history::routes(node, Arc::new(PostgresHistory(pool.clone())));
    let (app, health_calls) = health::app_with_routes("missing", false, true, routes);
    let server = support::serve_router(app).await;
    let client = reqwest::Client::builder()
        .no_proxy()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .unwrap();
    let url = format!("{}/v0/accTransactions/{RECEIVER}", server.endpoint);
    let response = client
        .get(format!("{url}?lang=da&includeRawRejectReason=n"))
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
    let body = response.json::<Value>().await.unwrap();
    assert_eq!(body["count"], 2);
    assert_eq!(body["transactions"][0]["subtotal"], "0");
    assert_eq!(body["transactions"][0]["total"], "-10");
    assert_eq!(body["transactions"][1]["cost"], "7");
    assert_eq!(body["transactions"][1]["total"], "-7");
    assert_eq!(
        body["transactions"][1]["details"]["rawRejectReason"],
        json!({"tag":"OutOfEnergy"})
    );
    let response = client
        .get(format!(
            "{}/v2/accTransactions/{RECEIVER}?includeRewards=none&order=d&from=4",
            server.endpoint
        ))
        .send()
        .await
        .unwrap()
        .json::<Value>()
        .await
        .unwrap();
    assert_eq!(response["count"], 3);
    assert_eq!(
        response["transactions"][0]["details"]["type"],
        "validatorSuspended"
    );
    let page = client
        .get(format!("{url}?order=d&from=3&limit=1"))
        .send()
        .await
        .unwrap()
        .json::<Value>()
        .await
        .unwrap();
    assert_eq!(page["count"], 1);
    assert_eq!(page["transactions"][0]["id"], 2);
    let response = client.head(&url).send().await.unwrap();
    assert_eq!(response.status(), 200);
    assert!(response.bytes().await.unwrap().is_empty());
    let calls = state.arrivals.load(Ordering::SeqCst);
    assert_eq!(client.post(&url).send().await.unwrap().status(), 405);
    assert_eq!(
        client.get(format!("{url}/")).send().await.unwrap().status(),
        301
    );
    assert_eq!(state.arrivals.load(Ordering::SeqCst), calls);
    assert_eq!(health_calls.load(Ordering::SeqCst), 0);
    super::insert(&pool, 5, AccountAddress([1; 32]), json!({"Left":{}})).await;
    let response = client.get(&url).send().await.unwrap();
    assert_eq!(response.status(), 502);
    assert_eq!(
        response.json::<Value>().await.unwrap(),
        json!({"error":0,"errorMessage":"Database error"})
    );
    sqlx::raw_sql("DROP TABLE ati")
        .execute(&pool)
        .await
        .unwrap();
    assert_eq!(client.get(&url).send().await.unwrap().status(), 500);
}
