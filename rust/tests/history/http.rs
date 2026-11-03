//! These tests check history request ordering, canonical addresses, and HTTP failures.
//! Expectations are derived from the legacy handler rather than replacement responses.

use axum::{Router, body::Body};
use concordium_rust_sdk::id::types::AccountAddress;
use concordium_wallet_proxy::{
    history::{
        self, AccountHistory, HistoryEntry, HistoryNode, HistoryOrder, HistoryQuery,
        HistoryVersion, RewardFilter,
    },
    network::NetworkFailure,
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This source fixture account sends the transaction.
pub(super) const SENDER: &str = "2xBpaHottqhwFZURMZW4uZduQvpxNDSy46iXMYs9kceNGaPpZX";
/// This source fixture account receives the transaction.
pub(super) const RECEIVER: &str = "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3";

/// This adapter records canonical lookups and returns a controlled result.
pub(super) struct Node {
    /// This address replaces the supplied alias when lookup succeeds.
    pub canonical: AccountAddress,
    /// This status can distinguish absence from a transport or server failure.
    pub failure: Option<tonic::Code>,
    /// These addresses retain every lookup input.
    pub calls: Mutex<Vec<AccountAddress>>,
}
impl HistoryNode for Node {
    async fn canonical_address(
        &self,
        address: AccountAddress,
    ) -> Result<AccountAddress, NetworkFailure> {
        self.calls.lock().unwrap().push(address);
        if let Some(code) = self.failure {
            return Err(NetworkFailure::Status(
                tonic::Status::new(code, "controlled").into(),
            ));
        }
        Ok(self.canonical)
    }
}

/// This adapter records typed filters and supplies hand-written persisted JSON.
#[derive(Default)]
pub(super) struct Database {
    /// These summaries form a controlled selected page.
    pub summaries: Vec<Value>,
    /// This timestamp can test the full unsigned persisted range.
    pub timestamp: Option<u64>,
    /// This flag injects a database exception.
    pub failure: bool,
    /// These values retain every canonical account and query.
    pub calls: Mutex<Vec<(AccountAddress, HistoryQuery)>>,
}
impl AccountHistory for Database {
    async fn account_history(
        &self,
        account: AccountAddress,
        query: &HistoryQuery,
    ) -> anyhow::Result<Vec<HistoryEntry>> {
        self.calls.lock().unwrap().push((account, query.clone()));
        anyhow::ensure!(!self.failure, "Controlled database exception");
        Ok(self
            .summaries
            .iter()
            .enumerate()
            .map(|(index, summary)| HistoryEntry {
                id: index as i64 + 7,
                block: [3; 32].into(),
                timestamp: self.timestamp.unwrap_or(1234),
                height: 9_007_199_254_740_993,
                summary: summary.clone(),
            })
            .collect())
    }
}

/// Build a complete legacy supplemented summary with a source-defined event list.
pub(super) fn summary(kind: &str, events: Vec<Value>) -> Value {
    json!({"Left":{"sender":SENDER,"hash":"04".repeat(32),"cost":"10","energyCost":18446744073709551615_u64,"type":{"type":"accountTransaction","contents":kind},"index":1,"result":{"outcome":"success","events":events}}})
}

/// Build the basic legacy transfer event with tagged addresses.
pub(super) fn transfer(from: &str, to: &str, amount: &str) -> Value {
    json!({"tag":"Transferred","from":{"type":"AddressAccount","address":from},"to":{"type":"AddressAccount","address":to},"amount":amount})
}

/// Build adapters for one selected persisted page.
pub(super) fn adapters(summaries: Vec<Value>) -> (Arc<Node>, Arc<Database>) {
    (
        Arc::new(Node {
            canonical: AccountAddress([1; 32]),
            failure: None,
            calls: Mutex::new(vec![]),
        }),
        Arc::new(Database {
            summaries,
            ..Default::default()
        }),
    )
}

/// Read a history response and enforce the language header.
pub(super) async fn request(router: Router, path: &str) -> (u16, Value) {
    let response = router
        .oneshot(::http::Request::get(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(response.headers()["content-language"], "en");
    let status = response.status().as_u16();
    let bytes = response.into_body().collect().await.unwrap().to_bytes();
    (status, serde_json::from_slice(&bytes).unwrap())
}

/// Read one response entry from a controlled adapter.
pub(super) async fn entry(summary: Value, version: u8, address: &str, query: &str) -> Value {
    let (mut node, database) = adapters(vec![summary]);
    Arc::get_mut(&mut node).unwrap().canonical = address.parse().unwrap();
    let (status, body) = request(
        history::routes(node, database),
        &format!("/v{version}/accTransactions/{address}{query}"),
    )
    .await;
    assert_eq!(status, 200, "{body}");
    body["transactions"][0].clone()
}

#[tokio::test]
async fn canonical_lookup_precedes_validation_and_not_found_retains_the_given_address() {
    let (node, database) = adapters(vec![]);
    let router = history::routes(node.clone(), database.clone());
    assert_eq!(
        request(router.clone(), "/v0/accTransactions/bad?onlyEncrypted=bad")
            .await
            .0,
        400
    );
    assert!(node.calls.lock().unwrap().is_empty());
    assert_eq!(
        request(
            router.clone(),
            &format!("/v0/accTransactions/{RECEIVER}?onlyEncrypted=bad")
        )
        .await,
        (
            400,
            json!({"error":1,"errorMessage":"Parsing input failed: Unsupported 'onlyEncrypted' parameter."})
        )
    );
    assert_eq!(*node.calls.lock().unwrap(), [AccountAddress([0; 32])]);
    assert!(database.calls.lock().unwrap().is_empty());
    assert_eq!(
        request(router, &format!("/v0/accTransactions/{RECEIVER}")).await,
        (
            200,
            json!({"limit":20,"order":"ascending","count":0,"transactions":[]})
        )
    );
    assert_eq!(database.calls.lock().unwrap()[0].0, AccountAddress([1; 32]));
    for (code, expected) in [
        (tonic::Code::NotFound, 200),
        (tonic::Code::Unavailable, 502),
        (tonic::Code::ResourceExhausted, 503),
    ] {
        let (mut node, database) = adapters(vec![]);
        Arc::get_mut(&mut node).unwrap().failure = Some(code);
        let status = request(
            history::routes(node, database.clone()),
            &format!("/v2/accTransactions/{RECEIVER}"),
        )
        .await
        .0;
        assert_eq!(status, expected);
        if code == tonic::Code::NotFound {
            assert_eq!(database.calls.lock().unwrap()[0].0, AccountAddress([0; 32]));
        } else {
            assert!(database.calls.lock().unwrap().is_empty());
        }
    }
}

#[tokio::test]
async fn query_defaults_integer_syntax_duplicates_and_filter_error_order_match_legacy() {
    let (node, database) = adapters(vec![]);
    let router = history::routes(node, database.clone());
    for version in 0..4 {
        let (_, body) = request(router.clone(), &format!("/v{version}/accTransactions/{SENDER}?order=DESC&from=3&limit=0x20&limit=1&blockTimeFrom=(2)&blockTimeTo=-1&blockHeightFrom=0o10&blockHeightTo=20&includeRewards=none&blockRewards=n&finalizationRewards=n&bakingRewards=n&onlyEncrypted=y")).await;
        assert_eq!(
            body,
            json!({"limit":32,"order":"descending","from":3,"count":0,"transactions":[]})
        );
        let calls = database.calls.lock().unwrap();
        let query = &calls.last().unwrap().1;
        assert_eq!(
            query.version,
            [
                HistoryVersion::V0,
                HistoryVersion::V1,
                HistoryVersion::V2,
                HistoryVersion::V3
            ][version as usize]
        );
        assert_eq!(query.time_from, Some(2000));
        assert_eq!(query.time_to, Some(u64::MAX.wrapping_mul(1000)));
        assert_eq!(query.height_from, Some(8));
        assert_eq!(query.height_to, Some(20));
        assert_eq!(query.rewards, RewardFilter::None);
        assert!(
            query.exclude_block_rewards
                && query.exclude_finalization_rewards
                && query.exclude_baking_rewards
                && query.only_encrypted
        );
        assert_eq!(query.order, HistoryOrder::Descending);
    }
    for (query, limit) in [
        ("limit=bad&from=bad", 20),
        ("limit=-1&from=%20", 0),
        ("limit=1001", 1000),
        ("order=other&from=0x10", 20),
    ] {
        let (_, body) = request(
            router.clone(),
            &format!("/v0/accTransactions/{SENDER}?{query}"),
        )
        .await;
        assert_eq!(body["limit"], limit);
        assert!(body.get("from").is_none());
    }
    let names = [
        ("blockTimeFrom", "blockTimeFrom"),
        ("blockTimeTo", "blockTimeTo"),
        ("blockRewards", "blockReward"),
        ("finalizationRewards", "finalizationReward"),
        ("bakingRewards", "bakingReward"),
        ("onlyEncrypted", "onlyEncrypted"),
        ("includeRewards", "includeRewards"),
        ("blockHeightFrom", "blockHeightFrom"),
        ("blockHeightTo", "blockHeightTo"),
    ];
    for index in 0..names.len() {
        let query = names[index..]
            .iter()
            .map(|(name, _)| format!("{name}=bad"))
            .collect::<Vec<_>>()
            .join("&");
        assert_eq!(
            request(
                router.clone(),
                &format!("/v0/accTransactions/{SENDER}?{query}")
            )
            .await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: Unsupported '{}' parameter.",names[index].1)})
            )
        );
    }
}

#[tokio::test]
async fn decode_and_query_failures_never_return_partial_history() {
    let good = summary("transfer", vec![transfer(SENDER, RECEIVER, "1")]);
    for bad in [
        json!({}),
        json!({"Left":{}}),
        json!({"Right":{"tag":"UnknownReward"}}),
        summary("transfer", vec![json!({"tag":"UnknownEvent"})]),
    ] {
        let (node, database) = adapters(vec![good.clone(), bad]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v0/accTransactions/{SENDER}")
            )
            .await,
            (502, json!({"error":0,"errorMessage":"Database error"}))
        );
    }
    let (node, mut database) = adapters(vec![good]);
    Arc::get_mut(&mut database).unwrap().failure = true;
    assert_eq!(
        request(
            history::routes(node, database),
            &format!("/v0/accTransactions/{SENDER}")
        )
        .await,
        (
            500,
            json!({"error":0,"errorMessage":"Internal server error"})
        )
    );
}
