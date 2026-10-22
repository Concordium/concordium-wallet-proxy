//! This module checks CCD balances against source-derived HTTP expectations.
//! Controlled adapters verify query order, block selectors, and staking boundaries.

mod fixtures;
mod rpc;

use axum::{body::Body, http::Request};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::{
    common::to_bytes,
    id::types::AccountAddress,
    types::{AccountInfo, BakerId, BakerPoolStatus, queries::ConsensusInfo},
    v2::{BlockIdentifier, generated as p},
};
use concordium_wallet_proxy::{
    balances::{self, BalanceNode, Balances},
    dependencies::Dependencies,
    health::Health,
    http::router_with_network,
    network::{NetworkFailure, NetworkNode},
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This adapter supplies controlled account and staking responses.
#[derive(Clone)]
struct TestNode {
    /// This state is used for the pinned finalized block.
    finalized: Option<p::AccountInfo>,
    /// This state is used for the best block.
    best: Option<p::AccountInfo>,
    /// This state is used for validator and delegation-target pool queries.
    pool: Option<p::PoolInfoResponse>,
    /// This value is the zero-based protobuf protocol version.
    protocol: i32,
    /// This flag selects whether tokenomics supplies a next payday.
    payday: bool,
    /// This value supplies the reward period length in epochs.
    epochs: Option<u64>,
    /// This optional failure replaces the named operation.
    failure: Option<(&'static str, tonic::Code)>,
    /// These records capture the ordered adapter calls.
    calls: Arc<Mutex<Vec<String>>>,
}
impl Default for TestNode {
    fn default() -> Self {
        Self {
            finalized: Some(fixtures::account()),
            best: Some(fixtures::account()),
            pool: Some(fixtures::pool(Some(false), Some(true))),
            protocol: 10,
            payday: true,
            epochs: Some(2),
            failure: None,
            calls: Default::default(),
        }
    }
}

fn status(code: tonic::Code) -> NetworkFailure {
    NetworkFailure::Status(tonic::Status::new(code, "controlled").into())
}

impl TestNode {
    fn record(&self, operation: &str) -> Result<(), NetworkFailure> {
        self.calls.lock().unwrap().push(operation.into());
        if let Some((name, code)) = self.failure
            && name == operation
        {
            return Err(status(code));
        }
        Ok(())
    }
    fn block(block: BlockIdentifier) -> &'static str {
        match block {
            BlockIdentifier::Given(hash) => {
                assert_eq!(hash.to_string(), "03".repeat(32));
                "finalized"
            }
            BlockIdentifier::Best => "best",
            _ => panic!("Unexpected block selector"),
        }
    }
    fn calls(&self) -> Vec<String> {
        self.calls.lock().unwrap().clone()
    }
}
impl NetworkNode for TestNode {
    async fn consensus_info(&self) -> Result<ConsensusInfo, NetworkFailure> {
        self.record("consensus")?;
        let mut info = super::support::consensus_info(self.protocol >= 5);
        info.protocol_version = self.protocol;
        info.epoch_duration = Some(p::Duration { value: 1000 });
        Ok(info.try_into().unwrap())
    }
}
impl BalanceNode for TestNode {
    async fn account_info(
        &self,
        address: AccountAddress,
        block: BlockIdentifier,
    ) -> Result<AccountInfo, NetworkFailure> {
        assert_eq!(address, AccountAddress(fixtures::ADDRESS));
        self.record(&format!("account:{}", Self::block(block)))?;
        let account = if block == BlockIdentifier::Best {
            &self.best
        } else {
            &self.finalized
        };
        account
            .clone()
            .ok_or_else(|| status(tonic::Code::NotFound))?
            .try_into()
            .map_err(|_| NetworkFailure::InvalidResponse)
    }
    async fn pool_info(
        &self,
        baker: BakerId,
        block: BlockIdentifier,
    ) -> Result<BakerPoolStatus, NetworkFailure> {
        assert_eq!(baker.id.index, 42);
        self.record(&format!("pool:{}", Self::block(block)))?;
        self.pool
            .clone()
            .ok_or_else(|| status(tonic::Code::NotFound))?
            .try_into()
            .map_err(|_| NetworkFailure::InvalidResponse)
    }
    async fn next_payday(
        &self,
        block: BlockIdentifier,
    ) -> Result<Option<DateTime<Utc>>, NetworkFailure> {
        assert_eq!(Self::block(block), "finalized");
        self.record("tokenomics")?;
        Ok(self
            .payday
            .then(|| DateTime::from_timestamp_millis(fixtures::NEXT as i64).unwrap()))
    }
    async fn reward_period_epochs(
        &self,
        block: BlockIdentifier,
    ) -> Result<Option<u64>, NetworkFailure> {
        assert_eq!(Self::block(block), "finalized");
        self.record("parameters")?;
        Ok(self.epochs)
    }
}

/// This family deliberately has no health, database, or clock implementations.
struct TestDependencies;
impl Dependencies for TestDependencies {
    type Node = TestNode;
    type History = ();
    type Clock = ();
}

fn app<D: Dependencies + 'static>(node: D::Node) -> axum::Router
where
    D::Node: BalanceNode + 'static,
{
    let unused = Arc::new(super::catalogs::UnusedDependencies);
    let health = Health::<super::catalogs::TestDependencies> {
        node: super::catalogs::UnusedDependencies,
        history: unused.clone(),
        clock: unused,
        tolerance_seconds: 300,
    };
    router_with_network(
        health,
        std::array::from_fn(|_| json!([])),
        Default::default(),
        true,
        balances::routes(Balances::<D> { node }),
    )
}

fn address() -> String {
    AccountAddress(fixtures::ADDRESS).to_string()
}
fn path(version: u8) -> String {
    format!("/v{version}/accBalance/{}", address())
}
fn expected(version: u8) -> Value {
    let mut value: Value =
        serde_json::from_str(include_str!("fixtures/balances/current.json")).unwrap();
    if version == 0 {
        let obj = value.as_object_mut().unwrap();
        obj.remove("accountCooldowns");
        obj.remove("accountAtDisposal");
    }
    value
}
async fn body(response: axum::response::Response) -> (u16, Value) {
    let status = response.status().as_u16();
    assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
    assert_eq!(
        response.headers()["content-type"],
        "application/json; charset=utf-8"
    );
    if status >= 400 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    (
        status,
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap(),
    )
}
async fn request(node: TestNode, version: u8) -> (u16, Value) {
    body(
        app::<TestDependencies>(node)
            .oneshot(Request::get(path(version)).body(Body::empty()).unwrap())
            .await
            .unwrap(),
    )
    .await
}
fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

#[tokio::test]
async fn versions_preserve_precise_balances_and_query_order() {
    for version in [0, 1] {
        let mut node = TestNode::default();
        node.best.as_mut().unwrap().amount = Some(p::Amount {
            value: 9007199254741994,
        });
        let (status, value) = request(node.clone(), version).await;
        assert_eq!(status, 200);
        let mut expected = json!({"finalizedBalance": expected(version)});
        if version == 0 {
            expected["currentBalance"] = expected["finalizedBalance"].clone();
            expected["currentBalance"]["accountAmount"] = json!("9007199254741994");
        }
        assert_eq!(value, expected);
        assert_eq!(
            node.calls(),
            if version == 0 {
                vec![
                    "consensus",
                    "account:finalized",
                    "account:best",
                    "tokenomics",
                ]
            } else {
                vec!["consensus", "account:finalized", "tokenomics"]
            }
        );
    }
}

#[tokio::test]
async fn missing_accounts_and_pools_keep_legacy_empty_object_behavior() {
    for version in [0, 1] {
        let node = TestNode {
            finalized: None,
            ..Default::default()
        };
        assert_eq!(request(node.clone(), version).await, (200, json!({})));
        assert_eq!(node.calls(), ["consensus", "account:finalized"]);
    }
    let node = TestNode {
        best: None,
        ..Default::default()
    };
    assert_eq!(request(node.clone(), 0).await, (200, json!({})));
    assert_eq!(
        node.calls(),
        ["consensus", "account:finalized", "account:best"]
    );
    assert_eq!(request(node, 1).await.0, 200);
    for block in ["pool:finalized", "pool:best"] {
        let mut node = TestNode {
            failure: Some((block, tonic::Code::NotFound)),
            ..Default::default()
        };
        node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(Some(42), None));
        node.best.as_mut().unwrap().stake = Some(fixtures::delegation(Some(42), None));
        assert_eq!(request(node.clone(), 0).await, (200, json!({})));
        assert!(!node.calls().contains(&"tokenomics".into()));
    }
}

#[tokio::test]
async fn empty_and_maximum_balances_keep_required_fields() {
    for amount in [0, u64::MAX] {
        let mut account = fixtures::account();
        account.amount = Some(p::Amount { value: amount });
        account.available_balance = None;
        account.cooldowns.clear();
        account.schedule = Some(p::ReleaseSchedule {
            total: Some(p::Amount { value: 0 }),
            schedules: vec![],
        });
        for version in [0, 1] {
            let node = TestNode {
                finalized: Some(account.clone()),
                best: Some(account.clone()),
                ..Default::default()
            };
            let mut balance = expected(version);
            balance["accountAmount"] = json!(amount.to_string());
            balance["accountReleaseSchedule"] = json!({"total":"0", "schedule":[]});
            if version == 1 {
                balance["accountCooldowns"] = json!([]);
                balance["accountAtDisposal"] = json!(amount.to_string());
            }
            let mut response = json!({"finalizedBalance":balance});
            if version == 0 {
                response["currentBalance"] = response["finalizedBalance"].clone();
            }
            assert_eq!(request(node, version).await, (200, response));
        }
    }
}

#[tokio::test]
async fn encrypted_aggregation_is_prepended_and_count_is_optional() {
    let mut node = TestNode::default();
    let raw = "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb";
    let point: Vec<u8> = (0..raw.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&raw[i..i + 2], 16).unwrap())
        .collect();
    let aggregated = p::EncryptedAmount {
        value: point.repeat(4),
    };
    let incoming = fixtures::encrypted();
    let encrypted = node
        .finalized
        .as_mut()
        .unwrap()
        .encrypted_balance
        .as_mut()
        .unwrap();
    encrypted.aggregated_amount = Some(aggregated.clone());
    encrypted.num_aggregated = Some(3);
    encrypted.incoming_amounts = vec![incoming.clone()];
    let mut expected = expected(1);
    expected["accountEncryptedAmount"]["incomingAmounts"] =
        json!([hex(&aggregated.value), hex(&incoming.value)]);
    expected["accountEncryptedAmount"]["numAggregated"] = json!(3);
    assert_eq!(
        request(node, 1).await,
        (200, json!({"finalizedBalance":expected}))
    );
}

#[tokio::test]
async fn available_balance_uses_node_value_or_sdk_legacy_lock_calculation() {
    for (release, staking, available, result) in [
        (4000, false, None, "9007199254737993"),
        (100, true, None, "9007199254739393"),
        (4000, true, Some(7), "7"),
    ] {
        let mut node = TestNode::default();
        let account = node.finalized.as_mut().unwrap();
        account.schedule.as_mut().unwrap().total = Some(p::Amount { value: release });
        account.available_balance = available.map(|value| p::Amount { value });
        if staking {
            account.stake = Some(fixtures::delegation(None, None));
        }
        let (status, value) = request(node, 1).await;
        assert_eq!(status, 200);
        assert_eq!(value["finalizedBalance"]["accountAtDisposal"], result);
    }
}

#[tokio::test]
async fn validator_fields_and_pool_suspension_are_independent() {
    for version in [0, 1] {
        for flag in [None, Some(false), Some(true)] {
            let mut node = TestNode {
                pool: Some(fixtures::pool(Some(false), flag)),
                ..Default::default()
            };
            node.finalized.as_mut().unwrap().stake = Some(fixtures::validator(None));
            let keys = fixtures::keys();
            let mut expected = expected(version);
            expected["accountBaker"] = json!({"stakedAmount":"2000", "restakeEarnings":false,"bakerId":42,
                "bakerElectionVerifyKey":hex(&to_bytes(&keys.election_verify)), "bakerSignatureVerifyKey":hex(&to_bytes(&keys.signature_verify)),"bakerAggregationVerifyKey":hex(&to_bytes(&keys.aggregation_verify)), "isSuspended":true,
                "bakerPoolInfo":{"openStatus":"openForAll","metadataUrl":"https://pool.example","commissionRates":{"finalizationCommission":0.1,"bakingCommission":0.2,"transactionCommission":0.3}}
            });
            if let Some(flag) = flag {
                expected["accountBaker"]["isPrimedForSuspension"] = json!(flag);
            }
            let mut response = json!({"finalizedBalance":expected});
            if version == 0 {
                response["currentBalance"] = self::expected(0);
            }
            assert_eq!(request(node.clone(), version).await, (200, response));
            assert!(node.calls().contains(&"pool:finalized".into()));
        }
    }
}

#[tokio::test]
async fn legacy_validator_omits_pool_metadata_and_reports_false_suspension() {
    let mut node = TestNode {
        pool: Some(fixtures::pool(None, None)),
        ..Default::default()
    };
    let mut stake = fixtures::validator(None);
    let Some(p::account_staking_info::StakingInfo::Baker(baker)) = stake.staking_info.as_mut()
    else {
        unreachable!()
    };
    baker.pool_info = None;
    baker.is_suspended = false;
    node.finalized.as_mut().unwrap().stake = Some(stake);
    let (status, value) = request(node, 0).await;
    assert_eq!(status, 200);
    let baker = &value["finalizedBalance"]["accountBaker"];
    assert_eq!(baker["isSuspended"], false);
    assert!(baker.get("bakerPoolInfo").is_none());
    assert!(baker.get("isPrimedForSuspension").is_none());
}

#[tokio::test]
async fn delegation_handles_passive_active_removed_and_optional_pool_flags() {
    for target in [None, Some(42)] {
        for suspended in [None, Some(false), Some(true)] {
            for primed in [None, Some(false), Some(true)] {
                let mut node = TestNode {
                    pool: Some(fixtures::pool(suspended, primed)),
                    ..Default::default()
                };
                node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(target, None));
                let mut expected = expected(1);
                expected["accountDelegation"] = json!({"stakedAmount":"2000","restakeEarnings":true,"delegationTarget": if target.is_some() {json!({"delegateType":"Baker","bakerId":42})} else {json!({"delegateType":"Passive"})}});
                if target.is_some() {
                    if let Some(flag) = suspended {
                        expected["accountDelegation"]["isSuspended"] = json!(flag);
                    }
                    if let Some(flag) = primed {
                        expected["accountDelegation"]["isPrimedForSuspension"] = json!(flag);
                    }
                }
                assert_eq!(
                    request(node.clone(), 1).await,
                    (200, json!({"finalizedBalance":expected}))
                );
                assert_eq!(
                    node.calls().contains(&"pool:finalized".into()),
                    target.is_some()
                );
            }
        }
    }
    let mut node = TestNode::default();
    node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(Some(42), None));
    let pool = node.pool.as_mut().unwrap();
    pool.pool_info = None;
    pool.equity_capital = None;
    pool.delegated_capital = None;
    pool.delegated_capital_cap = None;
    let (status, value) = request(node, 1).await;
    assert_eq!(status, 200);
    assert_eq!(
        value["finalizedBalance"]["accountDelegation"]["isPrimedForSuspension"],
        true
    );
    assert!(
        value["finalizedBalance"]["accountDelegation"]
            .get("isSuspended")
            .is_none()
    );
}

#[tokio::test]
async fn pending_changes_estimate_first_payday_at_or_after_cooldown_end() {
    for (offset, time) in [
        (-1_i64, "2026-01-01T00:00:00Z"),
        (0, "2026-01-01T00:00:00Z"),
        (1, "2026-01-01T00:00:02Z"),
        (2000, "2026-01-01T00:00:02Z"),
        (2001, "2026-01-01T00:00:04Z"),
    ] {
        for remove in [false, true] {
            let mut node = TestNode::default();
            node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(
                None,
                Some(fixtures::pending(
                    remove,
                    (fixtures::NEXT as i64 + offset) as u64,
                )),
            ));
            let (status, value) = request(node.clone(), 1).await;
            assert_eq!(status, 200);
            let pending = &value["finalizedBalance"]["accountDelegation"]["pendingChange"];
            assert_eq!(pending["estimatedChangeTime"], time);
            assert_eq!(
                pending["change"],
                if remove { "RemoveStake" } else { "ReduceStake" }
            );
            assert_eq!(
                pending.get("newStake").cloned(),
                if remove { None } else { Some(json!("1000")) }
            );
            assert_eq!(
                node.calls(),
                ["consensus", "account:finalized", "tokenomics", "parameters"]
            );
        }
    }
    for (payday, epochs) in [(false, Some(2)), (true, None)] {
        let mut node = TestNode {
            payday,
            epochs,
            ..Default::default()
        };
        node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(
            None,
            Some(fixtures::pending(true, fixtures::NEXT + 100)),
        ));
        let (status, value) = request(node, 1).await;
        assert_eq!(status, 200);
        assert_eq!(
            value["finalizedBalance"]["accountDelegation"]["pendingChange"],
            json!({"change":"RemoveStake","effectiveTime":"2026-01-01T00:00:00.1Z"})
        );
    }
    let mut node = TestNode::default();
    node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(
        None,
        Some(fixtures::pending(true, fixtures::NEXT)),
    ));
    node.best.as_mut().unwrap().stake = Some(fixtures::validator(Some(fixtures::pending(
        false,
        fixtures::NEXT + 2001,
    ))));
    let (status, value) = request(node.clone(), 0).await;
    assert_eq!(status, 200);
    assert_eq!(
        value["currentBalance"]["accountBaker"]["pendingChange"]["estimatedChangeTime"],
        "2026-01-01T00:00:04Z"
    );
    assert_eq!(
        node.calls(),
        [
            "consensus",
            "account:finalized",
            "account:best",
            "pool:best",
            "tokenomics",
            "parameters"
        ]
    );
}

#[tokio::test]
async fn failures_and_invalid_addresses_preserve_http_contracts() {
    let node = TestNode::default();
    let response = app::<TestDependencies>(node.clone())
        .oneshot(
            Request::get("/v1/accBalance/invalid")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(
        body(response).await,
        (
            400,
            json!({"error":1,"errorMessage":"Malformed account address"})
        )
    );
    assert!(node.calls().is_empty());
    for operation in [
        "consensus",
        "account:finalized",
        "pool:finalized",
        "account:best",
        "pool:best",
        "tokenomics",
        "parameters",
    ] {
        let mut node = TestNode {
            failure: Some((operation, tonic::Code::Unavailable)),
            ..Default::default()
        };
        node.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(
            Some(42),
            Some(fixtures::pending(true, fixtures::NEXT)),
        ));
        node.best.as_mut().unwrap().stake = Some(fixtures::delegation(Some(42), None));
        assert_eq!(
            request(node, 0).await,
            (
                502,
                json!({"error":0,"errorMessage":"GRPC call failed: Non-OK GRPC status code 'UNAVAILABLE'."})
            )
        );
    }
    let node = TestNode {
        failure: Some(("tokenomics", tonic::Code::NotFound)),
        ..Default::default()
    };
    assert_eq!(
        request(node, 1).await,
        (
            404,
            json!({"error":2,"errorMessage":"GRPC call failed: Requested object was not found: controlled"})
        )
    );
}

#[tokio::test]
async fn unsupported_staking_and_cooldown_variants_are_not_silently_omitted() {
    for version in [0, 1] {
        let mut node = TestNode::default();
        node.finalized.as_mut().unwrap().stake = Some(p::AccountStakingInfo::default());
        assert_eq!(request(node.clone(), version).await.0, 500);
        assert_eq!(node.calls(), ["consensus", "account:finalized"]);
        let mut node = TestNode::default();
        node.finalized.as_mut().unwrap().cooldowns[0].status = 99;
        assert_eq!(request(node.clone(), version).await.0, 500);
        assert_eq!(node.calls(), ["consensus", "account:finalized"]);
    }
}

#[tokio::test]
async fn shared_http_behavior_and_token_version_scope() {
    let app = app::<TestDependencies>(TestNode::default());
    let response = app
        .clone()
        .oneshot(
            Request::get(format!("{}?unused=true", path(1)))
                .header("origin", "https://wallet.example")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(response.headers()["access-control-allow-origin"], "*");
    let response = app
        .clone()
        .oneshot(Request::head(path(1)).body(Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert!(
        response
            .into_body()
            .collect()
            .await
            .unwrap()
            .to_bytes()
            .is_empty()
    );
    for (method, url, status) in [("POST", path(1), 405), ("GET", path(2), 404)] {
        let response = app
            .clone()
            .oneshot(
                Request::builder()
                    .method(method)
                    .uri(url)
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(response.status(), status);
    }
}
