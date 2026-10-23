//! This module checks staking HTTP contracts with controlled node responses.
//! Source-derived fixtures cover chain-parameter versions and pending-change boundaries.

pub(super) mod fixtures;
mod rpc;

use super::{balances::fixtures as balances, support};
use axum::{Router, body::Body, http::Request};
use chrono::TimeDelta;
use concordium_rust_sdk::{
    types::{
        BakerId, BakerPoolStatus, PassiveDelegationStatus, RewardsOverview, hashes::BlockHash,
        queries::ConsensusInfo,
    },
    v2::{BlockIdentifier, generated as p},
};
use concordium_wallet_proxy::{
    network::{NetworkFailure, NetworkNode},
    staking::{self, Conditions, StakingNode},
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use tower::ServiceExt;

/// This type selects the source of a controlled node failure.
#[derive(Clone, Copy)]
enum Failure {
    /// The connection failed.
    Transport,
    /// The response could not be decoded.
    Invalid,
    /// The node returned this RPC status.
    Status(
        /// This code identifies the controlled RPC failure.
        tonic::Code,
    ),
}

/// This adapter records operations and returns controlled protocol variants.
struct TestNode {
    /// This version selects the chain-parameter fixture.
    version: u8,
    /// This response controls active status, pending changes, and suspension.
    pool: p::PoolInfoResponse,
    /// This flag selects the payday-bearing reward status.
    payday: bool,
    /// This duration controls pending-change rounding without wall-clock time.
    epoch_millis: i64,
    /// These operations record query order and block selection.
    calls: Mutex<Vec<String>>,
    /// This operation fails before returning its response.
    fail_at: Option<&'static str>,
    /// This failure applies at the selected operation.
    failure: Failure,
}
impl Default for TestNode {
    fn default() -> Self {
        Self {
            version: 1,
            pool: balances::pool(None, None),
            payday: true,
            epoch_millis: 1000,
            calls: Mutex::new(vec![]),
            fail_at: None,
            failure: Failure::Status(tonic::Code::NotFound),
        }
    }
}
impl TestNode {
    fn record(&self, operation: &str) -> Result<(), NetworkFailure> {
        self.calls.lock().unwrap().push(operation.to_string());
        if self.fail_at == Some(operation) {
            return Err(match self.failure {
                Failure::Transport => NetworkFailure::Transport,
                Failure::Invalid => NetworkFailure::InvalidResponse,
                Failure::Status(code) => {
                    NetworkFailure::Status(tonic::Status::new(code, "controlled").into())
                }
            });
        }
        Ok(())
    }
}
fn block(block: BlockIdentifier) -> &'static str {
    match block {
        BlockIdentifier::LastFinal => "lastFinal",
        BlockIdentifier::Given(hash) => {
            assert_eq!(hash, BlockHash::from([3; 32]));
            "given"
        }
        _ => panic!("Unexpected selector"),
    }
}
impl NetworkNode for TestNode {
    async fn consensus_info(&self) -> Result<ConsensusInfo, NetworkFailure> {
        self.record("consensus")?;
        let mut info: ConsensusInfo = support::consensus_info(false).try_into().unwrap();
        info.epoch_duration = TimeDelta::milliseconds(self.epoch_millis);
        Ok(info)
    }
}
impl StakingNode for TestNode {
    async fn pool_status(
        &self,
        baker: BakerId,
    ) -> Result<(BlockHash, BakerPoolStatus), NetworkFailure> {
        self.record(&format!("pool:{}", baker.id.index))?;
        Ok(([3; 32].into(), self.pool.clone().try_into().unwrap()))
    }
    async fn passive_status(&self) -> Result<PassiveDelegationStatus, NetworkFailure> {
        self.record("passive")?;
        Ok(passive_proto().try_into().unwrap())
    }
    async fn conditions(&self, selector: BlockIdentifier) -> Result<Conditions, NetworkFailure> {
        self.record(&format!("conditions:{}", block(selector)))?;
        Ok(Conditions {
            parameters: fixtures::chain(self.version).try_into().unwrap(),
            foundation_index: 42,
        })
    }
    async fn rewards(&self, selector: BlockIdentifier) -> Result<RewardsOverview, NetworkFailure> {
        self.record(&format!("rewards:{}", block(selector)))?;
        Ok(
            balances::tokenomics(self.payday, if self.payday { 4 } else { 3 })
                .try_into()
                .unwrap(),
        )
    }
}
fn passive_proto() -> p::PassiveDelegationInfo {
    p::PassiveDelegationInfo {
        delegated_capital: Some(p::Amount { value: 100 }),
        commission_rates: balances::pool_metadata().commission_rates,
        current_payday_transaction_fees_earned: Some(p::Amount { value: 0 }),
        current_payday_delegated_capital: Some(p::Amount { value: 2000 }),
        all_pool_total_capital: Some(p::Amount { value: u64::MAX }),
    }
}
async fn request(router: Router, target: &str) -> (u16, Value) {
    let response = router
        .oneshot(Request::builder().uri(target).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    if status != 200 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    (
        status,
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap(),
    )
}
fn expected(name: &str) -> Value {
    serde_json::from_str::<Value>(include_str!("fixtures/staking.json")).unwrap()[name].clone()
}
fn expected_chain(version: u8) -> Value {
    let mut value = expected("chain0");
    if version >= 1 {
        let obj = value.as_object_mut().unwrap();
        obj.remove("bakerCooldownEpochs");
        obj.remove("minimumThresholdForBaking");
        obj.extend(expected("delegationFields").as_object().unwrap().clone());
        value["rewardParameters"]["mintDistribution"]
            .as_object_mut()
            .unwrap()
            .remove("mintPerSlot");
    }
    if version >= 2 {
        value.as_object_mut().unwrap().remove("electionDifficulty");
        value["rewardParameters"]["gASRewards"]
            .as_object_mut()
            .unwrap()
            .remove("finalizationProof");
        value
            .as_object_mut()
            .unwrap()
            .extend(expected("bftFields").as_object().unwrap().clone());
    }
    if version == 3 {
        value["maximumMissedRounds"] = json!(u64::MAX);
    }
    value
}

#[tokio::test]
async fn chain_parameter_versions_preserve_legacy_names_units_and_omissions() {
    for version in 0..=3 {
        let node = Arc::new(TestNode {
            version,
            ..Default::default()
        });
        assert_eq!(
            request(staking::routes(node.clone()), "/v0/chainParameters").await,
            (200, expected_chain(version))
        );
        assert_eq!(*node.calls.lock().unwrap(), ["conditions:lastFinal"]);
    }
}

#[tokio::test]
async fn pool_fields_preserve_optional_suspension_and_null_payday_status() {
    for suspension in [None, Some(false), Some(true)] {
        for rounds in [None, Some(0), Some(u64::MAX)] {
            let mut pool = balances::pool(suspension, suspension);
            pool.current_payday_info.as_mut().unwrap().missed_rounds = rounds;
            let node = Arc::new(TestNode {
                pool,
                ..Default::default()
            });
            let mut value = expected("pool");
            if let Some(flag) = suspension {
                value["isSuspended"] = json!(flag);
                value["currentPaydayStatus"]["isPrimedForSuspension"] = json!(flag);
            }
            if let Some(rounds) = rounds {
                value["currentPaydayStatus"]["missedRounds"] = json!(rounds);
            }
            assert_eq!(
                request(staking::routes(node.clone()), "/v0/bakerPool/42").await,
                (200, value)
            );
            assert_eq!(*node.calls.lock().unwrap(), ["pool:42"]);
        }
    }
    let mut node = TestNode::default();
    node.pool.current_payday_info = None;
    let mut value = expected("pool");
    value["currentPaydayStatus"] = Value::Null;
    assert_eq!(
        request(staking::routes(Arc::new(node)), "/v0/bakerPool/42").await,
        (200, value)
    );
}

#[tokio::test]
async fn pending_pool_changes_use_pinned_queries_and_ceiling_payday_rounding() {
    for remove in [false, true] {
        for (delta, expected_time) in [
            (0, "2026-01-01T00:00:00Z"),
            (1, "2026-01-01T00:00:02Z"),
            (2000, "2026-01-01T00:00:02Z"),
            (2001, "2026-01-01T00:00:04Z"),
        ] {
            let mut node = TestNode::default();
            node.pool.equity_pending_change =
                Some(fixtures::pending(remove, balances::NEXT + delta));
            let node = Arc::new(node);
            let (status, value) = request(staking::routes(node.clone()), "/v0/bakerPool/42").await;
            assert_eq!(status, 200);
            let change = &value["bakerStakePendingChange"];
            assert_eq!(
                change["pendingChangeType"],
                if remove {
                    "RemovePool"
                } else {
                    "ReduceBakerCapital"
                }
            );
            assert_eq!(change["estimatedChangeTime"], expected_time);
            assert_eq!(
                change.get("bakerEquityCapital").cloned(),
                if remove { None } else { Some(json!("1000")) }
            );
            assert_eq!(
                *node.calls.lock().unwrap(),
                ["pool:42", "conditions:given", "rewards:given", "consensus"]
            );
        }
    }
    let mut node = TestNode {
        version: 0,
        ..Default::default()
    };
    node.pool.equity_pending_change = Some(fixtures::pending(true, balances::NEXT));
    let node = Arc::new(node);
    let (status, value) = request(staking::routes(node.clone()), "/v0/bakerPool/42").await;
    assert_eq!(status, 200);
    assert!(
        value["bakerStakePendingChange"]
            .get("estimatedChangeTime")
            .is_none()
    );
    assert_eq!(*node.calls.lock().unwrap(), ["pool:42", "conditions:given"]);
}

#[tokio::test]
async fn pending_changes_reject_inconsistent_rewards_and_zero_epoch_lengths() {
    for (payday, epoch_millis) in [(false, 1000), (true, 0)] {
        let mut node = TestNode {
            payday,
            epoch_millis,
            ..Default::default()
        };
        node.pool.equity_pending_change = Some(fixtures::pending(false, balances::NEXT + 1));
        let node = Arc::new(node);
        assert_eq!(
            request(staking::routes(node.clone()), "/v0/bakerPool/42").await,
            (
                500,
                json!({"error": 0, "errorMessage": "Internal server error"})
            )
        );
        assert_eq!(
            *node.calls.lock().unwrap(),
            ["pool:42", "conditions:given", "rewards:given", "consensus"]
        );
    }
}

#[tokio::test]
async fn passive_delegation_and_payday_use_last_final_and_old_protocols_reject_payday() {
    let node = Arc::new(TestNode::default());
    let router = staking::routes(node.clone());
    assert_eq!(
        request(router.clone(), "/v0/passiveDelegation").await,
        (200, expected("passive"))
    );
    assert_eq!(
        request(router, "/v0/nextPayday").await,
        (200, json!({"nextPaydayTime": "2026-01-01T00:00:00Z"}))
    );
    assert_eq!(
        *node.calls.lock().unwrap(),
        ["passive", "rewards:lastFinal"]
    );
    let node = Arc::new(TestNode {
        payday: false,
        ..Default::default()
    });
    assert_eq!(
        request(staking::routes(node), "/v0/nextPayday").await,
        (
            400,
            json!({"error": 1, "errorMessage": "Invalid arguments"})
        )
    );
}

#[tokio::test]
async fn pool_validation_and_inactive_pools_keep_distinct_not_found_errors() {
    for path in ["-1", "invalid", "18446744073709551616"] {
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(
                staking::routes(node.clone()),
                &format!("/v0/bakerPool/{path}")
            )
            .await,
            (404, json!({"error": 1, "errorMessage": "Not found"}))
        );
        assert!(node.calls.lock().unwrap().is_empty());
    }
    for path in ["+42", "042", "18446744073709551615"] {
        let node = Arc::new(TestNode::default());
        assert_eq!(
            request(staking::routes(node), &format!("/v0/bakerPool/{path}"))
                .await
                .0,
            200
        );
    }
    let mut node = TestNode::default();
    node.pool.pool_info = None;
    node.pool.equity_capital = None;
    node.pool.delegated_capital = None;
    node.pool.delegated_capital_cap = None;
    assert_eq!(
        request(staking::routes(Arc::new(node)), "/v0/bakerPool/42").await,
        (404, json!({"error": 2, "errorMessage": "Not found"}))
    );
}

#[tokio::test]
async fn node_failures_preserve_custom_errors_and_stop_pending_change_queries() {
    for (operation, path) in [
        ("pool:42", "/v0/bakerPool/42"),
        ("passive", "/v0/passiveDelegation"),
        ("conditions:lastFinal", "/v0/chainParameters"),
        ("rewards:lastFinal", "/v0/nextPayday"),
    ] {
        for (failure, status, code, message) in [
            (
                Failure::Transport,
                502,
                0,
                "Cannot communicate with node via GRPC endpoint.",
            ),
            (
                Failure::Status(tonic::Code::DeadlineExceeded),
                503,
                3,
                "GRPC call failed: The node is overloaded so the request was cancelled: controlled",
            ),
            (
                Failure::Invalid,
                if path == "/v0/nextPayday" { 400 } else { 500 },
                if path == "/v0/nextPayday" { 1 } else { 0 },
                if path == "/v0/nextPayday" {
                    "Invalid arguments"
                } else {
                    "Internal server error"
                },
            ),
        ] {
            let node = Arc::new(TestNode {
                fail_at: Some(operation),
                failure,
                ..Default::default()
            });
            assert_eq!(
                request(staking::routes(node), path).await,
                (status, json!({"error": code, "errorMessage": message}))
            );
        }
    }
    for (operation, message) in [
        ("pool:42", "Not found"),
        (
            "conditions:given",
            "GRPC call failed: Requested object was not found: controlled",
        ),
        (
            "rewards:given",
            "GRPC call failed: Requested object was not found: controlled",
        ),
        (
            "consensus",
            "GRPC call failed: Requested object was not found: controlled",
        ),
    ] {
        let mut node = TestNode {
            fail_at: Some(operation),
            ..Default::default()
        };
        node.pool.equity_pending_change = Some(fixtures::pending(true, balances::NEXT));
        let node = Arc::new(node);
        assert_eq!(
            request(staking::routes(node.clone()), "/v0/bakerPool/42").await,
            (404, json!({"error": 2, "errorMessage": message}))
        );
        assert_eq!(node.calls.lock().unwrap().last().unwrap(), operation);
    }
}
