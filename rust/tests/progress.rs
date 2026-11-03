//! This module checks submission lifecycle states and simple outcome selection.
//! Source-derived expectations retain block ordering, omission rules, and ambiguity.

mod effects;
mod rejection;
mod rpc;

use crate::blocks::fixtures;
use axum::{Router, body::Body};
use concordium_rust_sdk::{
    common::types::Amount,
    types::{
        AccountTransactionEffects as Effect, BlockItemSummary, TransactionStatus,
        hashes::TransactionHash,
    },
    v2::Upward,
};
use concordium_wallet_proxy::{
    network::NetworkFailure,
    progress::{self, ProgressNode},
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};
use tower::ServiceExt;

/// This hash selects the submitted item, not the hash returned in its summary.
const HASH: &str = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
/// This address is derived from source fixture sender bytes 0x01 repeated 32 times.
const SENDER: &str = "2xBpaHottqhwFZURMZW4uZduQvpxNDSy46iXMYs9kceNGaPpZX";
/// This address is derived from source fixture receiver bytes 0x00 repeated 32 times.
const RECEIVER: &str = "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3";

/// This adapter records hashes and returns one controlled lifecycle result.
struct TestNode {
    /// This result is consumed once so unexpected retries fail the test.
    status: Mutex<Option<TransactionStatus>>,
    /// This code supplies a controlled node RPC failure.
    failure: Option<tonic::Code>,
    /// These hashes retain all status query inputs.
    calls: Mutex<Vec<TransactionHash>>,
}
impl ProgressNode for TestNode {
    async fn submission_status(
        &self,
        hash: TransactionHash,
    ) -> Result<TransactionStatus, NetworkFailure> {
        self.calls.lock().unwrap().push(hash);
        if let Some(code) = self.failure {
            return Err(NetworkFailure::Status(
                tonic::Status::new(code, "controlled").into(),
            ));
        }
        Ok(self
            .status
            .lock()
            .unwrap()
            .take()
            .expect("Unexpected status query"))
    }
}
fn node(status: TransactionStatus) -> Arc<TestNode> {
    Arc::new(TestNode {
        status: Mutex::new(Some(status)),
        failure: None,
        calls: Mutex::new(vec![]),
    })
}
fn transfer() -> BlockItemSummary {
    fixtures::account(Effect::AccountTransfer {
        amount: Amount::from_micro_ccd(u64::MAX),
        to: fixtures::RECEIVER,
    })
}
fn finalized(summary: BlockItemSummary) -> TransactionStatus {
    TransactionStatus::Finalized(BTreeMap::from([([3; 32].into(), summary)]))
}
fn expected(details: Value) -> Value {
    let mut value = json!({"status":"finalized","blockHashes":["03".repeat(32)],"transactionHash":"04".repeat(32),"sender":SENDER,"cost":"10"});
    value
        .as_object_mut()
        .unwrap()
        .extend(details.as_object().unwrap().clone());
    value
}
async fn request(router: Router, path: &str) -> (u16, Value) {
    let response = router
        .oneshot(http::Request::get(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    assert_eq!(response.headers()["content-language"], "en");
    (
        status,
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap(),
    )
}
async fn check(summary: BlockItemSummary, details: Value) {
    assert_eq!(
        request(
            progress::routes(node(finalized(summary))),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await,
        (200, expected(details))
    );
}

#[tokio::test]
async fn received_absent_and_finalized_statuses_keep_language_and_conditional_fields() {
    assert_eq!(
        request(
            progress::routes(node(TransactionStatus::Received)),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await,
        (200, json!({"status":"received"}))
    );
    let absent = Arc::new(TestNode {
        status: Mutex::new(None),
        failure: Some(tonic::Code::NotFound),
        calls: Mutex::new(vec![]),
    });
    assert_eq!(
        request(
            progress::routes(absent),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await,
        (200, json!({"status":"absent"}))
    );
    check(
        transfer(),
        json!({"outcome":"success","to":RECEIVER,"amount":"18446744073709551615"}),
    )
    .await;
}

#[tokio::test]
async fn committed_outcomes_are_sorted_and_compare_only_the_legacy_pair_fields() {
    let first = transfer();
    let mut second = first.clone();
    second.index.index = 88;
    second.energy_cost.energy = 999;
    let status = TransactionStatus::Committed(BTreeMap::from([
        ([9; 32].into(), second),
        ([1; 32].into(), first),
    ]));
    let mut value =
        expected(json!({"outcome":"success","to":RECEIVER,"amount":"18446744073709551615"}));
    value["status"] = "committed".into();
    value["blockHashes"] = json!(["01".repeat(32), "09".repeat(32)]);
    assert_eq!(
        request(
            progress::routes(node(status)),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await,
        (200, value)
    );
}

#[tokio::test]
async fn any_projected_difference_makes_committed_outcomes_ambiguous_without_common_fields() {
    for difference in 0..4 {
        let first = transfer();
        let mut second = transfer();
        if difference == 0 {
            second.hash = [5; 32].into();
        }
        if let Upward::Known(
            concordium_rust_sdk::types::BlockItemSummaryDetails::AccountTransaction(account),
        ) = &mut second.details
        {
            match difference {
                1 => account.cost = Amount::from_micro_ccd(11),
                2 => {
                    account.sponsor = Some(concordium_rust_sdk::types::SponsorDetails {
                        sponsor: fixtures::RECEIVER,
                        cost: Amount::from_micro_ccd(0),
                    })
                }
                3 => {
                    account.effects = Upward::Known(Effect::AccountTransfer {
                        amount: Amount::from_micro_ccd(1),
                        to: fixtures::RECEIVER,
                    })
                }
                _ => {}
            }
        }
        let status = TransactionStatus::Committed(BTreeMap::from([
            ([1; 32].into(), first),
            ([2; 32].into(), second),
        ]));
        assert_eq!(
            request(
                progress::routes(node(status)),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await,
            (
                200,
                json!({"status":"committed","blockHashes":["01".repeat(32),"02".repeat(32)],"outcome":"ambiguous"})
            )
        );
    }
}

#[tokio::test]
async fn a_later_unsupported_committed_outcome_is_not_hidden_by_earlier_ambiguity() {
    let one = transfer();
    let two = fixtures::account(Effect::AccountTransfer {
        amount: Amount::from_micro_ccd(1),
        to: fixtures::RECEIVER,
    });
    let unsupported = fixtures::account(Effect::TransferredWithSchedule {
        to: fixtures::RECEIVER,
        amount: vec![],
    });
    let status = TransactionStatus::Committed(BTreeMap::from([
        ([1; 32].into(), one),
        ([2; 32].into(), two),
        ([3; 32].into(), unsupported),
    ]));
    let (status, body) = request(
        progress::routes(node(status)),
        &format!("/v0/submissionStatus/{HASH}"),
    )
    .await;
    assert_eq!(status, 400);
    assert!(body.get("blockHashes").is_none());
}

#[tokio::test]
async fn invalid_empty_or_multiple_finalized_outcomes_fail_instead_of_guessing() {
    for status in [
        TransactionStatus::Committed(BTreeMap::new()),
        TransactionStatus::Finalized(BTreeMap::new()),
        TransactionStatus::Finalized(BTreeMap::from([
            ([1; 32].into(), transfer()),
            ([2; 32].into(), transfer()),
        ])),
    ] {
        assert_eq!(
            request(
                progress::routes(node(status)),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await,
            (
                500,
                json!({"error":0,"errorMessage":"Internal server error"})
            )
        );
    }
}

#[tokio::test]
async fn malformed_hashes_never_query_and_legacy_case_and_trailing_whitespace_are_accepted() {
    for hash in [
        "bad",
        &"a".repeat(63),
        &"g".repeat(64),
        &format!("%20{HASH}"),
    ] {
        let node = node(TransactionStatus::Received);
        assert_eq!(
            request(
                progress::routes(node.clone()),
                &format!("/v0/submissionStatus/{hash}")
            )
            .await,
            (
                400,
                json!({"error":1,"errorMessage":"Malformed transaction hash"})
            )
        );
        assert!(node.calls.lock().unwrap().is_empty());
    }
    let node = node(TransactionStatus::Received);
    assert_eq!(
        request(
            progress::routes(node.clone()),
            &format!("/v0/submissionStatus/{}%20%0A", HASH.to_uppercase())
        )
        .await
        .0,
        200
    );
    assert_eq!(
        *node.calls.lock().unwrap(),
        [TransactionHash::from([0xaa; 32])]
    );
}

#[tokio::test]
async fn unsupported_successes_fail_but_their_rejections_keep_legacy_descriptions() {
    for effect in [
        Effect::BakerRemoved {
            baker_id: concordium_rust_sdk::types::BakerId { id: 42.into() },
        },
        Effect::TransferredWithSchedule {
            to: fixtures::RECEIVER,
            amount: vec![],
        },
        Effect::CredentialKeysUpdated {
            cred_id: fixtures::credential(),
        },
    ] {
        assert_eq!(
            request(
                progress::routes(node(finalized(fixtures::account(effect)))),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await,
            (
                400,
                json!({"error":1,"errorMessage":"GRPC call failed: Unsupported transaction type for simple statuses."})
            )
        );
    }
    use concordium_rust_sdk::types::{
        BlockItemSummaryDetails, CreatePlt, TokenCreationDetails, UpdateDetails, UpdatePayload,
    };
    for details in [
        BlockItemSummaryDetails::Update(UpdateDetails {
            effective_time: concordium_rust_sdk::common::types::TransactionTime::from_seconds(0),
            payload: Upward::Known(UpdatePayload::FoundationAccount(fixtures::SENDER)),
        }),
        BlockItemSummaryDetails::TokenCreationDetails(TokenCreationDetails {
            create_plt: CreatePlt {
                token_id: "USD".parse().unwrap(),
                token_module: [7; 32].into(),
                decimals: 6,
                initialization_parameters: vec![0xa0].into(),
            },
            events: vec![],
        }),
    ] {
        let mut summary = transfer();
        summary.details = Upward::Known(details);
        assert_eq!(
            request(
                progress::routes(node(finalized(summary))),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await
            .0,
            400
        );
    }
    check(
        fixtures::account(Effect::None {
            transaction_type: None,
            reject_reason: Upward::Known(concordium_rust_sdk::types::RejectReason::OutOfEnergy),
        }),
        json!({"outcome":"reject","rejectReason":"Insufficient energy"}),
    )
    .await;
}
