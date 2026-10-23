//! This module checks fee modes, legacy numeric boundaries, and HTTP validation.
//! Expected calculations come from the Haskell sources, not the Rust estimator.

mod rpc;

use axum::{Router, body::Body};
use concordium_rust_sdk::types::{ProtocolVersion, chain_parameters::EnergyRate};
use concordium_wallet_proxy::{
    fees::{self, FeeNode},
    network::NetworkFailure,
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};
use tower::ServiceExt;

/// This adapter returns a controlled rate after recording context access.
struct TestNode {
    /// This protocol controls memo support.
    protocol: ProtocolVersion,
    /// This rate supplies exact micro-CCD pricing.
    rate: EnergyRate,
    /// This flag selects a transport failure before input validation.
    fail: bool,
    /// This counter records context reads.
    calls: AtomicUsize,
}
impl Default for TestNode {
    fn default() -> Self {
        Self {
            protocol: ProtocolVersion::P10,
            rate: EnergyRate {
                micro_ccd_per_energy: num::rational::Ratio::new(2, 3),
            },
            fail: false,
            calls: AtomicUsize::new(0),
        }
    }
}
impl FeeNode for TestNode {
    async fn fee_context(&self) -> Result<(EnergyRate, ProtocolVersion), NetworkFailure> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        if self.fail {
            Err(NetworkFailure::Transport)
        } else {
            Ok((self.rate.clone(), self.protocol))
        }
    }
}
async fn request(router: Router, query: &str) -> (u16, Value) {
    let response = router
        .oneshot(
            http::Request::builder()
                .uri(format!("/v0/transactionCost?{query}"))
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
    let status = response.status().as_u16();
    if status != 200 {
        assert_eq!(response.headers()["content-language"], "en");
    }
    let body = response.into_body().collect().await.unwrap().to_bytes();
    (status, serde_json::from_slice(&body).unwrap())
}

#[tokio::test]
async fn fee_modes_match_source_derived_http_fixtures() {
    let node = Arc::new(TestNode::default());
    let fixtures: Vec<Value> = serde_json::from_str(include_str!("fixtures/fees.json")).unwrap();
    for fixture in &fixtures {
        let query = fixture["query"].as_str().unwrap();
        assert_eq!(
            request(fees::routes(node.clone()), query).await,
            (
                200,
                json!({"energy": fixture["energy"], "cost": fixture["cost"]})
            ),
            "{query}"
        );
    }
    assert_eq!(node.calls.load(Ordering::SeqCst), fixtures.len());
}

#[tokio::test]
async fn numeric_reads_keep_legacy_syntax_wrap_points_and_first_values() {
    for (query, energy) in [
        (
            "type=simpleTransfer&memoSize=0x10&numSignatures=%28%202%20%29",
            619u64,
        ),
        ("type=simpleTransfer&memoSize=0o10", 511),
        ("type=simpleTransfer&memoSize=4294967295", 502),
        ("type=simpleTransfer&memoSize=-1", 502),
        ("type=simpleTransfer&memoSize=4294967296", 503),
        ("type=configureBaker&metadataSize=-3", 462),
        ("type=configureBaker&metadataSize=-4", 461),
        ("type=configureBaker&metadataSize=9223372036854775808", 465),
        (
            "type=simpleTransfer&numSignatures=18446744073709551617",
            501,
        ),
        ("type=simpleTransfer&numSignatures=9223372036854775807", 301),
    ] {
        let (_, value) = request(fees::routes(Arc::new(TestNode::default())), query).await;
        assert_eq!(value["energy"], json!(energy), "{query}");
    }
}

#[tokio::test]
async fn validation_order_and_messages_match_legacy_errors() {
    for (query, message) in [
        ("numSignatures=0", "Could not parse `numSignatures` value."),
        (
            "numSignatures=-1&type=simpleTransfer",
            "Could not parse `numSignatures` value.",
        ),
        (
            "numSignatures=9223372036854775808",
            "Could not parse `numSignatures` value.",
        ),
        ("", "Missing `type` value."),
        (
            "type=nope&memoSize=bad",
            "Could not parse transaction type: nope",
        ),
        (
            "type=simpleTransfer&memoSize=1junk",
            "Could not parse `memoSize` value.",
        ),
        (
            "type=simpleTransfer&memoSize=%2B1",
            "Could not parse `memoSize` value.",
        ),
        (
            "type=registerBaker&metadataSize=",
            "Could not parse `metadataSize` value.",
        ),
        (
            "type=updateBakerPool&metadataSize=1.5",
            "Could not parse `metadataSize` value.",
        ),
        (
            "type=configureBaker&metadataSize=bad",
            "Could not parse `metadataSize` value.",
        ),
    ] {
        assert_eq!(
            request(fees::routes(Arc::new(TestNode::default())), query).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})
            ),
            "{query}"
        );
    }
}

#[tokio::test]
async fn memos_are_only_protocol_gated_when_present_and_valid() {
    let node = Arc::new(TestNode {
        protocol: ProtocolVersion::P1,
        ..Default::default()
    });
    assert_eq!(
        request(fees::routes(node.clone()), "type=simpleTransfer")
            .await
            .0,
        200
    );
    for kind in ["simpleTransfer", "encryptedTransfer"] {
        assert_eq!(
            request(
                fees::routes(node.clone()),
                &format!("type={kind}&memoSize=0")
            )
            .await,
            (
                404,
                json!({"error":2,"errorMessage":"The required action is not supported. The node's protocol version is incompatible with it."})
            )
        );
        assert_eq!(
            request(
                fees::routes(node.clone()),
                &format!("type={kind}&memoSize=bad")
            )
            .await
            .0,
            400
        );
    }
    assert_eq!(
        request(
            fees::routes(node),
            "type=removeBaker&memoSize=bad&metadataSize=bad"
        )
        .await
        .0,
        200
    );
}

#[tokio::test]
async fn configure_validator_options_are_additive_for_every_presence_combination() {
    let node = Arc::new(TestNode::default());
    let fields = [
        "amount",
        "restake",
        "openStatus",
        "keys",
        "metadataSize",
        "transactionCommission",
        "bakerRewardCommission",
        "finalizationRewardCommission",
        "suspended",
    ];
    let sizes = [8u32, 1, 1, 352, 9, 4, 4, 4, 1];
    for mask in 0..512u32 {
        let mut query = String::from("type=configureBaker&sponsored=false");
        let mut payload = 3;
        for (index, field) in fields.iter().enumerate() {
            if mask & (1 << index) != 0 {
                query.push_str(&format!(
                    "&{field}={}",
                    if *field == "metadataSize" {
                        "7"
                    } else {
                        "false"
                    }
                ));
                payload += sizes[index];
            }
        }
        let expected = 160 + payload + if mask & 8 != 0 { 4050 } else { 300 };
        let (status, value) = request(fees::routes(node.clone()), &query).await;
        assert_eq!(status, 200);
        assert_eq!(value["energy"], json!(expected), "{query}");
    }
}

#[tokio::test]
async fn sponsorship_presence_only_changes_the_legacy_sponsorable_modes() {
    let node = Arc::new(TestNode::default());
    for kind in [
        "simpleTransfer",
        "encryptedTransfer",
        "transferToSecret",
        "transferToPublic",
        "registerDelegation",
        "updateDelegation",
        "removeDelegation",
        "registerBaker",
        "updateBakerStake",
        "updateBakerPool",
        "updateBakerKeys",
        "removeBaker",
        "configureBaker",
    ] {
        let (_, plain) = request(fees::routes(node.clone()), &format!("type={kind}")).await;
        let delta = if matches!(
            kind,
            "transferToSecret" | "updateBakerKeys" | "removeBaker" | "configureBaker"
        ) {
            0
        } else {
            34
        };
        for value in ["", "false", "true", "junk"] {
            let (_, sponsored) = request(
                fees::routes(node.clone()),
                &format!("type={kind}&sponsored={value}"),
            )
            .await;
            assert_eq!(
                sponsored["energy"].as_u64().unwrap(),
                plain["energy"].as_u64().unwrap() + delta,
                "{kind}: {value}"
            );
        }
    }
}

#[tokio::test]
async fn dependency_failure_precedes_input_errors() {
    let node = Arc::new(TestNode {
        fail: true,
        ..Default::default()
    });
    assert_eq!(
        request(fees::routes(node.clone()), "numSignatures=bad").await,
        (
            502,
            json!({"error":0,"errorMessage":"Cannot communicate with node via GRPC endpoint."})
        )
    );
    assert_eq!(node.calls.load(Ordering::SeqCst), 1);
}

#[tokio::test]
async fn fee_rounding_is_exact_above_float_precision_and_at_word64_boundary() {
    for (numerator, denominator, expected) in [
        (0u128, 1u128, "0"),
        (1, 501, "1"),
        (1, 502, "1"),
        (9007199254740993, 501, "9007199254740993"),
        (u64::MAX as u128, 501, "18446744073709551615"),
        (1u128 << 64, 501, "0"),
        (u128::MAX, 1, "18446744073709551115"),
    ] {
        let node = Arc::new(TestNode {
            rate: EnergyRate {
                micro_ccd_per_energy: num::rational::Ratio::new(numerator, denominator),
            },
            ..Default::default()
        });
        let (status, value) = request(fees::routes(node), "type=simpleTransfer").await;
        assert_eq!(status, 200);
        assert_eq!(value["cost"], expected);
    }
}
