//! These tests check exact numeric boundaries and legacy query parsing distinctions.
//! Expected numbers come from integer/rational arithmetic, not replacement output.

use super::http::*;
use concordium_wallet_proxy::history;
use serde_json::{Value, json};
use std::sync::Arc;

#[tokio::test]
async fn legacy_mint_payloads_accept_full_word8_exponents_without_sdk_decimal_truncation() {
    for text in ["1e-255", "4294967295e-255", "42949672950e-256", "0", "-0"] {
        let rate: serde_json::Value = serde_json::from_str(text).unwrap();
        for (kind, fields) in [
            (
                "mintDistribution",
                json!({"mintPerSlot":rate,"bakingReward":0.5,"finalizationReward":0.5}),
            ),
            (
                "timeParametersCPV1",
                json!({"mintPerPayday":rate,"rewardPeriodLength":2}),
            ),
        ] {
            let event = json!({"tag":"UpdateEnqueued","effectiveTime":1,"payload":{"updateType":kind,"update":fields}});
            let body = entry(summary("transfer", vec![event]), 2, SENDER, "").await;
            assert_eq!(
                body["details"]["events"],
                json!(["Chain update event enqueued."])
            );
        }
    }
    for text in ["1e-256", "4294967296e-255", "-1e-2", "1e1"] {
        let rate: serde_json::Value = serde_json::from_str(text).unwrap();
        let event = json!({"tag":"UpdateEnqueued","effectiveTime":1,"payload":{"updateType":"mintDistribution","update":{"mintPerSlot":rate,"bakingReward":0.5,"finalizationReward":0.5}}});
        let (node, database) = adapters(vec![summary("transfer", vec![event])]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v2/accTransactions/{SENDER}")
            )
            .await
            .0,
            502
        );
    }
}

#[tokio::test]
async fn integral_scientific_json_fields_decode_exactly_but_fractions_and_overflows_fail() {
    let mut value = summary("transfer", vec![]);
    value["Left"]["index"] = serde_json::from_str("1.0").unwrap();
    value["Left"]["energyCost"] = serde_json::from_str("18446744073709551615e0").unwrap();
    value["Left"]["result"] = json!({"outcome":"reject","rejectReason":{"tag":"RejectedInit","rejectReason":serde_json::from_str::<Value>("-2e0").unwrap()}});
    let body = entry(value.clone(), 2, SENDER, "?includeRawRejectReason").await;
    assert_eq!(body["energy"], json!(u64::MAX));
    assert_eq!(
        body["details"]["rawRejectReason"],
        json!({"tag":"RejectedInit","rejectReason":-2})
    );
    for text in ["1.5", "18446744073709551616e0"] {
        value["Left"]["energyCost"] = serde_json::from_str(text).unwrap();
        let (node, database) = adapters(vec![value.clone()]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v2/accTransactions/{SENDER}")
            )
            .await
            .0,
            502
        );
    }
}

#[tokio::test]
async fn timestamp_rounds_the_exact_rational_once_and_cursor_overflow_defaults_without_wrapping() {
    let (node, mut database) = adapters(vec![summary("transfer", vec![])]);
    Arc::get_mut(&mut database).unwrap().timestamp = Some(18_446_744_073_709_542_000);
    let router = history::routes(node, database);
    let (_, body) = request(router.clone(), &format!("/v0/accTransactions/{SENDER}")).await;
    assert_eq!(
        body["transactions"][0]["blockTime"].as_f64(),
        Some(18_446_744_073_709_544.0)
    );
    for text in [
        "9223372036854775808",
        "-9223372036854775809",
        "0x10",
        "(1)",
        "%201",
    ] {
        let (_, body) = request(
            router.clone(),
            &format!("/v0/accTransactions/{SENDER}?from={text}"),
        )
        .await;
        assert!(body.get("from").is_none());
    }
    for (text, expected) in [
        ("%2B3", 3),
        ("-9223372036854775808", i64::MIN),
        ("9223372036854775807", i64::MAX),
    ] {
        let (_, body) = request(
            router.clone(),
            &format!("/v0/accTransactions/{SENDER}?from={text}"),
        )
        .await;
        assert_eq!(body["from"], expected);
    }
    // Unlike PathPiece cursors, Read-based limits wrap before being clamped.
    let (_, body) = request(
        router,
        &format!("/v0/accTransactions/{SENDER}?limit=18446744073709551617"),
    )
    .await;
    assert_eq!(body["limit"], 1);
}
