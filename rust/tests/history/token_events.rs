//! These tests check source-derived token and lock history representations.
//! The repository's Haskell fixtures and literal expectations define the legacy contract.

use super::http::*;
use concordium_wallet_proxy::history;
use serde_json::{Value, json};

/// Read the committed Haskell lock fixtures without running Haskell or converting SDK outcomes.
pub(super) fn lock_fixture(name: &str) -> Value {
    let text = match name {
        "created" => include_str!("../../../test/fixtures/meta-update-lock-created.json"),
        "destroyed" => include_str!("../../../test/fixtures/meta-update-lock-destroyed.json"),
        "transfer" => include_str!("../../../test/fixtures/meta-update-token-transfer-locks.json"),
        _ => panic!("Unknown source fixture"),
    };
    json!({"Left":serde_json::from_str::<Value>(text).unwrap()})
}

/// Build one legacy account-token transfer without optional lock fields.
pub(super) fn token_transfer() -> Value {
    json!({"tag":"TokenTransfer","tokenId":"USD","from":{"type":"account","address":SENDER},"to":{"type":"account","address":RECEIVER},"amount":{"value":"10","decimals":2}})
}

#[tokio::test]
async fn committed_haskell_meta_fixtures_keep_lock_details_and_legacy_descriptions() {
    for (name, text) in [
        ("created", "Lock created: WV6MDZDimp."),
        ("destroyed", "Lock destroyed: WsA9qh35Yb."),
    ] {
        let fixture = lock_fixture(name);
        let body = entry(fixture.clone(), 3, SENDER, "").await;
        let event = &fixture["Left"]["result"]["events"][0];
        let mut details = json!({"type":"metaUpdate","description":"Meta update","outcome":"success","events":[text],"lockId":event["lockId"]});
        if name == "created" {
            details["lockConfig"] = event["lockConfig"].clone();
        }
        assert_eq!(body["details"], details);
        assert_eq!(
            body["transactionHash"],
            "039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81"
        );
        assert_eq!(body["total"], "0");
        assert_eq!(body["cost"], "0");
        assert!(body.get("subtotal").is_none());
    }
    let fixture = lock_fixture("transfer");
    let body = entry(fixture, 3, SENDER, "").await;
    assert_eq!(
        body["details"],
        json!({"type":"metaUpdate","description":"Meta update","outcome":"success","events":["10 CCD transferred from 2xdTv8awN1BjgYEw8W1BVXVtiEwG2b29U8KoZQqJrDuEqddseE locked by XFDxZx4Hoa to 2y57FyMyqAfY7X1SuSWJ5VMt1Z3ZgxbKt9w5mGoTwqA7YcpbXr locked by XdHmDZ3Ff9."],"transferSource":"2xdTv8awN1BjgYEw8W1BVXVtiEwG2b29U8KoZQqJrDuEqddseE","transferDestination":"2y57FyMyqAfY7X1SuSWJ5VMt1Z3ZgxbKt9w5mGoTwqA7YcpbXr","tokenId":"CCD","tokenTransferAmount":{"value":"10","decimals":0},"fromLock":{"accountIndex":30,"sequenceNumber":31,"creationOrder":32},"toLock":{"accountIndex":40,"sequenceNumber":41,"creationOrder":42}})
    );
}

#[tokio::test]
async fn token_and_meta_single_transfers_preserve_optional_omissions_and_public_fee_effects() {
    for kind in ["tokenUpdate", "metaUpdate"] {
        for mask in 0..8 {
            let mut event = token_transfer();
            if mask & 1 > 0 {
                event["fromLock"] =
                    json!({"accountIndex":10,"sequenceNumber":11,"creationOrder":12});
            }
            if mask & 2 > 0 {
                event["toLock"] = json!({"accountIndex":20,"sequenceNumber":21,"creationOrder":22});
            }
            if mask & 4 > 0 {
                event["memo"] = json!("00FF");
            }
            let body = entry(summary(kind, vec![event]), 3, SENDER, "").await;
            let expected_text = format!(
                "0.10 USD transferred from {SENDER}{} to {RECEIVER}{}{}.",
                if mask & 1 > 0 {
                    " locked by WV6MDZDimp"
                } else {
                    ""
                },
                if mask & 2 > 0 {
                    " locked by WsA9qh35Yb"
                } else {
                    ""
                },
                if mask & 4 > 0 { " with memo 00ff" } else { "" }
            );
            assert_eq!(body["details"]["events"], json!([expected_text]));
            assert_eq!(
                body["details"]["tokenTransferAmount"],
                json!({"value":"10","decimals":2})
            );
            for (name, bit) in [("fromLock", 1), ("toLock", 2), ("memo", 4)] {
                assert_eq!(body["details"].get(name).is_some(), mask & bit > 0);
            }
            assert_eq!(body["cost"], "10");
            assert_eq!(body["total"], "-10");
            assert!(body.get("subtotal").is_none());
            assert!(body.get("encrypted").is_none());
        }
        let mut event = token_transfer();
        event["fromLock"] = Value::Null;
        event["toLock"] = Value::Null;
        event["memo"] = Value::Null;
        let mut value = summary(kind, vec![event]);
        value["Left"]["sponsorDetails"] = json!({"sponsor":RECEIVER,"cost":"7"});
        let body = entry(value, 3, RECEIVER, "").await;
        assert_eq!(body["cost"], "7");
        assert_eq!(body["total"], "-7");
        assert!(body.get("subtotal").is_none());
        for name in ["fromLock", "toLock", "memo"] {
            assert!(body["details"].get(name).is_none());
        }
    }
}

#[tokio::test]
async fn token_creation_pretty_json_and_governance_events_match_legacy_wording() {
    let payload = json!({"tokenId":"USD","tokenModule":"05".repeat(32),"decimals":6,"initializationParameters":"A0","ignored":true});
    let mut value = summary(
        "transfer",
        vec![json!({"tag":"TokenCreated","payload":payload})],
    );
    value["Left"]["type"] = json!({"type":"updateTransaction","contents":"updateCreatePLT"});
    value["Left"]["sender"] = Value::Null;
    let body = entry(value, 3, SENDER, "").await;
    let expected = format!(
        "Token created: {{\n    \"decimals\": 6,\n    \"initializationParameters\": \"a0\",\n    \"tokenId\": \"USD\",\n    \"tokenModule\": \"{}\"\n}}",
        "05".repeat(32)
    );
    assert_eq!(
        body["details"],
        json!({"type":"chainUpdate","description":"Update create new PLT token","outcome":"success","events":[expected]})
    );
    assert_eq!(body["origin"], json!({"type":"none"}));
    assert_eq!(body["total"], "0");
    for (kind, expected) in [
        (
            "pause",
            "USD token module event occurred of type \"pause\".",
        ),
        (
            "é1",
            "USD token module event occurred of type \"\\195\\169\\&1\".",
        ),
    ] {
        let event = json!({"tag":"TokenModuleEvent","tokenId":"USD","type":kind,"details":"ff"});
        let body = entry(summary("tokenUpdate", vec![event]), 3, SENDER, "").await;
        assert_eq!(body["details"]["events"], json!([expected]));
        assert!(body["details"].get("tokenId").is_none());
    }
    for (tag, text) in [("TokenMint", "minted to"), ("TokenBurn", "burned from")] {
        let event = json!({"tag":tag,"tokenId":"USD","target":{"type":"account","address":RECEIVER},"amount":{"value":"18446744073709551615","decimals":6}});
        let body = entry(summary("tokenUpdate", vec![event]), 3, SENDER, "").await;
        assert_eq!(
            body["details"]["events"],
            json!([format!("18446744073709.551615 USD {text} {RECEIVER}.")])
        );
        assert_eq!(body["total"], "-10");
    }
}

#[tokio::test]
async fn singleton_patterns_do_not_leak_lock_or_transfer_fields_from_mixed_events() {
    let created = lock_fixture("created")["Left"]["result"]["events"][0].clone();
    for kind in ["tokenUpdate", "metaUpdate"] {
        for events in [
            vec![],
            vec![token_transfer(), token_transfer()],
            vec![created.clone(), token_transfer()],
        ] {
            let body = entry(summary(kind, events), 3, SENDER, "").await;
            for name in [
                "lockId",
                "lockConfig",
                "tokenId",
                "tokenTransferAmount",
                "transferSource",
                "transferDestination",
            ] {
                assert!(body["details"].get(name).is_none());
            }
        }
    }
    let body = entry(summary("tokenUpdate", vec![created]), 3, SENDER, "").await;
    assert!(body["details"].get("lockId").is_none());
    assert_eq!(
        body["details"]["events"],
        json!(["Lock created: WV6MDZDimp."])
    );
}

#[tokio::test]
async fn lock_boundaries_and_opaque_configuration_keep_exact_identifiers_and_bytes() {
    let event = json!({"tag":"LockCreated","lockId":{"accountIndex":18446744073709551615_u64,"sequenceNumber":18446744073709551615_u64,"creationOrder":18446744073709551615_u64},"lockConfig":"FF"});
    let body = entry(summary("metaUpdate", vec![event]), 3, SENDER, "").await;
    assert_eq!(
        body["details"]["events"],
        json!(["Lock created: Q9e2yAcYAVudCremvRWoRistPqxhaGcGhk33LRACJS5ZYWD."])
    );
    assert_eq!(body["details"]["lockConfig"], "ff");
    assert_eq!(body["details"]["lockId"]["accountIndex"], json!(u64::MAX));
}

#[tokio::test]
async fn token_decimal_text_retains_legacy_word64_scaling_and_failure_boundaries() {
    for (value, decimals, expected) in [
        ("00010", 0, "10".to_owned()),
        ("0", 2, "0.00".to_owned()),
        (
            "18446744073709551615",
            20,
            "2.02914184810805067775".to_owned(),
        ),
    ] {
        let mut event = token_transfer();
        event["amount"] = json!({"value":value,"decimals":decimals});
        let body = entry(summary("tokenUpdate", vec![event]), 3, SENDER, "").await;
        assert_eq!(
            body["details"]["events"],
            json!([format!(
                "{expected} USD transferred from {SENDER} to {RECEIVER}."
            )])
        );
    }
    for decimals in [64, 128, 255] {
        let mut event = token_transfer();
        event["amount"]["decimals"] = json!(decimals);
        let (node, database) = adapters(vec![summary("tokenUpdate", vec![event])]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v3/accTransactions/{SENDER}")
            )
            .await,
            (502, json!({"error":0,"errorMessage":"Database error"}))
        );
    }
}

#[tokio::test]
async fn token_identifiers_and_governance_type_strings_keep_legacy_byte_limits() {
    for (token, kind, valid) in [
        ("A".repeat(128), "é".repeat(127) + "a", true),
        ("A".repeat(129), "pause".to_owned(), false),
        ("USD".to_owned(), "é".repeat(128), false),
        ("USD".to_owned(), String::new(), true),
        (String::new(), "pause".to_owned(), false),
    ] {
        let event = json!({"tag":"TokenModuleEvent","tokenId":token,"type":kind,"details":""});
        let (node, database) = adapters(vec![summary("tokenUpdate", vec![event])]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v3/accTransactions/{SENDER}")
            )
            .await
            .0,
            if valid { 200 } else { 502 }
        );
    }
    let mut fixture = lock_fixture("created");
    fixture["Left"]["result"]["events"][0]["lockId"]["accountIndex"] =
        serde_json::from_str("10e0").unwrap();
    let body = entry(fixture, 3, SENDER, "").await;
    assert_eq!(
        body["details"]["events"],
        json!(["Lock created: WV6MDZDimp."])
    );
    assert_eq!(body["details"]["lockId"]["accountIndex"], 10);
}

#[tokio::test]
async fn malformed_token_and_lock_records_fail_without_dropping_any_selected_entry() {
    let mut cases = vec![
        json!({"tag":"FutureTokenEvent"}),
        json!({"tag":"LockCreated","lockId":{"accountIndex":1,"sequenceNumber":2},"lockConfig":"a0"}),
        json!({"tag":"LockDestroyed","lockId":"WV6MDZDimp"}),
        json!({"tag":"TokenCreated","payload":{}}),
    ];
    for (path, value) in [
        (
            "from",
            json!({"type":"contract","address":{"index":1,"subindex":0}}),
        ),
        ("tokenId", json!("bad!")),
        ("memo", json!("zz")),
        (
            "fromLock",
            json!({"accountIndex":-1,"sequenceNumber":2,"creationOrder":3}),
        ),
        ("toLock", json!({})),
    ] {
        let mut event = token_transfer();
        event[path] = value;
        cases.push(event);
    }
    for text in ["+1", "-1", "1.0", "18446744073709551616"] {
        let mut event = token_transfer();
        event["amount"]["value"] = json!(text);
        cases.push(event);
    }
    for event in cases {
        let (node, database) = adapters(vec![
            summary("transfer", vec![]),
            summary("metaUpdate", vec![event]),
        ]);
        assert_eq!(
            request(
                history::routes(node, database),
                &format!("/v3/accTransactions/{SENDER}")
            )
            .await,
            (502, json!({"error":0,"errorMessage":"Database error"}))
        );
    }
}
