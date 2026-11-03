//! These tests check persisted token and historical lock rejection descriptions.
//! Historical return names remain distinct from newer SDK release variants.

use super::http::*;
use concordium_wallet_proxy::history;
use serde_json::{Value, json};

/// Build a rejected persisted meta update without any success events.
fn rejected(reason: Value) -> Value {
    let mut value = summary("metaUpdate", vec![]);
    value["Left"]["result"] = json!({"outcome":"reject","rejectReason":reason});
    value
}

#[tokio::test]
async fn historical_lock_reasons_keep_exact_wording_raw_names_and_sponsor_costs() {
    let id = json!({"accountIndex":10,"sequenceNumber":11,"creationOrder":12});
    let details = json!({"lockId":id,"account":SENDER});
    let fixtures = [
        (
            "NonExistentLockId",
            id.clone(),
            "Non existing lock id WV6MDZDimp.".to_owned(),
        ),
        (
            "LockExpired",
            id.clone(),
            "Lock WV6MDZDimp has expired.".to_owned(),
        ),
        (
            "LockFundNotAuthorized",
            details.clone(),
            format!("Account {SENDER} is not authorized to fund lock WV6MDZDimp."),
        ),
        (
            "LockSendNotAuthorized",
            details.clone(),
            format!("Account {SENDER} is not authorized to send from lock WV6MDZDimp."),
        ),
        (
            "LockReturnNotAuthorized",
            details.clone(),
            format!("Account {SENDER} is not authorized to return from lock WV6MDZDimp."),
        ),
        (
            "LockCancelNotAuthorized",
            details.clone(),
            format!("Account {SENDER} is not authorized to cancel lock WV6MDZDimp."),
        ),
        (
            "LockTokenNotPermitted",
            json!({"lockId":id,"tokenId":"USD"}),
            "Token USD is not permitted by lock WV6MDZDimp.".to_owned(),
        ),
        (
            "LockRecipientNotPermitted",
            details,
            format!("Account {SENDER} is not a permitted recipient for lock WV6MDZDimp."),
        ),
    ];
    for (tag, contents, text) in fixtures {
        let reason = json!({"tag":tag,"contents":contents});
        let mut value = rejected(reason.clone());
        value["Left"]["sponsorDetails"] = json!({"sponsor":RECEIVER,"cost":"7"});
        for query in ["", "?includeRawRejectReason=n"] {
            let body = entry(value.clone(), 3, RECEIVER, query).await;
            assert_eq!(body["details"]["rejectReason"], text);
            assert_eq!(body["details"]["outcome"], "reject");
            assert_eq!(body["details"]["type"], "metaUpdate");
            assert!(body["details"].get("events").is_none());
            assert_eq!(body["cost"], "7");
            assert_eq!(body["total"], "-7");
            assert!(body.get("subtotal").is_none());
            if query.is_empty() {
                assert!(body["details"].get("rawRejectReason").is_none());
            } else {
                assert_eq!(body["details"]["rawRejectReason"], reason);
            }
        }
    }
}

#[tokio::test]
async fn token_rejections_retain_legacy_type_byte_escapes_and_optional_details() {
    let body = entry(
        rejected(json!({"tag":"NonExistentTokenId","contents":"USD"})),
        3,
        SENDER,
        "",
    )
    .await;
    assert_eq!(
        body["details"]["rejectReason"],
        "Non existing plt token id USD."
    );
    for (details, show) in [
        (Value::Null, "Nothing"),
        (json!(""), "Just "),
        (json!("A0"), "Just a0"),
    ] {
        let reason = json!({"tag":"TokenUpdateTransactionFailed","contents":{"tokenId":"USD","type":"é1","details":details}});
        let mut value = rejected(reason);
        value["Left"]["type"]["contents"] = json!("tokenUpdate");
        let body = entry(value, 3, SENDER, "?includeRawRejectReason").await;
        assert_eq!(
            body["details"]["rejectReason"],
            format!(
                "Token update transaction failed with reason: TokenModuleRejectReason {{tmrrTokenId = USD, tmrrType = \"\\195\\169\\&1\", tmrrDetails = {show}}}."
            )
        );
        assert_eq!(body["details"]["type"], "tokenUpdate");
        let raw = &body["details"]["rawRejectReason"]["contents"];
        if details.is_null() {
            assert!(raw.get("details").is_none());
        } else {
            assert_eq!(raw["details"], if show == "Just " { "" } else { "a0" });
        }
    }
}

#[tokio::test]
async fn newer_or_malformed_rejections_are_not_guessed_or_converted_to_historical_return() {
    for reason in [
        json!({"tag":"LockReleaseNotAuthorized","contents":{"lockId":{"accountIndex":10,"sequenceNumber":11,"creationOrder":12},"account":SENDER}}),
        json!({"tag":"LockDurationTooLong","contents":{"accountIndex":10,"sequenceNumber":11,"creationOrder":12}}),
        json!({"tag":"LockReturnNotAuthorized","contents":{"account":SENDER}}),
        json!({"tag":"FutureTokenReject"}),
    ] {
        let (node, database) = adapters(vec![summary("transfer", vec![]), rejected(reason)]);
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
