//! This module checks token counts, payload sizes, and legacy validation ordering.
//! Tests use independent cost literals and do not serialize real token transactions.

use super::*;

/// Encode token query values without changing JSON or UTF-8 contents.
pub(super) fn query(size: &str, counts: &str, token: &str) -> String {
    let mut url = reqwest::Url::parse("http://localhost/v0/transactionCost").unwrap();
    url.query_pairs_mut()
        .append_pair("type", "tokenUpdate")
        .append_pair("listOperationsSize", size)
        .append_pair("tokenOperationTypeCount", counts)
        .append_pair("tokenId", token);
    url.query().unwrap().into()
}

#[tokio::test]
async fn every_legacy_token_operation_count_uses_its_specific_cost() {
    for (name, specific) in [
        ("transfer", 100),
        ("mint", 50),
        ("burn", 50),
        ("addAllowList", 50),
        ("removeAllowList", 50),
        ("addDenyList", 50),
        ("removeDenyList", 50),
        ("pause", 50),
        ("unpause", 50),
        ("assignAdminRoles", 50),
        ("revokeAdminRoles", 50),
        ("updateMetadata", 50),
    ] {
        let counts = format!("{{\"{name}\":3}}");
        let (_, value) = request(
            fees::routes(Arc::new(TestNode::default())),
            &query("10", &counts, "CCD"),
        )
        .await;
        // Header 160 + token payload (6 + 10 + 3) + base operation 300.
        assert_eq!(value["energy"], json!(479 + 3 * specific), "{name}");
        assert!(value.get("success").is_none());
    }
    let counts = r#"{"transfer":1,"mint":1,"burn":1,"addAllowList":1,"removeAllowList":1,"addDenyList":1,"removeDenyList":1,"pause":1,"unpause":1,"assignAdminRoles":1,"revokeAdminRoles":1,"updateMetadata":1}"#;
    assert_eq!(
        request(
            fees::routes(Arc::new(TestNode::default())),
            &query("10", counts, "CCD")
        )
        .await,
        (200, json!({"energy":1129,"cost":"753"}))
    );
}

#[tokio::test]
async fn token_size_counts_sponsorship_and_duplicate_keys_keep_legacy_wrap_rules() {
    for (size, counts, token, expected) in [
        ("0", "{}", "", 466u64),
        ("0", "{}", "é", 468),
        ("-10", "{}", "X", 4294967753),
        ("4294967296", "{}", "X", 467),
        ("0", r#"{"transfer":-1}"#, "X", 367),
        ("0", r#"{"transfer":1e2}"#, "X", 10467),
        ("0", r#"{"transfer":1.00}"#, "X", 567),
        ("0", r#"{"transfer":9223372036854775807}"#, "X", 367),
        ("0", r#"{"transfer":-9223372036854775808}"#, "X", 467),
        ("0", r#"{"transfer":1,"transfer":99}"#, "X", 567),
        ("0", r#"{"transfer":1,"transfer":"ignored"}"#, "X", 567),
        ("0", r#"{"transfer":0e1000000}"#, "X", 467),
    ] {
        let raw = query(size, counts, token);
        for (extra, delta) in [("", 0), ("&sponsored=false", 34), ("&sponsored=", 34)] {
            let (status, value) = request(
                fees::routes(Arc::new(TestNode::default())),
                &format!("{raw}{extra}"),
            )
            .await;
            assert_eq!(status, 200, "{raw}");
            assert_eq!(value["energy"], json!(expected + delta), "{raw}");
        }
    }
    assert_eq!(
        request(
            fees::routes(Arc::new(TestNode::default())),
            &query("0", "{}", &"x".repeat(256))
        )
        .await
        .1["energy"],
        722
    );
}

#[tokio::test]
async fn token_estimation_has_no_local_protocol_gate_or_token_identifier_validation() {
    let node = Arc::new(TestNode {
        protocol: ProtocolVersion::P1,
        ..Default::default()
    });
    assert_eq!(
        request(fees::routes(node), &query("0", "{}", "")).await,
        (200, json!({"energy":466,"cost":"311"}))
    );
}

#[tokio::test]
async fn token_request_validation_preserves_order_and_unknown_operation_messages() {
    let node = Arc::new(TestNode::default());
    for (raw, message) in [
        ("type=tokenUpdate".into(), "Missing `listOperationsSize` value.".to_string()),
        ("type=tokenUpdate&listOperationsSize=bad".into(), "Could not parse `listOperationsSize` value.".into()),
        ("type=tokenUpdate&listOperationsSize=0".into(), "Missing `tokenOperationTypeCount` value.".into()),
        ("type=tokenUpdate&listOperationsSize=0&tokenOperationTypeCount=%7B%7D".into(), "Missing `tokenId` value.".into()),
        (query("0", r#"{"zzz":0,"aaa":0}"#, "X"), "Token operation specific cost error: \"Token operation specific cost for operation `aaa` is unknown.\"".into()),
        (query("0", r#"{"lockCreate":0}"#, "X"), "Token operation specific cost error: \"Token operation specific cost for operation `lockCreate` is unknown.\"".into()),
        ("type=tokenUpdate&listOperationsSize=0&tokenOperationTypeCount=%7B%22unknown%22%3A1%7D".into(), "Missing `tokenId` value.".into()),
    ] {
        assert_eq!(request(fees::routes(node.clone()), &raw).await,
            (400, json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})), "{raw}");
    }
}

#[tokio::test]
async fn token_json_type_integer_and_trailing_errors_match_source_derived_messages() {
    for (counts, detail) in [
        (
            "[]",
            "Error in $: parsing Map ~Text failed, expected Object, but encountered Array",
        ),
        (
            "null",
            "Error in $: parsing Map ~Text failed, expected Object, but encountered Null",
        ),
        (
            r#"{"transfer":"1"}"#,
            "Error in $.transfer: parsing Int failed, expected Number, but encountered String",
        ),
        (
            r#"{"transfer":true}"#,
            "Error in $.transfer: parsing Int failed, expected Number, but encountered Boolean",
        ),
        (
            r#"{"transfer":{"$serde_json::private::Number":"1"}}"#,
            "Error in $.transfer: parsing Int failed, expected Number, but encountered Object",
        ),
        (
            r#"{"transfer":"1","transfer":0}"#,
            "Error in $.transfer: parsing Int failed, expected Number, but encountered String",
        ),
        (
            r#"{"transfer":"1"}tail"#,
            "Error in $.transfer: parsing Int failed, expected Number, but encountered String",
        ),
        (
            r#"{"transfer":1.5}"#,
            "Error in $.transfer: parsing Int failed, value is either floating or will cause over or underflow 1.5",
        ),
        (
            r#"{"transfer":9223372036854775808}"#,
            "Error in $.transfer: parsing Int failed, value is either floating or will cause over or underflow 9.223372036854775808e18",
        ),
        (
            r#"{"transfer":1e-3}"#,
            "Error in $.transfer: parsing Int failed, value is either floating or will cause over or underflow 1.0e-3",
        ),
        (r#"{"transfer":0}tail"#, "Trailing garbage"),
        ("", "Unexpected end-of-input, expecting JSON value"),
        ("tru", "Unexpected \"tru\", expecting JSON value"),
        (
            r#"{"transfer":1e}"#,
            "Unexpected '}' while parsing number literal",
        ),
        (
            r#"{"transfer":1.}"#,
            "Unexpected 125 while parsing number literal",
        ),
        (
            r#"{"transfer":1e"#,
            "Unexpected end-of-input while parsing number literal",
        ),
        (
            r#"{"transfer":"\q"}"#,
            "Cannot decode input: Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream",
        ),
        (
            r#"{"transfer":"\q"#,
            "Unexpected end-of-input while parsing string literal",
        ),
        (
            "{\"transfer\":\"\n\"}",
            "Unespected control character while parsing string literal",
        ),
        (
            r#"{"transfer":"\uD800"}"#,
            "Cannot decode input: Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream",
        ),
        (
            "{",
            "Unexpected end-of-input, expecting record key literal or }",
        ),
        ("[", "Unexpected end-of-input, expecting JSON value or ]"),
        (r#"{"transfer":01}"#, "Number literal with leading zero"),
        (r#"{"transfer":}"#, "Unexpected \"}\", expecting JSON value"),
        (r#"{"transfer" 1}"#, "Unexpected \"1}\", expecting :"),
        (
            r#"{"transfer":1,}"#,
            "Unexpected \"}\", expecting key literal",
        ),
    ] {
        // These details contain only ASCII and need the same quote/backslash escapes as Show Text.
        let quoted = format!("\"{}\"", detail.replace('\\', "\\\\").replace('"', "\\\""));
        let message = format!(
            "Parsing input failed: Could not parse `tokenOperationTypeCount` JSON: {quoted}"
        );
        assert_eq!(
            request(
                fees::routes(Arc::new(TestNode::default())),
                &query("0", counts, "X")
            )
            .await,
            (400, json!({"error":1,"errorMessage":message})),
            "{counts}"
        );
    }
}
