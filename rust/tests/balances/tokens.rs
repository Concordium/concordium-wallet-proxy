//! This module checks token-inclusive balances and legacy CBOR state projection.
//! Input bytes and expected JSON are independent of replacement serializers.

use super::*;
use concordium_rust_sdk::v2::generated::plt;
use concordium_wallet_proxy::{config::NodeConfig, dependencies::Production, node::SdkNode};

fn unhex(text: &str) -> Vec<u8> {
    (0..text.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&text[i..i + 2], 16).unwrap())
        .collect()
}
fn text(value: &str) -> Vec<u8> {
    concordium_rust_sdk::common::cbor::cbor_encode(&value.to_string())
}
fn map(entries: Vec<(&str, Vec<u8>)>) -> Vec<u8> {
    assert!(entries.len() < 24);
    let mut out = vec![0xa0 | entries.len() as u8];
    for (key, value) in entries {
        out.extend(text(key));
        out.extend(value);
    }
    out
}
fn token(id: &str, amount: u64, decimals: u32, state: Option<Vec<u8>>) -> p::account_info::Token {
    p::account_info::Token {
        token_id: Some(plt::TokenId { value: id.into() }),
        token_account_state: Some(plt::TokenAccountState {
            balance: Some(plt::TokenAmount {
                value: amount,
                decimals,
            }),
            module_state: state.map(|value| plt::Cbor { value }),
        }),
    }
}
fn info(id: &str, state: Vec<u8>) -> p::TokenInfo {
    p::TokenInfo {
        token_id: Some(plt::TokenId { value: id.into() }),
        token_state: Some(plt::TokenState {
            token_module_ref: Some(plt::TokenModuleRef { value: vec![2; 32] }),
            decimals: 2,
            total_supply: Some(plt::TokenAmount {
                value: u64::MAX,
                decimals: 2,
            }),
            module_state: Some(plt::Cbor { value: state }),
        }),
    }
}
fn node(global: Vec<u8>, account: Option<Vec<u8>>) -> TestNode {
    let mut node = TestNode::default();
    node.finalized.as_mut().unwrap().tokens = vec![token("ZED", 9_007_199_254_740_993, 2, account)];
    node.token_infos.insert("ZED".into(), info("ZED", global));
    node
}
fn token_value(global: Value, account: Option<Value>) -> Value {
    let mut account_value = json!({"balance":{"value":"9007199254740993","decimals":2}});
    if let Some(state) = account {
        account_value["state"] = state;
    }
    json!({"token":{"tokenId":"ZED","tokenState":{"tokenModuleRef":"02".repeat(32),"decimals":2,"totalSupply":{"value":"18446744073709551615","decimals":2},"moduleState":global}},"tokenAccountState":account_value})
}
fn complete(tokens: Vec<Value>) -> Value {
    let mut value = expected(1);
    value["accountTokens"] = json!(tokens);
    json!({"finalizedBalance":value})
}
fn holding(value: &Value) -> &Value {
    &value["finalizedBalance"]["accountTokens"][0]
}

// Published SDK CBOR vector, also valid under the Haskell token-module schema.
const GLOBAL: &str = "a8646e616d6563544b3166706175736564f4686275726e61626c65f56864656e794c697374f5686d65746164617461a26375726c7168747470733a2f2f746f6b656e75726c316e636865636b73756d53686132353658200101010101010101010101010101010101010101010101010101010101010101686d696e7461626c65f569616c6c6f774c697374f571676f7665726e616e63654163636f756e74d99d73a201d99d71a101190397035820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff";

#[tokio::test]
async fn source_derived_state_fixtures_are_exercised_through_http() {
    let cases: Value =
        serde_json::from_str(include_str!("../fixtures/tokens/module-state.json")).unwrap();
    for case in cases.as_array().unwrap() {
        let raw = unhex(case["cbor"].as_str().unwrap());
        let (_, value) = request(node(raw.clone(), Some(raw)), 2).await;
        assert_eq!(
            holding(&value)["token"]["tokenState"]["moduleState"],
            case["module"],
            "{case}"
        );
        assert_eq!(
            holding(&value)["tokenAccountState"]["state"],
            case["account"],
            "{case}"
        );
    }
}

#[tokio::test]
async fn multiple_tokens_preserve_ccd_state_order_amounts_metadata_and_optional_account_state() {
    let state = map(vec![("allowList", vec![0xf4]), ("denyList", vec![0xf5])]);
    let mut node = node(unhex(GLOBAL), Some(state));
    node.finalized
        .as_mut()
        .unwrap()
        .tokens
        .push(token("AAA", 0, 255, None));
    let mut second = info("AAA", vec![0xa0]);
    let ts = second.token_state.as_mut().unwrap();
    ts.decimals = 255;
    ts.total_supply = Some(plt::TokenAmount {
        value: 0,
        decimals: 0,
    });
    node.token_infos.insert("AAA".into(), second);
    let first = token_value(
        json!({"name":"TK1","paused":false,"burnable":true,"denyList":true,"mintable":true,"allowList":true,
            "metadata":{"url":"https://tokenurl1","checksumSha256":"01".repeat(32)},
            "governanceAccount":{"type":"account","address":AccountAddress([0xff;32]).to_string(),"coinInfo":"CCD"}
        }),
        Some(json!({"allowList":false,"denyList":true})),
    );
    let second = json!({"token":{"tokenId":"AAA","tokenState":{"tokenModuleRef":"02".repeat(32),"decimals":255,"totalSupply":{"value":"0","decimals":0},"moduleState":{}}},"tokenAccountState":{"balance":{"value":"0","decimals":255}}});
    assert_eq!(
        request(node.clone(), 2).await,
        (200, complete(vec![first, second]))
    );
    assert_eq!(
        node.calls(),
        [
            "consensus",
            "account:finalized",
            "tokenomics",
            "token:ZED",
            "token:AAA"
        ]
    );
}

#[tokio::test]
async fn empty_holdings_missing_accounts_and_old_versions_do_not_query_tokens() {
    for protocol in [2, 4, 6, 9, 10] {
        let node = TestNode {
            protocol,
            ..Default::default()
        };
        assert_eq!(request(node.clone(), 2).await, (200, complete(vec![])));
        assert_eq!(
            node.calls(),
            ["consensus", "account:finalized", "tokenomics"]
        );
    }
    let missing = TestNode {
        finalized: None,
        ..Default::default()
    };
    assert_eq!(request(missing.clone(), 2).await, (200, json!({})));
    assert_eq!(missing.calls(), ["consensus", "account:finalized"]);
    for version in [0, 1] {
        let node = node(vec![0xa0], None);
        let (status, value) = request(node.clone(), version).await;
        assert_eq!(status, 200);
        assert!(value["finalizedBalance"].get("accountTokens").is_none());
        assert!(!node.calls().iter().any(|call| call.starts_with("token:")));
    }
}

#[tokio::test]
async fn account_state_absence_empty_and_invalid_cbor_remain_distinct() {
    for (raw, json) in [
        (None, None),
        (Some(vec![0xa0]), Some(json!({}))),
        (Some(vec![]), Some(json!(""))),
        (Some(vec![0xff]), Some(json!("ff"))),
    ] {
        let node = node(vec![0xa0], raw);
        assert_eq!(
            request(node, 2).await,
            (200, complete(vec![token_value(json!({}), json)]))
        );
    }
}

#[tokio::test]
async fn known_properties_are_omitted_not_defaulted_and_invalid_types_fall_back_to_hex() {
    for flag in [0xf4, 0xf5] {
        let raw = map(vec![("allowList", vec![flag])]);
        let (_, value) = request(node(raw.clone(), Some(raw)), 2).await;
        assert_eq!(
            holding(&value)["token"]["tokenState"]["moduleState"],
            json!({"allowList":flag==0xf5})
        );
        assert_eq!(
            holding(&value)["tokenAccountState"]["state"],
            json!({"allowList":flag==0xf5})
        );
    }
    let duplicate = map(vec![("allowList", vec![0xf5]), ("allowList", vec![0xf4])]);
    for raw in [
        map(vec![("allowList", vec![0xf6])]),
        duplicate,
        vec![0xa0, 0x00],
        vec![0x01],
        vec![0xa1, 0x01, 0xf5],
        vec![0x5f, 0xff],
        unhex("5bffffffffffffffff"),
        unhex("9bffffffffffffffff"),
        unhex("bbffffffffffffffff"),
        unhex("d8ff"),
    ] {
        let (_, value) = request(node(raw.clone(), Some(raw.clone())), 2).await;
        assert_eq!(
            holding(&value)["token"]["tokenState"]["moduleState"],
            hex(&raw)
        );
        assert_eq!(holding(&value)["tokenAccountState"]["state"], hex(&raw));
    }
}

#[tokio::test]
async fn additional_fields_are_dropped_including_p11_extensions() {
    for raw in [
        "1801",
        "9f0102ff",
        "a2616201616102",
        "5f41014102ff",
        "7f61616162ff",
        "f93c00",
        "fa3f800000",
        "fb3ff0000000000000",
        "c249010000000000000000",
    ]
    .map(unhex)
    {
        let state = map(vec![("_x", raw)]);
        let (_, value) = request(node(state.clone(), Some(state)), 2).await;
        assert_eq!(
            holding(&value)["token"]["tokenState"]["moduleState"],
            json!({})
        );
        assert_eq!(holding(&value)["tokenAccountState"]["state"], json!({}));
    }
    let state = map(vec![
        ("available", unhex("c482211864")),
        ("locks", vec![0x80]),
    ]);
    let (_, value) = request(node(vec![0xa0], Some(state)), 2).await;
    assert_eq!(holding(&value)["tokenAccountState"]["state"], json!({}));
    let state = map(vec![
        ("locks", vec![0xf5]),
        ("available", text("ignored")),
        ("allowList", vec![0xf4]),
    ]);
    let (_, value) = request(node(state.clone(), Some(state)), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        json!({"allowList":false})
    );
    assert_eq!(
        holding(&value)["tokenAccountState"]["state"],
        json!({"allowList":false})
    );
}

#[tokio::test]
async fn indefinite_known_strings_maps_and_duplicate_outer_keys_match_legacy_decoding() {
    let state = unhex("bf646e616d657f61616162ffff");
    let (_, value) = request(node(state, None), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        json!({"name":"ab"})
    );
    // The outer map uses decodeString for its keys, unlike nested metadata.
    let state = unhex("a17f646e616d65ff6161");
    let (_, value) = request(node(state.clone(), None), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        hex(&state)
    );
}

#[tokio::test]
async fn metadata_and_governance_validation_use_the_haskell_schema() {
    let metadata = map(vec![
        ("url", text("https://example")),
        ("_checksum", vec![0x01]),
    ]);
    let state = map(vec![("metadata", metadata)]);
    let (_, value) = request(node(state, None), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        json!({"metadata":{"url":"https://example"}})
    );
    let metadata = map(vec![("url", vec![0xf4]), ("url", text("last"))]);
    let (_, value) = request(node(map(vec![("metadata", metadata)]), None), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        json!({"metadata":{"url":"last"}})
    );
    for metadata in [
        vec![0xa0],
        map(vec![("url", vec![0xf5])]),
        map(vec![("url", text("ok")), ("checksumSha256", vec![0x40])]),
    ] {
        let state = map(vec![("metadata", metadata)]);
        let (_, value) = request(node(state.clone(), None), 2).await;
        assert_eq!(
            holding(&value)["token"]["tokenState"]["moduleState"],
            hex(&state)
        );
    }
    let mut short = unhex("d99d73a1035820");
    short.extend([1; 32]);
    let state = map(vec![("governanceAccount", short)]);
    let (_, value) = request(node(state, None), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        json!({"governanceAccount":{"type":"account","address":AccountAddress([1;32]).to_string()}})
    );
    let state = map(vec![("governanceAccount", vec![0xa0])]);
    let (_, value) = request(node(state.clone(), None), 2).await;
    assert_eq!(
        holding(&value)["token"]["tokenState"]["moduleState"],
        hex(&state)
    );
}

#[tokio::test]
async fn enrichment_stays_sequential_preserves_duplicates_and_fails_whole_response() {
    let mut controlled = node(vec![0xa0], None);
    controlled
        .finalized
        .as_mut()
        .unwrap()
        .tokens
        .extend([token("AAA", 1, 0, None), token("ZED", 2, 0, None)]);
    controlled
        .token_infos
        .insert("AAA".into(), info("AAA", vec![0xa0]));
    controlled.finalized.as_mut().unwrap().stake = Some(fixtures::delegation(
        None,
        Some(fixtures::pending(true, fixtures::NEXT)),
    ));
    assert_eq!(request(controlled.clone(), 2).await.0, 200);
    assert_eq!(
        controlled.calls(),
        [
            "consensus",
            "account:finalized",
            "tokenomics",
            "token:ZED",
            "token:AAA",
            "token:ZED",
            "parameters"
        ]
    );
    for (code, status, error, message) in [
        (
            tonic::Code::NotFound,
            404,
            2,
            "Requested object was not found: controlled",
        ),
        (
            tonic::Code::Cancelled,
            503,
            3,
            "The node is overloaded so the request was cancelled: controlled",
        ),
        (
            tonic::Code::Unavailable,
            502,
            0,
            "Non-OK GRPC status code 'UNAVAILABLE'.",
        ),
    ] {
        let mut controlled = node(vec![0xa0], None);
        controlled
            .finalized
            .as_mut()
            .unwrap()
            .tokens
            .extend([token("AAA", 1, 0, None), token("AFTER", 1, 0, None)]);
        controlled.failure = Some(("token:AAA", code));
        assert_eq!(
            request(controlled.clone(), 2).await,
            (
                status,
                json!({"error":error,"errorMessage":format!("GRPC call failed: {message}")})
            )
        );
        assert_eq!(
            controlled.calls(),
            [
                "consensus",
                "account:finalized",
                "tokenomics",
                "token:ZED",
                "token:AAA"
            ]
        );
    }
}

#[tokio::test]
async fn sdk_decoder_limits_use_hex_fallback_without_a_private_http_error_policy() {
    let mut nested = vec![0x81; 127];
    nested.push(0);
    let state = map(vec![("_nested", nested.clone())]);
    assert_eq!(request(node(state, None), 2).await.0, 200);
    nested.insert(0, 0x81);
    let state = map(vec![("_nested", nested)]);
    assert_eq!(
        request(node(state.clone(), None), 2).await,
        (200, complete(vec![token_value(json!(hex(&state)), None)]))
    );
    let mut controlled = node(state, None);
    controlled
        .finalized
        .as_mut()
        .unwrap()
        .tokens
        .push(token("AAA", 0, 0, None));
    // Complete lookups first: a later missing token takes precedence over the
    // decoder-limited state in an earlier token, rather than returning a partial list.
    assert_eq!(request(controlled.clone(), 2).await.0, 404);
    assert_eq!(
        controlled.calls(),
        [
            "consensus",
            "account:finalized",
            "tokenomics",
            "token:ZED",
            "token:AAA"
        ]
    );
}

#[tokio::test]
async fn real_sdk_token_queries_use_last_final_and_http_returns_enriched_state() {
    let controlled = node(unhex(GLOBAL), Some(vec![0xa0]));
    let server = super::rpc::server(controlled.clone()).await;
    let sdk = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    let http = crate::support::serve_router(app::<Production>(sdk)).await;
    let response = reqwest::get(format!("{}{}", http.endpoint, path(2)))
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    let value: Value = response.json().await.unwrap();
    assert_eq!(
        holding(&value)["token"]["tokenState"]["totalSupply"],
        json!({"value":"18446744073709551615","decimals":2})
    );
    assert_eq!(holding(&value)["tokenAccountState"]["state"], json!({}));
    assert_eq!(
        controlled.calls(),
        ["consensus", "account:finalized", "tokenomics", "token:ZED"]
    );
}

#[tokio::test]
async fn real_sdk_failure_does_not_return_a_partial_token_list() {
    let mut controlled = node(vec![0xa0], None);
    controlled
        .finalized
        .as_mut()
        .unwrap()
        .tokens
        .push(token("AAA", 0, 0, None));
    controlled.failure = Some(("token:AAA", tonic::Code::Unavailable));
    let server = super::rpc::server(controlled.clone()).await;
    let sdk = SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
        .await
        .unwrap();
    let response = body(
        app::<Production>(sdk)
            .oneshot(Request::get(path(2)).body(Body::empty()).unwrap())
            .await
            .unwrap(),
    )
    .await;
    assert_eq!(
        response,
        (
            502,
            json!({"error":0,"errorMessage":"GRPC call failed: Non-OK GRPC status code 'UNAVAILABLE'."})
        )
    );
    assert_eq!(
        controlled.calls(),
        [
            "consensus",
            "account:finalized",
            "tokenomics",
            "token:ZED",
            "token:AAA"
        ]
    );
}
