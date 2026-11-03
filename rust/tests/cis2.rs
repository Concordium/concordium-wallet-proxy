//! These tests check legacy CIS-2 parsing, binary codecs, and versioned invocation behavior.
//! Controlled node adapters never submit transactions or query a public node.

mod postgres;
mod rpc;

use axum::{Router, body::Body};
use concordium_rust_sdk::{
    types::{
        ContractAddress, Energy,
        hashes::BlockHash,
        smart_contracts::{ContractContext, InvokeContractResult, OwnedContractName, ReturnValue},
    },
    v2::Upward,
};
use concordium_wallet_proxy::{
    cis2::{self, Cis2History, Cis2Node, ContractSnapshot, IndexedToken},
    network::NetworkFailure,
};
use http_body_util::BodyExt;
use serde_json::{Value, json};
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};
use tower::ServiceExt;

/// This source fixture address is serialized verbatim, not canonicalized.
const ACCOUNT: &str = "2xBpaHottqhwFZURMZW4uZduQvpxNDSy46iXMYs9kceNGaPpZX";

/// This adapter forbids database access from contract metadata and balance requests.
struct NoHistory;
impl Cis2History for NoHistory {
    async fn contract_tokens(
        &self,
        _: ContractAddress,
        _: Option<i64>,
        _: u16,
    ) -> anyhow::Result<Vec<IndexedToken>> {
        panic!("Unexpected database access");
    }
}

/// This adapter records complete invocation contexts and returns a controlled sequence.
struct Node {
    /// These results are consumed once, so unexpected retries fail tests.
    replies: Mutex<VecDeque<Result<InvokeContractResult, NetworkFailure>>>,
    /// These targets retain instance-information lookups.
    snapshots: Mutex<Vec<ContractAddress>>,
    /// These block/context pairs retain all read-only invocations.
    invocations: Mutex<Vec<(BlockHash, ContractContext)>>,
    /// This code can fail instance lookup before invocation.
    snapshot_failure: Option<tonic::Code>,
    /// This name can exercise unchecked receive-name construction.
    name: String,
}
impl Cis2Node for Node {
    async fn contract_snapshot(
        &self,
        address: ContractAddress,
    ) -> Result<ContractSnapshot, NetworkFailure> {
        self.snapshots.lock().unwrap().push(address);
        if let Some(code) = self.snapshot_failure {
            return Err(status(code));
        }
        Ok(ContractSnapshot {
            block: [9; 32].into(),
            name: OwnedContractName::new(self.name.clone()).unwrap(),
        })
    }
    async fn cis2_invoke(
        &self,
        block: BlockHash,
        context: &ContractContext,
    ) -> Result<InvokeContractResult, NetworkFailure> {
        self.invocations
            .lock()
            .unwrap()
            .push((block, context.clone()));
        self.replies
            .lock()
            .unwrap()
            .pop_front()
            .expect("Unexpected invocation")
    }
}
fn node(replies: Vec<Result<InvokeContractResult, NetworkFailure>>) -> Arc<Node> {
    Arc::new(Node {
        replies: Mutex::new(replies.into()),
        snapshots: Mutex::new(vec![]),
        invocations: Mutex::new(vec![]),
        snapshot_failure: None,
        name: "init_token".into(),
    })
}
fn status(code: tonic::Code) -> NetworkFailure {
    NetworkFailure::Status(tonic::Status::new(code, "controlled").into())
}
fn success(bytes: Option<Vec<u8>>) -> Result<InvokeContractResult, NetworkFailure> {
    Ok(InvokeContractResult::Success {
        return_value: bytes.map(|value| ReturnValue { value }),
        events: vec![],
        used_energy: Energy { energy: 1 },
    })
}
fn failure() -> Result<InvokeContractResult, NetworkFailure> {
    Ok(InvokeContractResult::Failure {
        return_value: None,
        reason: Upward::Known(concordium_rust_sdk::types::RejectReason::OutOfEnergy),
        used_energy: Energy { energy: 1 },
    })
}
fn app(node: Arc<Node>) -> Router {
    cis2::routes(node, Arc::new(NoHistory))
}
async fn request(router: Router, path: &str) -> (u16, Value) {
    let response = router
        .oneshot(http::Request::get(path).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    if status == 200 {
        assert!(!response.headers().contains_key("content-language"));
    } else {
        assert_eq!(response.headers()["content-language"], "en");
    }
    (
        status,
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap(),
    )
}
/// Encode one source-defined metadata reference without using the production codec.
fn metadata(url: &str, checksum: Option<[u8; 32]>) -> Vec<u8> {
    let mut bytes = (url.len() as u16).to_le_bytes().to_vec();
    bytes.extend(url.as_bytes());
    bytes.push(u8::from(checksum.is_some()));
    if let Some(checksum) = checksum {
        bytes.extend(checksum);
    }
    bytes
}
fn metadata_result(url: &str) -> Vec<u8> {
    let mut bytes = vec![1, 0];
    bytes.extend(metadata(url, None));
    bytes
}

#[tokio::test]
async fn batch_metadata_preserves_ids_checksum_nulls_zip_truncation_and_pinned_context() {
    for count in [0_u16, 1, 2, 3] {
        let mut bytes = count.to_le_bytes().to_vec();
        for index in 0..count {
            bytes.extend(metadata(
                &format!("https://example.org/{index}"),
                (index == 1).then_some([0xab; 32]),
            ));
        }
        bytes.extend([255, 255]);
        let node = node(vec![success(Some(bytes))]);
        let (status, body) = request(
            app(node.clone()),
            "/v0/CIS2TokenMetadata/42/7?tokenId=AA,00ff&tokenId=11",
        )
        .await;
        let mut expected = vec![];
        if count > 0 {
            expected.push(json!({"tokenId":"aa","metadataURL":"https://example.org/0","metadataChecksum":null}));
        }
        if count > 1 {
            expected.push(json!({"tokenId":"00ff","metadataURL":"https://example.org/1","metadataChecksum":"ab".repeat(32)}));
        }
        assert_eq!(
            (status, body),
            (200, json!({"contractName":"token","metadata":expected}))
        );
        assert_eq!(
            *node.snapshots.lock().unwrap(),
            [ContractAddress {
                index: 42,
                subindex: 7
            }]
        );
        let calls = node.invocations.lock().unwrap();
        let (block, context) = &calls[0];
        assert_eq!(*block, [9; 32].into());
        assert_eq!(
            context.contract,
            ContractAddress {
                index: 42,
                subindex: 7
            }
        );
        assert!(context.invoker.is_none());
        assert_eq!(context.amount.micro_ccd, 0);
        assert_eq!(context.method.to_string(), "token.tokenMetadata");
        assert_eq!(context.energy.unwrap().energy, 500_000);
        assert_eq!(context.parameter.as_ref(), [2, 0, 1, 170, 2, 0, 255]);
    }
}

#[tokio::test]
async fn balances_preserve_account_bytes_duplicates_padding_and_259_bit_precision() {
    let mut bytes = vec![2, 0];
    bytes.extend([0xff; 36]);
    bytes.push(0x7f);
    bytes.extend([0x80, 0]);
    bytes.push(99);
    let node = node(vec![success(Some(bytes))]);
    let (_, body) = request(
        app(node.clone()),
        &format!("/v0/CIS2TokenBalance/42/7/{ACCOUNT}?tokenId=,AA"),
    )
    .await;
    let maximum = ((num::BigUint::from(1_u8) << 259_usize) - num::BigUint::from(1_u8)).to_string();
    assert_eq!(
        body,
        json!([{"tokenId":"","balance":maximum},{"tokenId":"aa","balance":"0"}])
    );
    let calls = node.invocations.lock().unwrap();
    let context = &calls[0].1;
    let mut expected = vec![2, 0, 0, 0];
    expected.extend([1; 32]);
    expected.extend([1, 170, 0]);
    expected.extend([1; 32]);
    assert_eq!(context.parameter.as_ref(), expected);
    assert_eq!(context.method.to_string(), "token.balanceOf");
}

#[tokio::test]
async fn partial_versions_query_sequential_singletons_and_omit_only_contract_failures() {
    for balance in [false, true] {
        let valid = if balance {
            vec![1, 0, 5]
        } else {
            metadata_result("ipfs://five")
        };
        let node = node(vec![
            success(Some(valid.clone())),
            failure(),
            success(None),
            success(Some(vec![0, 0])),
            success(Some(valid)),
        ]);
        let path = if balance {
            format!("/v1/CIS2TokenBalance/42/7/{ACCOUNT}?tokenId=aa,bb,cc,dd,aa")
        } else {
            "/v1/CIS2TokenMetadata/42/7?tokenId=aa,bb,cc,dd,aa".into()
        };
        let (_, body) = request(app(node.clone()), &path).await;
        let expected = if balance {
            json!([{"tokenId":"aa","balance":"5"},{"tokenId":"aa","balance":"5"}])
        } else {
            json!({"contractName":"token","metadata":[{"tokenId":"aa","metadataURL":"ipfs://five","metadataChecksum":null},{"tokenId":"aa","metadataURL":"ipfs://five","metadataChecksum":null}]})
        };
        assert_eq!(body, expected);
        assert_eq!(node.snapshots.lock().unwrap().len(), 1);
        let calls = node.invocations.lock().unwrap();
        assert_eq!(calls.len(), 5);
        for ((block, context), token) in calls.iter().zip([0xaa, 0xbb, 0xcc, 0xdd, 0xaa]) {
            assert_eq!(*block, [9; 32].into());
            assert_eq!(context.energy.unwrap().energy, 50_000);
            let mut parameter = vec![1, 0, 1, token];
            if balance {
                parameter.push(0);
                parameter.extend([1; 32]);
            }
            assert_eq!(context.parameter.as_ref(), parameter);
        }
    }
}

#[tokio::test]
async fn partial_versions_require_singletons_and_skip_malformed_bytes_before_later_success() {
    for balance in [false, true] {
        let too_many = if balance {
            vec![2, 0, 1, 2]
        } else {
            let mut bytes = vec![2, 0];
            bytes.extend(metadata("one", None));
            bytes.extend(metadata("two", None));
            bytes
        };
        let malformed = if balance {
            let mut bytes = vec![1, 0];
            bytes.extend([128; 37]);
            bytes
        } else {
            vec![1, 0, 1, 0, 255, 0]
        };
        let valid = if balance {
            vec![1, 0, 7]
        } else {
            metadata_result("last")
        };
        let node = node(vec![
            success(Some(too_many)),
            success(Some(malformed)),
            success(Some(valid)),
        ]);
        let path = if balance {
            format!("/v1/CIS2TokenBalance/42/7/{ACCOUNT}?tokenId=aa,bb,cc")
        } else {
            "/v1/CIS2TokenMetadata/42/7?tokenId=aa,bb,cc".into()
        };
        let (_, body) = request(app(node), &path).await;
        assert_eq!(
            body,
            if balance {
                json!([{"tokenId":"cc","balance":"7"}])
            } else {
                json!({"contractName":"token","metadata":[{"tokenId":"cc","metadataURL":"last","metadataChecksum":null}]})
            }
        );
    }
}

#[tokio::test]
async fn account_input_shape_and_alias_bytes_are_preserved_before_token_parsing() {
    for (bytes, message) in [
        (vec![1; 4], "Wrong address length."),
        (
            {
                let mut bytes = vec![1; 33];
                bytes[0] = 2;
                bytes
            },
            "Unknown base58 check version byte.",
        ),
    ] {
        let account = bs58::encode(bytes).with_check().into_string();
        let node = node(vec![]);
        assert_eq!(
            request(
                app(node.clone()),
                &format!("/v0/CIS2TokenBalance/42/7/{account}")
            )
            .await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})
            )
        );
        assert!(node.snapshots.lock().unwrap().is_empty());
    }
    let mut alias = [1; 32];
    alias[31] = 99;
    let account = concordium_rust_sdk::id::types::AccountAddress(alias).to_string();
    let node = node(vec![success(Some(vec![1, 0, 1]))]);
    assert_eq!(
        request(
            app(node.clone()),
            &format!("/v0/CIS2TokenBalance/42/7/{account}?tokenId=")
        )
        .await
        .0,
        200
    );
    let calls = node.invocations.lock().unwrap();
    assert_eq!(&calls[0].1.parameter.as_ref()[4..], alias);
}

#[tokio::test]
async fn rpc_errors_abort_partial_results_and_not_found_keeps_custom_error() {
    for (code, http) in [
        (tonic::Code::NotFound, 404),
        (tonic::Code::Unavailable, 502),
        (tonic::Code::ResourceExhausted, 503),
    ] {
        let node = node(vec![
            success(Some(metadata_result("ok"))),
            Err(status(code)),
            success(Some(metadata_result("not queried"))),
        ]);
        let (status, body) = request(
            app(node.clone()),
            "/v1/CIS2TokenMetadata/42/7?tokenId=aa,bb,cc",
        )
        .await;
        assert_eq!(status, http);
        assert!(body.get("metadata").is_none());
        assert_eq!(node.invocations.lock().unwrap().len(), 2);
        if code == tonic::Code::NotFound {
            assert_eq!(body, json!({"error":2,"errorMessage":"Not found"}));
        }
        let mut node = self::node(vec![]);
        Arc::get_mut(&mut node).unwrap().snapshot_failure = Some(code);
        assert_eq!(
            request(app(node.clone()), "/v0/CIS2TokenMetadata/42/7?tokenId=")
                .await
                .0,
            http
        );
        assert!(node.invocations.lock().unwrap().is_empty());
    }
}

#[tokio::test]
async fn batch_failure_missing_return_and_malformed_results_have_distinct_legacy_errors() {
    for (reply, message) in [
        (failure(), "Invoking a contract failed."),
        (success(None), "Invoking a V0 contract is not supported."),
        (
            success(Some(vec![1, 0, 255])),
            "Invoking a contract failed.",
        ),
    ] {
        assert_eq!(
            request(
                app(node(vec![reply])),
                "/v0/CIS2TokenMetadata/42/7?tokenId=aa"
            )
            .await,
            (400, json!({"error":1,"errorMessage":message}))
        );
    }
    for bytes in [vec![0, 0], vec![1, 0], {
        let mut bytes = vec![1, 0];
        bytes.extend([128; 37]);
        bytes.push(0);
        bytes
    }] {
        assert_eq!(
            request(
                app(node(vec![success(Some(bytes))])),
                &format!("/v0/CIS2TokenBalance/42/7/{ACCOUNT}?tokenId=")
            )
            .await
            .0,
            400
        );
    }
    for bytes in [
        vec![1, 0, 1, 0, 255, 0],
        vec![1, 0, 0, 0, 2],
        vec![1, 0, 0, 0, 1, 0],
        {
            let mut bytes = vec![2, 0];
            bytes.extend(metadata("valid but not sufficient", None));
            bytes.extend([1, 0, 255, 0]);
            bytes
        },
    ] {
        assert_eq!(
            request(
                app(node(vec![success(Some(bytes))])),
                "/v0/CIS2TokenMetadata/42/7?tokenId="
            )
            .await
            .0,
            400
        );
    }
}

#[tokio::test]
async fn parser_precedence_count_limits_and_word_paths_prevent_external_queries() {
    for (path, message) in [
        ("/v0/CIS2TokenMetadata/42/7", "Missing `tokenId` value."),
        (
            "/v1/CIS2TokenMetadata/42/7?tokenId=gg",
            "Error in $: expected Not a valid Base16 encoding., but encountered String",
        ),
        ("/v0/CIS2TokenBalance/42/7/bad", "Base 58 checksum invalid."),
    ] {
        let node = node(vec![]);
        assert_eq!(
            request(app(node.clone()), path).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})
            )
        );
        assert!(node.snapshots.lock().unwrap().is_empty());
    }
    for (tokens, partial, message) in [
        (vec![""; 21].join(","), true, "Too many tokens requested."),
        (
            format!("{},gg", vec![""; 20].join(",")),
            true,
            "Error in $: expected Not a valid Base16 encoding., but encountered String",
        ),
    ] {
        let node = node(vec![]);
        let path = format!(
            "/v{}/CIS2TokenMetadata/42/7?tokenId={tokens}",
            u8::from(partial)
        );
        assert_eq!(
            request(app(node.clone()), &path).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})
            )
        );
        assert!(node.snapshots.lock().unwrap().is_empty());
    }
    for path in [
        "/v0/CIS2TokenMetadata/-1/7?tokenId=",
        "/v0/CIS2TokenMetadata/18446744073709551616/7?tokenId=",
        "/v0/CIS2TokenBalance/bad/7/bad",
    ] {
        assert_eq!(request(app(node(vec![])), path).await.0, 404);
    }
    let node = node(vec![success(Some(vec![0, 0]))]);
    assert_eq!(
        request(app(node.clone()), "/v0/CIS2TokenMetadata/-0/+7?tokenId=")
            .await
            .0,
        200
    );
    assert_eq!(
        node.snapshots.lock().unwrap()[0],
        ContractAddress {
            index: 0,
            subindex: 7
        }
    );
}

#[tokio::test]
async fn unrestricted_token_lengths_parameters_and_receive_names_retain_legacy_wrapping() {
    let mut node = node(vec![success(Some(metadata_result("ok")))]);
    Arc::get_mut(&mut node).unwrap().name = format!("init_{}", "a".repeat(95));
    let token = "ab".repeat(256);
    let (_, body) = request(
        app(node.clone()),
        &format!("/v0/CIS2TokenMetadata/42/7?tokenId={token}"),
    )
    .await;
    assert_eq!(body["metadata"][0]["tokenId"], token);
    {
        let calls = node.invocations.lock().unwrap();
        assert_eq!(&calls[0].1.parameter.as_ref()[..3], [1, 0, 0]);
        assert_eq!(calls[0].1.parameter.as_ref().len(), 259);
        assert!(calls[0].1.method.to_string().len() > 100);
    }
    let mut bytes = 2000_u16.to_le_bytes().to_vec();
    bytes.extend(vec![0; 2000]);
    let node = self::node(vec![success(Some(bytes))]);
    let query = vec![""; 2000].join(",");
    assert_eq!(
        request(
            app(node.clone()),
            &format!("/v0/CIS2TokenBalance/42/7/{ACCOUNT}?tokenId={query}")
        )
        .await
        .0,
        200
    );
    assert_eq!(
        node.invocations.lock().unwrap()[0]
            .1
            .parameter
            .as_ref()
            .len(),
        68_002
    );
}
