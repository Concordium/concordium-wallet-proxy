//! This module checks contract input validation, invocation content, and buffered fees.
//! Rejection and success fixtures use controlled execution energy with no chain effects.

use super::*;
use concordium_rust_sdk::{
    smart_contracts::common::{AccountAddress, Address},
    types::smart_contracts::{ContractContext, InvokeContractResult},
    v2::generated as p,
};
use std::sync::Mutex;

/// This value is the legacy sender for controlled contract invocations.
pub(super) const SENDER: &str = "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3";

/// This adapter records complete invocation contexts and controls execution results.
#[derive(Default)]
struct InvokeNode {
    /// This node supplies the exchange rate and counts context reads.
    base: TestNode,
    /// These contexts retain all validated request contents.
    invocations: Mutex<Vec<ContractContext>>,
    /// This amount of execution energy is returned by the node.
    used: u64,
    /// This flag selects contract rejection rather than execution success.
    reject: bool,
    /// This flag selects an RPC NOT_FOUND response.
    missing: bool,
}
impl FeeNode for InvokeNode {
    async fn fee_context(&self) -> Result<(EnergyRate, ProtocolVersion), NetworkFailure> {
        self.base.fee_context().await
    }
    async fn invoke_contract(
        &self,
        context: &ContractContext,
    ) -> Result<InvokeContractResult, NetworkFailure> {
        self.invocations.lock().unwrap().push(context.clone());
        if self.missing {
            return Err(NetworkFailure::Status(Box::new(tonic::Status::not_found(
                "controlled",
            ))));
        }
        Ok(invocation_result(self.used, self.reject)
            .try_into()
            .unwrap())
    }
}
/// Build an independent protobuf invocation result for SDK conversion.
pub(super) fn invocation_result(used: u64, reject: bool) -> p::InvokeInstanceResponse {
    use p::invoke_instance_response::{Failure, Result, Success};
    p::InvokeInstanceResponse {
        result: Some(if reject {
            Result::Failure(Failure {
                return_value: Some(vec![1, 2]),
                used_energy: Some(p::Energy { value: used }),
                reason: Some(p::RejectReason {
                    reason: Some(p::reject_reason::Reason::RuntimeFailure(p::Empty {})),
                }),
            })
        } else {
            Result::Success(Success {
                return_value: Some(vec![3, 4]),
                used_energy: Some(p::Energy { value: used }),
                effects: vec![],
            })
        }),
    }
}
/// Build an update request with a known 40-byte payload and 200 minimum energy.
pub(super) fn query(extra: &str) -> String {
    format!(
        "type=update&sender={SENDER}&contractIndex=42&contractSubindex=7&amount=123&receiveName=hello.go&parameter=00aAFF{extra}"
    )
}

#[tokio::test]
async fn contract_estimates_preserve_invocation_inputs_defaults_and_rejection_indicators() {
    for reject in [false, true] {
        let node = Arc::new(InvokeNode {
            used: 101,
            reject,
            ..Default::default()
        });
        assert_eq!(
            request(fees::routes(node.clone()), &query("")).await,
            (200, json!({"energy":321,"cost":"214","success":!reject}))
        );
        assert_eq!(node.base.calls.load(Ordering::SeqCst), 1);
        let calls = node.invocations.lock().unwrap();
        assert_eq!(calls.len(), 1);
        let context = &calls[0];
        assert_eq!(
            context.invoker,
            Some(Address::Account(SENDER.parse::<AccountAddress>().unwrap()))
        );
        assert_eq!(context.contract.index, 42);
        assert_eq!(context.contract.subindex, 7);
        assert_eq!(context.amount.micro_ccd, 123);
        assert_eq!(
            context.method.as_receive_name().get_chain_name(),
            "hello.go"
        );
        assert_eq!(context.parameter.as_ref(), [0, 170, 255]);
        assert_eq!(context.energy.unwrap().energy, 500_000);
    }
}

#[tokio::test]
async fn contract_buffers_wrap_multiply_before_division_and_do_not_buffer_header_energy() {
    for (extra, used, energy) in [
        ("&executionNRGBuffer=0", 101u64, 301u64),
        ("&executionNRGBuffer=33", 101, 334),
        ("&executionNRGBuffer=-1", 101, 184467440737095816),
        (
            "&executionNRGBuffer=100&numSignatures=3&sponsored=false",
            101,
            636,
        ),
        ("&executionNRGBuffer=18446744073709551616", 101, 301),
        ("", u64::MAX, 184467440737095714),
        ("&executionNRGBuffer=bad&executionNRGBuffer=0", 0, 0),
    ] {
        let node = Arc::new(InvokeNode {
            used,
            ..Default::default()
        });
        let (status, value) = request(fees::routes(node.clone()), &query(extra)).await;
        if extra.contains("bad") {
            assert_eq!(status, 400);
            assert!(node.invocations.lock().unwrap().is_empty());
        } else {
            assert_eq!(status, 200);
            assert_eq!(value["energy"], json!(energy), "{extra}, {used}");
        }
    }
}

#[tokio::test]
async fn contract_validation_keeps_required_field_order_and_never_invokes_on_bad_input() {
    let node = Arc::new(InvokeNode::default());
    let fields = [
        ("sender", SENDER),
        ("contractIndex", "42"),
        ("contractSubindex", "7"),
        ("amount", "123"),
        ("receiveName", "hello.go"),
        ("parameter", "00"),
    ];
    let mut prefix = String::from("type=update");
    for (field, value) in fields {
        assert_eq!(
            request(fees::routes(node.clone()), &prefix).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: Missing `{field}` value.")})
            )
        );
        prefix.push_str(&format!("&{field}={value}"));
    }
    for (field, value, message) in [
        (
            "sender",
            "nope",
            "Could not parse `sender` value: Base 58 checksum invalid.",
        ),
        (
            "contractIndex",
            "bad",
            "Could not parse `contractIndex` value.",
        ),
        (
            "contractSubindex",
            "bad",
            "Could not parse `contractSubindex` value.",
        ),
        ("amount", "1.5", "Could not parse `amount` value."),
        ("receiveName", "no-dot", "Invalid receive name."),
        ("receiveName", "hello.%C3%A9", "Invalid receive name."),
        ("receiveName", "hello.%20go", "Invalid receive name."),
        (
            "parameter",
            "zz0",
            "Could not parse `parameter` value: invalid bytestring size",
        ),
        (
            "parameter",
            "00gg",
            "Could not parse `parameter` value: invalid character at offset: 2",
        ),
        (
            "executionNRGBuffer",
            "bad",
            "Could not parse `executionNRGBuffer` value.",
        ),
    ] {
        // First query occurrences override the valid suffix.
        let raw = format!("type=update&{field}={value}&{}", query(""));
        assert_eq!(
            request(fees::routes(node.clone()), &raw).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})
            ),
            "{field}"
        );
    }
    assert!(node.invocations.lock().unwrap().is_empty());
}

#[tokio::test]
async fn sender_length_and_version_errors_follow_checksum_validation() {
    for (bytes, message) in [
        (vec![2; 33], "Unknown base58 check version byte."),
        (vec![2; 34], "Wrong address length."),
        (vec![1; 32], "Wrong address length."),
    ] {
        let sender = bs58::encode(bytes).with_check().into_string();
        let raw = format!("type=update&sender={sender}&{}", query(""));
        assert_eq!(
            request(fees::routes(Arc::new(InvokeNode::default())), &raw).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: Could not parse `sender` value: {message}")})
            )
        );
    }
}

#[tokio::test]
async fn contract_word64_values_and_unrestricted_parameter_bytes_reach_the_node() {
    let node = Arc::new(InvokeNode::default());
    let raw = format!(
        "type=update&contractIndex=-1&contractSubindex=18446744073709551616&amount=0x10&parameter={}&{}",
        "ab".repeat(16_000),
        query("")
    );
    assert_eq!(request(fees::routes(node.clone()), &raw).await.0, 200);
    let calls = node.invocations.lock().unwrap();
    assert_eq!(calls[0].contract.index, u64::MAX);
    assert_eq!(calls[0].contract.subindex, 0);
    assert_eq!(calls[0].amount.micro_ccd, 16);
    assert_eq!(calls[0].parameter.as_ref(), vec![0xab; 16_000]);
}

#[tokio::test]
async fn contract_receive_names_and_empty_parameters_keep_the_legacy_boundaries() {
    for (name, status, energy) in [
        (".".to_string(), 200, 190u64),
        (format!(".{}", "x".repeat(99)), 200, 289),
        (format!(".{}", "x".repeat(100)), 400, 0),
    ] {
        let node = Arc::new(InvokeNode::default());
        let raw = format!("type=update&receiveName={name}&parameter=&{}", query(""));
        let (actual, value) = request(fees::routes(node.clone()), &raw).await;
        assert_eq!(actual, status);
        if status == 200 {
            assert_eq!(value["energy"], energy);
            assert!(
                node.invocations.lock().unwrap()[0]
                    .parameter
                    .as_ref()
                    .is_empty()
            );
        } else {
            assert!(node.invocations.lock().unwrap().is_empty());
        }
    }
}

#[tokio::test]
async fn invocation_rpc_errors_are_not_reported_as_execution_rejections() {
    let node = Arc::new(InvokeNode {
        missing: true,
        ..Default::default()
    });
    assert_eq!(
        request(fees::routes(node), &query("")).await,
        (
            404,
            json!({"error":2,"errorMessage":"GRPC call failed: Requested object was not found: controlled"})
        )
    );
}
