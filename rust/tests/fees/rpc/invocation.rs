//! This module checks contract estimation through local SDK invocation transport.
//! It validates complete protobuf inputs and distinguishes execution failure from RPC failure.

use super::*;
use crate::fees::contract;

/// Serve a controlled invocation without submitting a transaction.
pub(super) async fn invoke(State(state): State<Arc<RpcState>>, request: Request) -> Response {
    if state.delayed_invoke {
        tokio::time::sleep(std::time::Duration::from_millis(1100)).await;
    }
    dispatch(state, request, |state, input: p::InvokeInstanceRequest| {
        assert!(matches!(
            input.block_hash.unwrap().block_hash_input,
            Some(p::block_hash_input::BlockHashInput::Best(_))
        ));
        let sender = contract::SENDER
            .parse::<concordium_rust_sdk::id::types::AccountAddress>()
            .unwrap();
        assert_eq!(
            input.invoker.unwrap().r#type,
            Some(p::address::Type::Account(p::AccountAddress {
                value: sender.0.to_vec()
            }))
        );
        assert_eq!(
            input.instance,
            Some(p::ContractAddress {
                index: 42,
                subindex: 7
            })
        );
        assert_eq!(input.amount, Some(p::Amount { value: 123 }));
        assert_eq!(
            input.entrypoint,
            Some(p::ReceiveName {
                value: "hello.go".into()
            })
        );
        assert_eq!(
            input.parameter,
            Some(p::Parameter {
                value: vec![0, 170, 255]
            })
        );
        assert_eq!(input.energy, Some(p::Energy { value: 500_000 }));
        record(state, "invoke:best")?;
        Ok(if state.malformed_invoke {
            p::InvokeInstanceResponse::default()
        } else {
            contract::invocation_result(101, state.reject)
        })
    })
    .await
}

#[tokio::test]
async fn sdk_contract_estimates_preserve_best_context_and_success_indicators() {
    for reject in [false, true] {
        let state = Arc::new(RpcState {
            reject,
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap(),
        );
        let http = support::serve_router(fees::routes(sdk)).await;
        let client = reqwest::Client::builder().no_proxy().build().unwrap();
        let response = client
            .get(format!(
                "{}/v0/transactionCost?{}",
                http.endpoint,
                contract::query("&sponsored=false&numSignatures=2")
            ))
            .send()
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        // Minimum 200 + 100 extra signature + 34 sponsorship; 101 execution + floor(20.2) buffer.
        assert_eq!(
            response.json::<Value>().await.unwrap(),
            json!({"energy":455,"cost":"22750","success":!reject})
        );
        assert_eq!(
            *state.calls.lock().unwrap(),
            ["consensus", "parameters:best", "invoke:best"]
        );
    }
}

#[tokio::test]
async fn sdk_token_estimation_only_reads_rates_and_never_invokes_or_submits_operations() {
    let state = Arc::new(RpcState::default());
    let server = server(state.clone()).await;
    let sdk = Arc::new(
        SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
            .await
            .unwrap(),
    );
    let raw = crate::fees::token::query("10", r#"{"transfer":2,"mint":1}"#, "CCD");
    assert_eq!(
        request(fees::routes(sdk), &raw).await,
        (200, json!({"energy":729,"cost":"36450"}))
    );
    assert_eq!(
        *state.calls.lock().unwrap(),
        ["consensus", "parameters:best"]
    );
}

#[tokio::test]
async fn sdk_contract_validation_and_rate_failures_never_invoke_the_contract() {
    for fail in [None, Some("parameters:best")] {
        let state = Arc::new(RpcState {
            fail,
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap(),
        );
        let (status, _) = request(
            fees::routes(sdk),
            &contract::query("&executionNRGBuffer=bad"),
        )
        .await;
        assert_eq!(status, if fail.is_some() { 502 } else { 400 });
        assert_eq!(
            *state.calls.lock().unwrap(),
            ["consensus", "parameters:best"]
        );
    }
}

#[tokio::test]
async fn sdk_invocation_status_and_decode_errors_do_not_gain_success_fields() {
    for malformed_invoke in [false, true] {
        let state = Arc::new(RpcState {
            malformed_invoke,
            fail: if malformed_invoke {
                None
            } else {
                Some("invoke:best")
            },
            ..Default::default()
        });
        let server = server(state.clone()).await;
        let sdk = Arc::new(
            SdkNode::connect(&NodeConfig::new(server.endpoint.clone()))
                .await
                .unwrap(),
        );
        let (status, body) = request(fees::routes(sdk), &contract::query("")).await;
        assert_eq!(status, 502);
        assert_eq!(
            body,
            json!({"error":0,"errorMessage":format!("GRPC call failed: Non-OK GRPC status code '{}'.", if malformed_invoke { "INVALID_ARGUMENT" } else { "UNAVAILABLE" })})
        );
        assert_eq!(
            *state.calls.lock().unwrap(),
            ["consensus", "parameters:best", "invoke:best"]
        );
    }
}

#[tokio::test]
async fn sdk_invocation_timeout_is_bounded_and_does_not_return_an_estimate() {
    let state = Arc::new(RpcState {
        delayed_invoke: true,
        ..Default::default()
    });
    let server = server(state.clone()).await;
    let mut config = NodeConfig::new(server.endpoint.clone());
    config.timeout_seconds = 1;
    let sdk = Arc::new(SdkNode::connect(&config).await.unwrap());
    let (status, body) = request(fees::routes(sdk), &contract::query("")).await;
    assert_eq!(status, 503);
    assert!(body.get("success").is_none());
    assert_eq!(body["error"], 3);
}
