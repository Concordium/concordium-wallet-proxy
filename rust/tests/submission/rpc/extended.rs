//! This module checks extended and raw submissions through actual SDK transport.
//! Local mock requests retain complete sender, sponsor, credential, and update contents.

use super::*;

#[tokio::test]
async fn sdk_extended_and_raw_v1_submissions_preserve_both_authorization_maps() {
    let state = Arc::new(RpcState::default());
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
    for sponsored in [false, true] {
        for (route, body) in [
            (
                "submitExtended",
                fixtures::extended(sponsored).to_string().into_bytes(),
            ),
            ("submitRawTransaction", fixtures::extended_bytes(sponsored)),
        ] {
            assert_eq!(
                request(submission::routes(node.clone()), route, body)
                    .await
                    .0,
                200
            );
        }
    }
    let calls = state.requests.lock().unwrap();
    assert_eq!(calls.len(), 4);
    for (pair, sponsored) in calls.as_chunks::<2>().0.iter().zip([false, true]) {
        assert_eq!(pair[0], pair[1]);
        let Some(p::send_block_item_request::BlockItem::AccountTransactionV1(item)) =
            &pair[0].block_item
        else {
            panic!("Wrong item kind");
        };
        let signatures = item.signatures.as_ref().unwrap();
        let sender = &signatures.sender_signatures.as_ref().unwrap().signatures;
        assert_eq!(sender.len(), 2);
        assert_eq!(sender[&0].signatures.len(), 2);
        assert_eq!(sender[&0].signatures[&0].value, [0xab; 64]);
        assert_eq!(sender[&0].signatures[&2].value, [0xcd; 64]);
        assert_eq!(sender[&3].signatures.len(), 1);
        assert_eq!(sender[&3].signatures[&1].value, [0xef; 64]);
        if sponsored {
            let sponsor = &signatures.sponsor_signatures.as_ref().unwrap().signatures;
            assert_eq!(sponsor.len(), 1);
            assert_eq!(sponsor[&2].signatures.len(), 1);
            assert_eq!(sponsor[&2].signatures[&7].value, [0x12; 64]);
        } else {
            assert!(signatures.sponsor_signatures.is_none());
        }
        let header = item.header.as_ref().unwrap();
        assert_eq!(header.sender.as_ref().unwrap().value, [1; 32]);
        assert_eq!(header.sequence_number.as_ref().unwrap().value, u64::MAX);
        assert_eq!(header.energy_amount.as_ref().unwrap().value, 45_000);
        assert_eq!(header.expiry.as_ref().unwrap().value, u64::MAX);
        assert_eq!(
            header.sponsor.as_ref().map(|address| address.value.clone()),
            sponsored.then(|| vec![4; 32])
        );
        let standard = fixtures::hex(fixtures::transaction()["transaction"].as_str().unwrap());
        assert_eq!(
            item.payload.as_ref().unwrap().payload,
            Some(p::account_transaction_payload::Payload::RawPayload(
                standard[60..].to_vec()
            ))
        );
    }
}

#[tokio::test]
async fn sdk_raw_credentials_and_standard_items_match_existing_json_submission_operations() {
    let state = Arc::new(RpcState::default());
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
    for (route, value, bytes) in [
        (
            "submitTransfer",
            fixtures::transaction(),
            fixtures::transaction_bytes(),
        ),
        (
            "submitCredential",
            fixtures::credential(false),
            fixtures::credential_bytes(false),
        ),
        (
            "submitCredential",
            fixtures::credential(true),
            fixtures::credential_bytes(true),
        ),
    ] {
        assert_eq!(
            request(submission::routes(node.clone()), route, value.to_string())
                .await
                .0,
            200
        );
        assert_eq!(
            request(
                submission::routes(node.clone()),
                "submitRawTransaction",
                bytes
            )
            .await
            .0,
            200
        );
    }
    let calls = state.requests.lock().unwrap();
    assert_eq!(calls.len(), 6);
    for pair in calls.as_chunks::<2>().0 {
        assert_eq!(pair[0], pair[1]);
    }
}

#[tokio::test]
async fn sdk_raw_update_preserves_header_payload_signatures_and_local_hash() {
    let state = Arc::new(RpcState::default());
    let server = server(state.clone()).await;
    let node = sdk(&server).await;
    assert_eq!(
        request(
            submission::routes(node),
            "submitRawTransaction",
            fixtures::exchange_rate_update()
        )
        .await,
        (
            200,
            json!({"submissionId":"99cfc039ad665a2ac2ee8fe2645b09b80c2eb2735769106e7ebc26618adc23a1"})
        )
    );
    let calls = state.requests.lock().unwrap();
    assert_eq!(calls.len(), 1);
    let Some(p::send_block_item_request::BlockItem::UpdateInstruction(item)) = &calls[0].block_item
    else {
        panic!("Wrong item kind");
    };
    let header = item.header.as_ref().unwrap();
    assert_eq!(header.sequence_number.as_ref().unwrap().value, 5);
    assert_eq!(header.effective_time.as_ref().unwrap().value, 0);
    assert_eq!(header.timeout.as_ref().unwrap().value, u64::MAX);
    assert_eq!(
        item.payload.as_ref().unwrap().payload,
        Some(p::update_instruction_payload::Payload::RawPayload(
            fixtures::exchange_rate_update()[29..46].to_vec()
        ))
    );
    let signatures = &item.signatures.as_ref().unwrap().signatures;
    assert_eq!(signatures.len(), 1);
    assert_eq!(signatures[&7].value, [0x34; 64]);
}

#[tokio::test]
async fn sdk_extended_and_raw_requests_share_rejection_timeout_and_one_reconnect_policy() {
    for (route, body) in [
        (
            "submitExtended",
            fixtures::extended(true).to_string().into_bytes(),
        ),
        ("submitRawTransaction", fixtures::exchange_rate_update()),
    ] {
        for (status, resets, delayed, expected, arrivals) in [
            (Some(tonic::Code::AlreadyExists), 0, false, 400, 1),
            (Some(tonic::Code::Unavailable), 0, false, 400, 1),
            (None, 1, false, 200, 2),
            (None, 2, false, 400, 2),
            (None, 0, true, 400, 1),
        ] {
            let state = Arc::new(RpcState {
                status,
                resets,
                delayed,
                ..Default::default()
            });
            let server = server(state.clone()).await;
            let node = sdk(&server).await;
            let (actual, value) = request(submission::routes(node), route, body.clone()).await;
            assert_eq!(actual, expected);
            assert_eq!(state.arrivals.load(Ordering::SeqCst), arrivals);
            if expected != 200 {
                assert!(value.get("submissionId").is_none());
            }
            let calls = state.requests.lock().unwrap();
            if calls.len() == 2 {
                assert_eq!(calls[0], calls[1]);
            }
        }
    }
}
