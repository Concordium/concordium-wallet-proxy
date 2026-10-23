//! This module checks token event ordering, opaque details, and legacy lock exclusion.
//! It also checks that absent token rejection details are omitted rather than null.

use super::*;

#[tokio::test]
async fn token_effects_keep_raw_details_optional_memos_and_legacy_lock_exclusion() {
    let token_id: plt::TokenId = "USD".parse().unwrap();
    let holder = plt::TokenHolder::Account { address: SENDER };
    let amount = plt::TokenAmount::from_raw(u64::MAX, 6);
    let tokens = vec![
        plt::TokenEvent {
            token_id: token_id.clone(),
            event: plt::TokenEventDetails::Module(plt::EncodedTokenModuleEvent {
                event_type: "opaque".parse().unwrap(),
                details: vec![0xa0].into(),
            }),
        },
        plt::TokenEvent {
            token_id: token_id.clone(),
            event: plt::TokenEventDetails::Mint(plt::TokenSupplyUpdateEvent {
                target: holder.clone(),
                amount,
            }),
        },
        plt::TokenEvent {
            token_id: token_id.clone(),
            event: plt::TokenEventDetails::Burn(plt::TokenSupplyUpdateEvent {
                target: holder.clone(),
                amount,
            }),
        },
        plt::TokenEvent {
            token_id: token_id.clone(),
            event: plt::TokenEventDetails::Transfer(plt::TokenTransferEvent {
                from: holder.clone(),
                to: holder.clone(),
                amount,
                memo: None,
            }),
        },
        plt::TokenEvent {
            token_id: token_id.clone(),
            event: plt::TokenEventDetails::Transfer(plt::TokenTransferEvent {
                from: holder.clone(),
                to: holder,
                amount,
                memo: Some(vec![0, 255].try_into().unwrap()),
            }),
        },
    ];
    let amount_json = json!({"value":"18446744073709551615","decimals":6});
    let holder_json = json!({"type":"account","address":account()});
    let expected = json!([
        tag(
            "TokenModuleEvent",
            json!({"tokenId":"USD","type":"opaque","details":"a0"})
        ),
        tag(
            "TokenMint",
            json!({"tokenId":"USD","target":holder_json,"amount":amount_json})
        ),
        tag(
            "TokenBurn",
            json!({"tokenId":"USD","target":holder_json,"amount":amount_json})
        ),
        tag(
            "TokenTransfer",
            json!({"tokenId":"USD","from":holder_json,"to":holder_json,"amount":amount_json})
        ),
        tag(
            "TokenTransfer",
            json!({"tokenId":"USD","from":holder_json,"to":holder_json,"amount":amount_json,"memo":"00ff"})
        )
    ]);
    let lock = plt::OperationEvent::Lock(plt::LockEvent::Destroy(plt::LockDestroyEvent {
        lock_id: plt::LockId {
            account_index: 42,
            sequence_number: 1,
            creation_order: 0,
        },
    }));
    let mut events = vec![Upward::Known(lock.clone())];
    events.extend(
        tokens
            .iter()
            .cloned()
            .map(plt::OperationEvent::Token)
            .map(Upward::Known),
    );
    events.push(Upward::Known(lock.clone()));
    check(
        AccountTransactionEffects::TokenUpdate { events },
        "tokenUpdate",
        expected.clone(),
    )
    .await;
    check(
        AccountTransactionEffects::TokenUpdate {
            events: vec![Upward::Known(lock)],
        },
        "tokenUpdate",
        json!([]),
    )
    .await;
    let mut summary = fixtures::account(AccountTransactionEffects::DataRegistered {
        data: vec![].try_into().unwrap(),
    });
    summary.details = Upward::Known(BlockItemSummaryDetails::TokenCreationDetails(
        TokenCreationDetails {
            create_plt: CreatePlt {
                token_id,
                token_module: [7; 32].into(),
                decimals: 6,
                initialization_parameters: vec![0xa0].into(),
            },
            events: tokens,
        },
    ));
    let mut expected_events = vec![tag(
        "TokenCreated",
        json!({"payload":{"tokenId":"USD","tokenModule":"07".repeat(32),"decimals":6,"initializationParameters":"a0"}}),
    )];
    expected_events.extend(expected.as_array().unwrap().iter().cloned());
    let mut output = super::super::expected("summary");
    output["sender"] = Value::Null;
    output["cost"] = json!("0");
    output["type"] = json!({"type":"updateTransaction","contents":"updateCreatePLT"});
    output["result"] = json!({"outcome":"success","events":expected_events});
    let node = Arc::new(TestNode {
        summaries: vec![summary],
        ..Default::default()
    });
    assert_eq!(
        request(
            blocks::routes(node),
            &format!("/v0/blockTransactionEvents/{HASH}")
        )
        .await,
        (200, json!([output]))
    );
}

#[tokio::test]
async fn token_rejections_preserve_missing_versus_present_raw_details() {
    for details in [None, Some(vec![0xa0])] {
        let reason = RejectReason::TokenUpdateTransactionFailed {
            reject_reason: plt::EncodedTokenModuleRejectReason {
                token_id: "USD".parse().unwrap(),
                reason_type: "opaque".parse().unwrap(),
                details: details.clone().map(Into::into),
            },
        };
        let mut value = expected("summary");
        value["type"] = json!({"type":"accountTransaction","contents":"tokenUpdate"});
        let mut contents = json!({"tokenId":"USD","type":"opaque"});
        if details.is_some() {
            contents["details"] = json!("a0");
        }
        value["result"] = json!({"outcome":"reject","rejectReason":{"tag":"TokenUpdateTransactionFailed","contents":contents}});
        let node = Arc::new(TestNode {
            summaries: vec![fixtures::account(AccountTransactionEffects::None {
                transaction_type: Some(TransactionType::TokenUpdate),
                reject_reason: Upward::Known(reason),
            })],
            ..Default::default()
        });
        assert_eq!(
            request(
                blocks::routes(node),
                &format!("/v0/blockTransactionEvents/{HASH}")
            )
            .await,
            (200, json!([value]))
        );
    }
}
