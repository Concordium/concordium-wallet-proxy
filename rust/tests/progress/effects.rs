//! This module checks simple status details for each supported successful effect.
//! It retains encrypted sender overrides, sponsor omissions, and contract trace restrictions.

use super::*;
use concordium_rust_sdk::{
    id::types::CredentialType,
    protocol_level_tokens as plt,
    types::{
        AccountCreationDetails, BakerEvent, BakerId, BlockItemSummaryDetails, ContractAddress,
        ContractInitializedEvent, ContractTraceElement, DelegatorId, EncryptedSelfAmountAddedEvent,
        InstanceUpdatedEvent, SponsorDetails,
        smart_contracts::{ContractEvent, OwnedContractName, OwnedReceiveName, WasmVersionInt},
    },
};

#[tokio::test]
async fn transfer_memos_and_sponsor_details_preserve_omissions_and_full_cost_precision() {
    let mut summary: BlockItemSummary = fixtures::raw_transfer().try_into().unwrap();
    check(summary.clone(), json!({"outcome":"success","to":RECEIVER,"amount":"18446744073709551615","memo":"00ff","sponsor":{"sponsor":RECEIVER,"cost":"18446744073709551615"}})).await;
    if let Upward::Known(BlockItemSummaryDetails::AccountTransaction(details)) =
        &mut summary.details
    {
        details.sponsor = Some(SponsorDetails {
            sponsor: fixtures::RECEIVER,
            cost: Amount::from_micro_ccd(0),
        });
    }
    check(summary, json!({"outcome":"success","to":RECEIVER,"amount":"18446744073709551615","memo":"00ff","sponsor":{"sponsor":RECEIVER,"cost":"0"}})).await;
}

#[tokio::test]
async fn credential_deployments_have_null_sender_zero_cost_and_no_sponsor() {
    for credential_type in [CredentialType::Initial, CredentialType::Normal] {
        let mut summary = transfer();
        summary.details = Upward::Known(BlockItemSummaryDetails::AccountCreation(
            AccountCreationDetails {
                credential_type,
                address: fixtures::RECEIVER,
                reg_id: fixtures::credential(),
            },
        ));
        assert_eq!(
            request(
                progress::routes(node(finalized(summary))),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await,
            (
                200,
                json!({"status":"finalized","blockHashes":["03".repeat(32)],"transactionHash":"04".repeat(32),"sender":null,"cost":"0","outcome":"success"})
            )
        );
    }
}

#[tokio::test]
async fn encrypted_transfers_and_balance_conversions_keep_legacy_field_names() {
    let encrypted = format!("c0{}", "00".repeat(47)).repeat(4);
    for memo in [false, true] {
        let effect = if memo {
            Effect::EncryptedAmountTransferredWithMemo {
                removed: fixtures::removed(),
                added: fixtures::added(),
                memo: vec![0, 255].try_into().unwrap(),
            }
        } else {
            Effect::EncryptedAmountTransferred {
                removed: fixtures::removed(),
                added: fixtures::added(),
            }
        };
        let mut expected = json!({"outcome":"success","sender":SENDER,"to":RECEIVER,"encryptedAmount":encrypted,"inputEncryptedAmount":encrypted,"aggregatedIndex":2,"newSelfEncryptedAmount":encrypted});
        if memo {
            expected["memo"] = "00ff".into();
        }
        check(fixtures::account(effect), expected).await;
    }
    let mut boundary = fixtures::removed();
    boundary.up_to_index = u64::MAX.into();
    check(fixtures::account(Effect::TransferredToPublic { removed:boundary, amount:Amount::from_micro_ccd(0) }), json!({"outcome":"success","sender":SENDER,"newSelfEncryptedAmount":encrypted,"inputEncryptedAmount":encrypted,"aggregatedIndex":18446744073709551615u64,"amountAdded":"0"})).await;
    check(fixtures::account(Effect::TransferredToPublic { removed:fixtures::removed(), amount:Amount::from_micro_ccd(u64::MAX) }), json!({"outcome":"success","sender":SENDER,"newSelfEncryptedAmount":encrypted,"inputEncryptedAmount":encrypted,"aggregatedIndex":2,"amountAdded":"18446744073709551615"})).await;
    check(fixtures::account(Effect::TransferredToEncrypted { data:Box::new(EncryptedSelfAmountAddedEvent { account:fixtures::SENDER, new_amount:fixtures::encrypted(), amount:Amount::from_micro_ccd(7) }) }), json!({"outcome":"success","sender":SENDER,"newSelfEncryptedAmount":encrypted,"amountSubtracted":"7"})).await;
}

#[tokio::test]
async fn encrypted_override_does_not_hide_a_common_sender_difference_during_ambiguity_checks() {
    let first = fixtures::account(Effect::TransferredToPublic {
        removed: fixtures::removed(),
        amount: Amount::from_micro_ccd(1),
    });
    let mut second = first.clone();
    if let Upward::Known(BlockItemSummaryDetails::AccountTransaction(account)) = &mut second.details
    {
        account.sender = fixtures::RECEIVER;
    }
    let status = TransactionStatus::Committed(BTreeMap::from([
        ([1; 32].into(), first),
        ([2; 32].into(), second),
    ]));
    assert_eq!(
        request(
            progress::routes(node(status)),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await,
        (
            200,
            json!({"status":"committed","blockHashes":["01".repeat(32),"02".repeat(32)],"outcome":"ambiguous"})
        )
    );
}

#[tokio::test]
async fn validator_id_comes_only_from_the_first_configuration_event() {
    let baker = Upward::Known(BakerEvent::BakerRemoved {
        baker_id: BakerId { id: 42.into() },
    });
    check(
        fixtures::account(Effect::BakerConfigured {
            data: vec![baker.clone()],
        }),
        json!({"outcome":"success","bakerId":42}),
    )
    .await;
    check(
        fixtures::account(Effect::BakerConfigured {
            data: vec![
                Upward::Known(BakerEvent::DelegationRemoved {
                    delegator_id: DelegatorId { id: 7.into() },
                }),
                baker,
            ],
        }),
        json!({"outcome":"success"}),
    )
    .await;
    check(
        fixtures::account(Effect::BakerConfigured { data: vec![] }),
        json!({"outcome":"success"}),
    )
    .await;
    check(
        fixtures::account(Effect::DelegationConfigured { data: vec![] }),
        json!({"outcome":"success"}),
    )
    .await;
}

#[tokio::test]
async fn module_and_contract_initialization_keep_the_legacy_fields_and_drop_parameter() {
    check(
        fixtures::account(Effect::ModuleDeployed {
            module_ref: [5; 32].into(),
        }),
        json!({"outcome":"success","moduleRef":"05".repeat(32)}),
    )
    .await;
    for version in [0, 1] {
        let data = ContractInitializedEvent {
            contract_version: WasmVersionInt(version),
            origin_ref: [5; 32].into(),
            address: ContractAddress {
                index: 7,
                subindex: 8,
            },
            amount: Amount::from_micro_ccd(1000),
            init_name: OwnedContractName::new("init_test".into()).unwrap(),
            events: vec![ContractEvent::from(vec![0, 255])],
            parameter: Some(vec![1, 2].try_into().unwrap()),
        };
        check(fixtures::account(Effect::ContractInitialized { data }), json!({"outcome":"success","moduleRef":"05".repeat(32),"address":{"index":7,"subindex":8},"amount":"1000","initName":"init_test","contractVersion":version,"events":["00ff"]})).await;
    }
}

#[tokio::test]
async fn contract_trace_names_addresses_and_order_are_specific_to_simple_statuses() {
    let address = ContractAddress {
        index: 7,
        subindex: 8,
    };
    let trace = vec![
        ContractTraceElement::Updated {
            data: InstanceUpdatedEvent {
                contract_version: WasmVersionInt(1),
                address,
                instigator: concordium_rust_sdk::types::Address::Account(fixtures::SENDER),
                amount: Amount::from_micro_ccd(0),
                message: vec![0, 255].try_into().unwrap(),
                receive_name: OwnedReceiveName::new("test.receive".into()).unwrap(),
                events: vec![],
            },
        },
        ContractTraceElement::Transferred {
            from: address,
            amount: Amount::from_micro_ccd(1000),
            to: fixtures::RECEIVER,
        },
        ContractTraceElement::Interrupted {
            address,
            events: vec![ContractEvent::from(vec![0, 255])],
        },
        ContractTraceElement::Resumed {
            address,
            success: false,
        },
    ];
    check(fixtures::account(Effect::ContractUpdateIssued { effects:trace.into_iter().map(Upward::Known).collect() }), json!({"outcome":"success","trace":[
        {"type":"updated","address":{"index":7,"subindex":8},"instigator":{"type":"AddressAccount","address":SENDER},"amount":"0","message":"00ff","receiveName":"test.receive","contractVersion":1,"events":[]},
        {"type":"transferred","from":{"index":7,"subindex":8},"amount":"1000","to":RECEIVER},
        {"type":"interrupted","address":{"index":7,"subindex":8},"events":["00ff"]},
        {"type":"resumed","address":{"index":7,"subindex":8},"success":false}
    ]})).await;
    let upgrade = Effect::ContractUpdateIssued {
        effects: vec![Upward::Known(ContractTraceElement::Upgraded {
            address,
            from: [5; 32].into(),
            to: [6; 32].into(),
        })],
    };
    assert_eq!(
        request(
            progress::routes(node(finalized(fixtures::account(upgrade)))),
            &format!("/v0/submissionStatus/{HASH}")
        )
        .await
        .0,
        500
    );
}

#[tokio::test]
async fn token_single_transfer_and_other_successes_keep_the_outcome_omission_quirk() {
    for memo in [false, true] {
        let event = plt::OperationEvent::Token(plt::TokenEvent {
            token_id: "USD".parse().unwrap(),
            event: plt::TokenEventDetails::Transfer(plt::TokenTransferEvent {
                from: plt::TokenHolder::Account {
                    address: fixtures::SENDER,
                },
                to: plt::TokenHolder::Account {
                    address: fixtures::RECEIVER,
                },
                amount: plt::TokenAmount::from_raw(u64::MAX, 6),
                memo: if memo {
                    Some(vec![0, 255].try_into().unwrap())
                } else {
                    None
                },
            }),
        });
        let mut expected = json!({"outcome":"success","to":RECEIVER,"tokenId":"USD","tokenAmount":{"value":"18446744073709551615","decimals":6}});
        if memo {
            expected["memo"] = "00ff".into();
        }
        check(
            fixtures::account(Effect::TokenUpdate {
                events: vec![Upward::Known(event.clone())],
            }),
            expected,
        )
        .await;
        check(
            fixtures::account(Effect::TokenUpdate {
                events: vec![Upward::Known(event.clone()), Upward::Known(event)],
            }),
            json!({}),
        )
        .await;
    }
    let mint = plt::OperationEvent::Token(plt::TokenEvent {
        token_id: "USD".parse().unwrap(),
        event: plt::TokenEventDetails::Mint(plt::TokenSupplyUpdateEvent {
            target: plt::TokenHolder::Account {
                address: fixtures::SENDER,
            },
            amount: plt::TokenAmount::from_raw(1, 0),
        }),
    });
    check(
        fixtures::account(Effect::TokenUpdate {
            events: vec![Upward::Known(mint)],
        }),
        json!({}),
    )
    .await;
    // The pinned legacy gRPC adapter ignores unified lock-only events.
    let lock = plt::OperationEvent::Lock(plt::LockEvent::Destroy(plt::LockDestroyEvent {
        lock_id: plt::LockId {
            account_index: 42,
            sequence_number: 1,
            creation_order: 0,
        },
    }));
    check(
        fixtures::account(Effect::TokenUpdate {
            events: vec![Upward::Known(lock)],
        }),
        json!({}),
    )
    .await;
    check(
        fixtures::account(Effect::TokenUpdate { events: vec![] }),
        json!({}),
    )
    .await;
    check(
        fixtures::account(Effect::DataRegistered {
            data: vec![0, 255].try_into().unwrap(),
        }),
        json!({"outcome":"success","registeredData":"00ff"}),
    )
    .await;
}
