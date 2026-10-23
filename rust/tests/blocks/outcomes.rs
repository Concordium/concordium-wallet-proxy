//! This module checks legacy event projections independently of SDK summary JSON.
//! Inputs use typed effects; expectations come from Haskell event field definitions.

mod configuration;
mod governance;
mod tokens;

use super::*;
use concordium_rust_sdk::{
    common::types::{Amount, Timestamp, TransactionTime},
    protocol_level_tokens as plt,
    types::{smart_contracts::*, *},
    v2::Upward,
};
use fixtures::{RECEIVER, SENDER};
/// This value identifies the baker in legacy event fixtures.
const BAKER: BakerId = BakerId {
    id: AccountIndex { index: 42 },
};
/// This value identifies the delegator in configuration fixtures.
const DELEGATOR: DelegatorId = DelegatorId {
    id: AccountIndex { index: 42 },
};

async fn check(effect: AccountTransactionEffects, kind: &str, events: Value) {
    let node = Arc::new(TestNode {
        summaries: vec![fixtures::account(effect)],
        ..Default::default()
    });
    assert_eq!(
        request(
            blocks::routes(node),
            &format!("/v0/blockTransactionEvents/{HASH}")
        )
        .await,
        (200, json!([success_summary(kind, events)]))
    );
}
fn tag(name: &str, mut fields: Value) -> Value {
    fields["tag"] = json!(name);
    fields
}
fn account() -> Value {
    expected("summary")["sender"].clone()
}
fn encrypted_hex() -> String {
    format!("c0{}", "00".repeat(47)).repeat(4)
}
fn removed() -> Value {
    tag(
        "EncryptedAmountsRemoved",
        json!({"account": account(), "newAmount": encrypted_hex(), "inputAmount": encrypted_hex(), "upToIndex": 2}),
    )
}
fn added() -> Value {
    tag(
        "NewEncryptedAmount",
        json!({"account": "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3", "newIndex": 3, "encryptedAmount": encrypted_hex()}),
    )
}

#[tokio::test]
async fn transfer_schedule_module_and_data_effects_preserve_memos_and_order() {
    use AccountTransactionEffects::*;
    check(
        AccountTransfer {
            amount: Amount::from_micro_ccd(u64::MAX),
            to: RECEIVER,
        },
        "transfer",
        expected("transfer"),
    )
    .await;
    let mut with_memo = expected("transfer");
    with_memo.as_array_mut().unwrap().push(expected("memo"));
    check(
        AccountTransferWithMemo {
            amount: Amount::from_micro_ccd(u64::MAX),
            to: RECEIVER,
            memo: vec![0, 255].try_into().unwrap(),
        },
        "transferWithMemo",
        with_memo,
    )
    .await;
    let schedule = vec![
        (
            Timestamp::from_timestamp_millis(NEXT),
            Amount::from_micro_ccd(0),
        ),
        (
            Timestamp::from_timestamp_millis(NEXT + 1),
            Amount::from_micro_ccd(u64::MAX),
        ),
    ];
    check(
        TransferredWithSchedule {
            to: RECEIVER,
            amount: schedule.clone(),
        },
        "transferWithSchedule",
        expected("schedule"),
    )
    .await;
    let mut with_memo = expected("schedule");
    with_memo.as_array_mut().unwrap().push(expected("memo"));
    check(
        TransferredWithScheduleAndMemo {
            to: RECEIVER,
            amount: schedule,
            memo: vec![0, 255].try_into().unwrap(),
        },
        "transferWithScheduleAndMemo",
        with_memo,
    )
    .await;
    check(
        ModuleDeployed {
            module_ref: [5; 32].into(),
        },
        "deployModule",
        expected("module"),
    )
    .await;
    check(
        DataRegistered {
            data: vec![0, 255].try_into().unwrap(),
        },
        "registerData",
        expected("registered"),
    )
    .await;
}

#[tokio::test]
async fn encrypted_effects_keep_all_historical_events_and_memo_order() {
    use AccountTransactionEffects::*;
    check(
        EncryptedAmountTransferred {
            removed: fixtures::removed(),
            added: fixtures::added(),
        },
        "encryptedAmountTransfer",
        json!([removed(), added()]),
    )
    .await;
    check(
        EncryptedAmountTransferredWithMemo {
            removed: fixtures::removed(),
            added: fixtures::added(),
            memo: vec![0, 255].try_into().unwrap(),
        },
        "encryptedAmountTransferWithMemo",
        json!([removed(), added(), expected("memo")]),
    )
    .await;
    check(
        TransferredToEncrypted {
            data: Box::new(EncryptedSelfAmountAddedEvent {
                account: SENDER,
                new_amount: fixtures::encrypted(),
                amount: Amount::from_micro_ccd(12),
            }),
        },
        "transferToEncrypted",
        json!([tag(
            "EncryptedSelfAmountAdded",
            json!({"account": account(), "newAmount": encrypted_hex(), "amount": "12"})
        )]),
    )
    .await;
    check(
        TransferredToPublic {
            removed: fixtures::removed(),
            amount: Amount::from_micro_ccd(12),
        },
        "transferToPublic",
        json!([
            removed(),
            tag(
                "AmountAddedByDecryption",
                json!({"account": account(), "amount": "12"})
            )
        ]),
    )
    .await;
}

#[tokio::test]
async fn legacy_baker_credential_effects_keep_actors_keys_and_empty_changes() {
    use AccountTransactionEffects::*;
    let keys = fixtures::baker_keys();
    let key_fields = fixtures::key_fields(&keys);
    let mut fields = key_fields.clone();
    fields["stake"] = json!("1000");
    fields["restakeEarnings"] = json!(false);
    check(
        BakerAdded {
            data: Box::new(BakerAddedEvent {
                keys_event: keys.clone(),
                stake: Amount::from_micro_ccd(1000),
                restake_earnings: false,
            }),
        },
        "addBaker",
        json!([tag("BakerAdded", fields)]),
    )
    .await;
    check(
        BakerKeysUpdated {
            data: Box::new(keys),
        },
        "updateBakerKeys",
        json!([tag("BakerKeysUpdated", key_fields)]),
    )
    .await;
    check(
        BakerRemoved { baker_id: BAKER },
        "removeBaker",
        json!([tag(
            "BakerRemoved",
            json!({"bakerId": 42, "account": account()})
        )]),
    )
    .await;
    check(
        BakerRestakeEarningsUpdated {
            baker_id: BAKER,
            restake_earnings: false,
        },
        "updateBakerRestakeEarnings",
        json!([tag(
            "BakerSetRestakeEarnings",
            json!({"bakerId":42,"account":account(),"restakeEarnings":false})
        )]),
    )
    .await;
    for increased in [true, false] {
        check(
            BakerStakeUpdated {
                data: Some(BakerStakeUpdatedData {
                    baker_id: BAKER,
                    new_stake: Amount::from_micro_ccd(1000),
                    increased,
                }),
            },
            "updateBakerStake",
            json!([tag(
                if increased {
                    "BakerStakeIncreased"
                } else {
                    "BakerStakeDecreased"
                },
                json!({"bakerId":42,"account":account(),"newStake":"1000"})
            )]),
        )
        .await;
    }
    check(
        BakerStakeUpdated { data: None },
        "updateBakerStake",
        json!([]),
    )
    .await;
    let credential = fixtures::credential();
    let hex = format!("c0{}", "00".repeat(47));
    check(
        CredentialKeysUpdated {
            cred_id: credential,
        },
        "updateCredentialKeys",
        json!([tag("CredentialKeysUpdated", json!({"credId":hex}))]),
    )
    .await;
    check(
        CredentialsUpdated {
            new_cred_ids: vec![credential],
            removed_cred_ids: vec![],
            new_threshold: 1u8.try_into().unwrap(),
        },
        "updateCredentials",
        json!([tag(
            "CredentialsUpdated",
            json!({"account":account(),"newCredIds":[hex],"removedCredIds":[],"newThreshold":1})
        )]),
    )
    .await;
}

#[tokio::test]
async fn contract_initialization_and_all_trace_kinds_preserve_parameters_and_roles() {
    for version in [0, 1] {
        for supplied in [false, true] {
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
                parameter: supplied.then(|| vec![0, 255].try_into().unwrap()),
            };
            check(AccountTransactionEffects::ContractInitialized { data }, "initContract", json!([tag("ContractInitialized", json!({"contractVersion":version,"ref":"05".repeat(32),"address":{"index":7,"subindex":8},"amount":"1000","initName":"init_test","events":["00ff"],"parameter":if supplied {"00ff"} else {""}}))])).await;
        }
    }
    let address = ContractAddress {
        index: 7,
        subindex: 8,
    };
    let trace = vec![
        ContractTraceElement::Updated {
            data: InstanceUpdatedEvent {
                contract_version: WasmVersionInt(1),
                address,
                instigator: Address::Account(SENDER),
                amount: Amount::from_micro_ccd(0),
                message: vec![0, 255].try_into().unwrap(),
                receive_name: OwnedReceiveName::new("test.receive".into()).unwrap(),
                events: vec![],
            },
        },
        ContractTraceElement::Transferred {
            from: address,
            amount: Amount::from_micro_ccd(1000),
            to: RECEIVER,
        },
        ContractTraceElement::Interrupted {
            address,
            events: vec![],
        },
        ContractTraceElement::Resumed {
            address,
            success: false,
        },
        ContractTraceElement::Upgraded {
            address,
            from: [5; 32].into(),
            to: [6; 32].into(),
        },
    ];
    check(AccountTransactionEffects::ContractUpdateIssued { effects: trace.into_iter().map(Upward::Known).collect() }, "update", json!([
        tag("Updated",json!({"contractVersion":1,"address":{"index":7,"subindex":8},"instigator":{"type":"AddressAccount","address":account()},"amount":"0","message":"00ff","receiveName":"test.receive","events":[]})),
        tag("Transferred",json!({"from":{"type":"AddressContract","address":{"index":7,"subindex":8}},"to":{"type":"AddressAccount","address":"2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3"},"amount":"1000"})),
        tag("Interrupted",json!({"address":{"index":7,"subindex":8},"events":[]})), tag("Resumed",json!({"address":{"index":7,"subindex":8},"success":false})), tag("Upgraded",json!({"address":{"index":7,"subindex":8},"from":"05".repeat(32),"to":"06".repeat(32)}))
    ])).await;
}

#[tokio::test]
async fn rejection_records_keep_contract_parameters_and_account_amount_roles() {
    for (reason, output) in [
        (
            RejectReason::RejectedReceive {
                reject_reason: -7,
                contract_address: ContractAddress {
                    index: 7,
                    subindex: 8,
                },
                receive_name: OwnedReceiveName::new("test.receive".into()).unwrap(),
                parameter: vec![0, 255].try_into().unwrap(),
            },
            json!({"tag":"RejectedReceive","rejectReason":-7,"contractAddress":{"index":7,"subindex":8},"receiveName":"test.receive","parameter":"00ff"}),
        ),
        (
            RejectReason::AmountTooLarge {
                contents: (Address::Account(SENDER), Amount::from_micro_ccd(u64::MAX)),
            },
            json!({"tag":"AmountTooLarge","contents":[{"type":"AddressAccount","address":account()},"18446744073709551615"]}),
        ),
    ] {
        let mut value = expected("summary");
        value["type"] = json!({"type":"accountTransaction","contents":"update"});
        value["result"] = json!({"outcome":"reject","rejectReason":output});
        let node = Arc::new(TestNode {
            summaries: vec![fixtures::account(AccountTransactionEffects::None {
                transaction_type: Some(TransactionType::Update),
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

#[tokio::test]
async fn unsupported_legacy_variants_fail_instead_of_leaking_new_sdk_schemas() {
    let mut update = fixtures::account(AccountTransactionEffects::DataRegistered {
        data: vec![].try_into().unwrap(),
    });
    update.details = Upward::Known(BlockItemSummaryDetails::Update(UpdateDetails {
        effective_time: TransactionTime { seconds: 0 },
        payload: Upward::Known(UpdatePayload::MaxLockDuration(
            concordium_rust_sdk::smart_contracts::common::Duration::from_millis(1000),
        )),
    }));
    let rejection = fixtures::account(AccountTransactionEffects::None {
        transaction_type: Some(TransactionType::TokenUpdate),
        reject_reason: Upward::Known(RejectReason::NonExistentLockId {
            lock_id: plt::LockId {
                account_index: 42,
                sequence_number: 1,
                creation_order: 0,
            },
        }),
    });
    let initialization = fixtures::account(AccountTransactionEffects::ContractInitialized {
        data: ContractInitializedEvent {
            contract_version: WasmVersionInt(2),
            origin_ref: [5; 32].into(),
            address: ContractAddress {
                index: 7,
                subindex: 8,
            },
            amount: Amount::from_micro_ccd(0),
            init_name: OwnedContractName::new("init_test".into()).unwrap(),
            events: vec![],
            parameter: None,
        },
    });
    let trace = fixtures::account(AccountTransactionEffects::ContractUpdateIssued {
        effects: vec![Upward::Known(ContractTraceElement::Updated {
            data: InstanceUpdatedEvent {
                contract_version: WasmVersionInt(2),
                address: ContractAddress {
                    index: 7,
                    subindex: 8,
                },
                instigator: Address::Account(SENDER),
                amount: Amount::from_micro_ccd(0),
                message: vec![].try_into().unwrap(),
                receive_name: OwnedReceiveName::new("test.receive".into()).unwrap(),
                events: vec![],
            },
        })],
    });
    for summary in [update, rejection, initialization, trace] {
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
            (
                500,
                json!({"error":0,"errorMessage":"Internal server error"})
            )
        );
    }
}

#[tokio::test]
async fn sponsor_rejections_and_forward_unknowns_preserve_complete_response_semantics() {
    let mut transfer = fixtures::account(AccountTransactionEffects::AccountTransfer {
        to: RECEIVER,
        amount: Amount::from_micro_ccd(u64::MAX),
    });
    let Upward::Known(BlockItemSummaryDetails::AccountTransaction(ref mut details)) =
        transfer.details
    else {
        unreachable!()
    };
    details.sponsor = Some(SponsorDetails {
        sponsor: RECEIVER,
        cost: Amount::from_micro_ccd(u64::MAX),
    });
    let mut expected = success_summary("transfer", expected("transfer"));
    expected["sponsorDetails"] = json!({"sponsor":"2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3","cost":"18446744073709551615"});
    let node = Arc::new(TestNode {
        summaries: vec![transfer],
        ..Default::default()
    });
    assert_eq!(
        request(
            blocks::routes(node),
            &format!("/v0/blockTransactionEvents/{HASH}")
        )
        .await,
        (200, json!([expected]))
    );
    for (kind, reason, output) in [
        (
            None,
            RejectReason::SerializationFailure,
            json!({"tag":"SerializationFailure"}),
        ),
        (
            Some(TransactionType::Transfer),
            RejectReason::OutOfEnergy,
            json!({"tag":"OutOfEnergy"}),
        ),
        (
            Some(TransactionType::InitContract),
            RejectReason::RejectedInit {
                reject_reason: -2147483648,
            },
            json!({"tag":"RejectedInit","rejectReason":-2147483648}),
        ),
    ] {
        let node = Arc::new(TestNode {
            summaries: vec![fixtures::account(AccountTransactionEffects::None {
                transaction_type: kind,
                reject_reason: Upward::Known(reason),
            })],
            ..Default::default()
        });
        let mut value = super::expected("summary");
        value["type"] = json!({"type":"accountTransaction","contents":kind});
        value["result"] = json!({"outcome":"reject","rejectReason":output});
        assert_eq!(
            request(
                blocks::routes(node),
                &format!("/v0/blockTransactionEvents/{HASH}")
            )
            .await,
            (200, json!([value]))
        );
    }
    let valid = fixtures::account(AccountTransactionEffects::DataRegistered {
        data: vec![].try_into().unwrap(),
    });
    let mut unknown = valid.clone();
    unknown.details = Upward::Unknown(());
    let node = Arc::new(TestNode {
        summaries: vec![valid, unknown],
        ..Default::default()
    });
    assert_eq!(
        request(
            blocks::routes(node),
            &format!("/v0/blockTransactionEvents/{HASH}")
        )
        .await,
        (
            500,
            json!({"error":0,"errorMessage":"Internal server error"})
        )
    );
}
