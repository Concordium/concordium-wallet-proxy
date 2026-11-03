//! These tests check source-derived history fields, descriptions, and account-relative effects.
//! They use hand-written legacy summaries rather than SDK outcome projection as an oracle.

use super::http::*;
use serde_json::{Value, json};

#[tokio::test]
async fn transfer_versions_preserve_precision_memos_description_and_block_fields() {
    for version in 0..4 {
        let value = summary(
            "transferWithMemo",
            vec![
                transfer(SENDER, RECEIVER, "18446744073709551615"),
                json!({"tag":"TransferMemo","memo":"00ff"}),
            ],
        );
        let mut events = vec![format!(
            "Transferred 18446744073709.551615 CCD from {SENDER} to {RECEIVER}"
        )];
        let mut details = json!({"type":if version == 0 {"transfer"} else {"transferWithMemo"},"description":"Transfer with a memo","outcome":"success","events":[] ,"transferSource":SENDER,"transferDestination":RECEIVER,"transferAmount":"18446744073709551615"});
        if version != 0 {
            events.push("Memo '00ff' included in a transfer.".into());
            details["memo"] = json!("00ff");
        }
        details["events"] = json!(events);
        assert_eq!(
            entry(value, version, SENDER, "").await,
            json!({"id":7,"blockHash":"03".repeat(32),"blockTime":1.234,"blockHeight":9007199254740993_u64,"origin":{"type":"self"},"energy":18446744073709551615_u64,"details":details,"transactionHash":"04".repeat(32),"cost":"10","subtotal":"-18446744073709551615","total":"-18446744073709551625"})
        );
    }
}

#[tokio::test]
async fn aliases_self_transfers_receivers_and_sponsors_attribute_costs_separately() {
    let value = summary("transfer", vec![transfer(SENDER, RECEIVER, "100")]);
    let receiver = entry(value.clone(), 1, RECEIVER, "").await;
    assert_eq!(
        receiver["origin"],
        json!({"type":"account","address":SENDER})
    );
    assert_eq!(receiver["total"], "100");
    assert!(receiver.get("cost").is_none());
    assert!(receiver.get("subtotal").is_none());
    let self_transfer = entry(
        summary("transfer", vec![transfer(SENDER, SENDER, "100")]),
        1,
        SENDER,
        "",
    )
    .await;
    assert_eq!(self_transfer["subtotal"], "0");
    assert_eq!(self_transfer["total"], "-10");
    let mut alias = [1; 32];
    alias[31] = 99;
    let alias = concordium_rust_sdk::id::types::AccountAddress(alias).to_string();
    let alias_self = entry(
        summary("transfer", vec![transfer(SENDER, &alias, "100")]),
        1,
        &alias,
        "",
    )
    .await;
    assert_eq!(alias_self["origin"], json!({"type":"self"}));
    assert_eq!(alias_self["subtotal"], "0");
    let mut sponsored = value;
    sponsored["Left"]["sponsorDetails"] = json!({"sponsor":RECEIVER,"cost":"18446744073709551615"});
    let receiver = entry(sponsored.clone(), 2, RECEIVER, "").await;
    assert_eq!(receiver["subtotal"], "100");
    assert_eq!(receiver["cost"], "18446744073709551615");
    assert_eq!(receiver["total"], "-18446744073709551515");
    let sender = entry(sponsored.clone(), 2, SENDER, "").await;
    assert_eq!(sender["cost"], "10");
    assert_eq!(sender["subtotal"], "-100");
    assert_eq!(sender["total"], "-110");
    sponsored["Left"]["sponsorDetails"]["sponsor"] = json!(SENDER);
    let same_sponsor = entry(sponsored, 2, SENDER, "").await;
    assert_eq!(same_sponsor["cost"], "18446744073709551615");
}

#[tokio::test]
async fn rejected_history_preserves_raw_reason_presence_and_malformed_type() {
    let mut value = summary("transfer", vec![]);
    value["Left"]["result"] =
        json!({"outcome":"reject","rejectReason":{"tag":"RejectedInit","rejectReason":-2}});
    for query in ["", "?includeRawRejectReason", "?includeRawRejectReason=n"] {
        let body = entry(value.clone(), 0, SENDER, query).await;
        let details = &body["details"];
        assert_eq!(
            details["rejectReason"],
            "Failed contract initialization due to contract logic with error code -2"
        );
        assert_eq!(details["outcome"], "reject");
        assert!(details.get("events").is_none());
        assert!(body.get("subtotal").is_none());
        assert_eq!(body["total"], "-10");
        if query.is_empty() {
            assert!(details.get("rawRejectReason").is_none());
        } else {
            assert_eq!(
                details["rawRejectReason"],
                json!({"tag":"RejectedInit","rejectReason":-2})
            );
        }
    }
    value["Left"]["type"]["contents"] = Value::Null;
    value["Left"]["sender"] = Value::Null;
    let body = entry(value, 2, SENDER, "").await;
    assert_eq!(body["details"]["type"], "Malformed account transaction");
    assert_eq!(body["details"]["description"], "Malformed transaction body");
    assert_eq!(body["origin"], json!({"type":"none"}));
    assert_eq!(body["total"], "0");
    assert!(body.get("cost").is_none());
}

#[tokio::test]
async fn scheduled_transfers_registered_data_and_exact_event_patterns_keep_legacy_fields() {
    let schedule = json!({"tag":"TransferredWithSchedule","from":SENDER,"to":RECEIVER,"amount":[[1,"10"],[2,"20"]]});
    let body = entry(
        summary(
            "transferWithScheduleAndMemo",
            vec![schedule, json!({"tag":"TransferMemo","memo":"ff"})],
        ),
        0,
        SENDER,
        "",
    )
    .await;
    assert_eq!(body["details"]["type"], "transferWithSchedule");
    assert_eq!(
        body["details"]["description"],
        "Transfer with schedule and a memo"
    );
    assert_eq!(body["details"]["transferAmount"], "30");
    assert_eq!(body["subtotal"], "-30");
    assert_eq!(body["total"], "-40");
    assert!(body["details"].get("memo").is_none());
    let body = entry(
        summary(
            "registerData",
            vec![json!({"tag":"DataRegistered","data":"00ff"})],
        ),
        1,
        SENDER,
        "",
    )
    .await;
    assert_eq!(body["details"]["registeredData"], "00ff");
    assert_eq!(
        body["details"]["events"],
        json!(["Data registered on the chain."])
    );
    assert!(body.get("subtotal").is_none());
    let body = entry(
        summary(
            "transferWithMemo",
            vec![
                transfer(SENDER, RECEIVER, "1"),
                json!({"tag":"TransferMemo","memo":"00"}),
                json!({"tag":"TransferMemo","memo":"ff"}),
            ],
        ),
        1,
        SENDER,
        "",
    )
    .await;
    assert!(body["details"].get("memo").is_none());
    assert_eq!(body["details"]["events"].as_array().unwrap().len(), 3);
}

#[tokio::test]
async fn encrypted_effects_keep_sender_receiver_indices_and_public_subtotals() {
    let encrypted = format!("c0{}", "00".repeat(47)).repeat(4);
    let removed = json!({"tag":"EncryptedAmountsRemoved","account":SENDER,"newAmount":encrypted,"inputAmount":encrypted,"upToIndex":18446744073709551615_u64});
    let added = json!({"tag":"NewEncryptedAmount","account":RECEIVER,"encryptedAmount":encrypted,"newIndex":2});
    let summary = summary(
        "encryptedAmountTransferWithMemo",
        vec![
            removed.clone(),
            added,
            json!({"tag":"TransferMemo","memo":"00"}),
        ],
    );
    let sender = entry(summary.clone(), 1, SENDER, "").await;
    assert_eq!(
        sender["encrypted"],
        json!({"encryptedAmount":encrypted,"newStartIndex":18446744073709551615_u64,"newSelfEncryptedAmount":encrypted})
    );
    assert_eq!(
        sender["details"]["aggregatedIndex"],
        json!(18446744073709551615_u64)
    );
    assert_eq!(sender["details"]["memo"], "00");
    assert_eq!(sender["total"], "-10");
    assert!(sender.get("subtotal").is_none());
    let receiver = entry(summary, 1, RECEIVER, "").await;
    assert_eq!(
        receiver["encrypted"],
        json!({"encryptedAmount":encrypted,"newIndex":2})
    );
    assert_eq!(receiver["total"], "0");
    let public = entry(
        super::http::summary(
            "transferToPublic",
            vec![
                removed,
                json!({"tag":"AmountAddedByDecryption","account":SENDER,"amount":"20"}),
            ],
        ),
        1,
        SENDER,
        "",
    )
    .await;
    assert_eq!(public["subtotal"], "20");
    assert_eq!(public["total"], "10");
    assert_eq!(public["details"]["amountAdded"], "20");
    let shield = entry(super::http::summary("transferToEncrypted",vec![json!({"tag":"EncryptedSelfAmountAdded","account":SENDER,"newAmount":encrypted,"amount":"20"})]),1,SENDER,"").await;
    assert_eq!(shield["subtotal"], "-20");
    assert_eq!(shield["total"], "-30");
    assert_eq!(
        shield["encrypted"],
        json!({"newSelfEncryptedAmount":encrypted})
    );
}

#[tokio::test]
async fn contract_traces_and_initialization_decode_historical_versions_and_sum_effects() {
    let address = json!({"index":7,"subindex":8});
    let initialized = json!({"tag":"ContractInitialized","ref":"05".repeat(32),"address":address,"amount":"7","initName":"init_test","events":[],"parameter":"00ff"});
    let body = entry(summary("initContract", vec![initialized]), 2, SENDER, "").await;
    assert_eq!(
        body["details"]["events"],
        json!([
            "Initialized smart contract test at address <7, 8> with balance 0.000007 CCD. 0 events were logged"
        ])
    );
    assert_eq!(body["subtotal"], "-7");
    let events = vec![
        json!({"tag":"Updated","address":address,"instigator":{"type":"AddressAccount","address":SENDER},"amount":"20","message":"00ff","receiveName":"test.receive","events":[]}),
        json!({"tag":"Transferred","from":{"type":"AddressContract","address":address},"amount":"5","to":{"type":"AddressAccount","address":SENDER}}),
        json!({"tag":"Interrupted","address":address,"events":["00ff"]}),
        json!({"tag":"Resumed","address":address,"success":false}),
        json!({"tag":"Upgraded","address":address,"from":"05".repeat(32),"to":"06".repeat(32)}),
    ];
    let body = entry(summary("update", events), 2, SENDER, "").await;
    assert_eq!(body["subtotal"], "-15");
    assert_eq!(body["total"], "-25");
    assert_eq!(
        body["details"]["events"][0],
        format!(
            "Invoked smart contract: source={SENDER}, target=<7, 8>, contract= test, function= receive, amount=0.000020 CCD, message=00ff"
        )
    );
    assert_eq!(
        body["details"]["events"][2],
        "Execution of <7, 8> triggered an operation."
    );
    assert_eq!(
        body["details"]["events"][3],
        "Operation failed and execution of <7, 8> resumed."
    );
    assert_eq!(
        body["details"]["events"][4],
        format!(
            "Smart contract instance at <7, 8> was upgraded from {} to {}.",
            "05".repeat(32),
            "06".repeat(32)
        )
    );
}

#[tokio::test]
async fn staking_descriptions_preserve_legacy_wording_commission_show_and_targets() {
    let fixtures = [
        (
            "BakerRemoved",
            json!({}),
            format!("Removed validator {SENDER} (ID 42)"),
        ),
        (
            "BakerStakeIncreased",
            json!({"newStake":"1000000"}),
            format!("Stake of validator {SENDER} (ID 42) increased to 1.000000 CCD"),
        ),
        (
            "BakerStakeDecreased",
            json!({"newStake":"0"}),
            format!("Stake of validator {SENDER} (ID 42) decreased to 0.000000 CCD"),
        ),
        (
            "BakerSetRestakeEarnings",
            json!({"restakeEarnings":false}),
            format!("Validator {SENDER} (ID 42) unset restaking of earnings."),
        ),
        (
            "BakerSetOpenStatus",
            json!({"openStatus":"closedForNew"}),
            format!("Open status of validator {SENDER} (ID 42) set to ClosedForNew"),
        ),
        (
            "BakerSetMetadataURL",
            json!({"metadataURL":"https://example.org"}),
            format!("Metadata URL of validator {SENDER} (ID 42) set to https://example.org"),
        ),
        (
            "BakerSetTransactionFeeCommission",
            json!({"transactionFeeCommission":0.125}),
            format!("Transaction fee commission of validator {SENDER} (ID 42) set to 0.125"),
        ),
        (
            "BakerSetBakingRewardCommission",
            json!({"bakingRewardCommission":0.00001}),
            format!("Block reward commission of validator {SENDER} (ID 42) set to 1.0e-5"),
        ),
        (
            "BakerSetFinalizationRewardCommission",
            json!({"finalizationRewardCommission":1}),
            format!("Finalization reward commission of validator {SENDER} (ID 42) set to 1.0"),
        ),
        (
            "BakerSuspended",
            json!({}),
            format!("Validator {SENDER} (ID 42) was suspended."),
        ),
        (
            "BakerResumed",
            json!({}),
            format!("Validator {SENDER} (ID 42) was resumed."),
        ),
        (
            "DelegationAdded",
            json!({}),
            format!("Added delegator {SENDER} (ID 42)"),
        ),
        (
            "DelegationRemoved",
            json!({}),
            format!("Removed delegator {SENDER} (ID 42)"),
        ),
        (
            "DelegationStakeIncreased",
            json!({"newStake":"1"}),
            format!("Stake of delegator {SENDER} (ID 42) increased to 0.000001 CCD"),
        ),
        (
            "DelegationStakeDecreased",
            json!({"newStake":"1"}),
            format!("Stake of delegator {SENDER} (ID 42) decreased to 0.000001 CCD"),
        ),
        (
            "DelegationSetRestakeEarnings",
            json!({"restakeEarnings":true}),
            format!("Delegator {SENDER} (ID 42) set to restake earnings."),
        ),
        (
            "DelegationSetDelegationTarget",
            json!({"delegationTarget":{"delegateType":"Passive"}}),
            format!("Delegator {SENDER} (ID 42) set delegation target to passive delegation"),
        ),
        (
            "DelegationSetDelegationTarget",
            json!({"delegationTarget":{"delegateType":"Baker","bakerId":7}}),
            format!("Delegator {SENDER} (ID 42) set delegation target to staking pool 7"),
        ),
    ];
    for (tag, mut fields, expected) in fixtures {
        fields["tag"] = json!(tag);
        fields["account"] = json!(SENDER);
        fields[if tag.starts_with("Delegation") {
            "delegatorId"
        } else {
            "bakerId"
        }] = json!(42);
        let body = entry(
            summary(
                if tag.starts_with("Delegation") {
                    "configureDelegation"
                } else {
                    "configureBaker"
                },
                vec![fields],
            ),
            2,
            SENDER,
            "",
        )
        .await;
        assert_eq!(body["details"]["events"], json!([expected]));
    }
}

#[tokio::test]
async fn rewards_preserve_multiline_text_totals_null_maps_and_suspension_origins() {
    let cases = [
        (json!({"tag":"BakingRewards","bakerRewards":[{"address":SENDER,"amount":"1"},{"address":SENDER,"amount":"3"}],"remainder":"0"}),"bakingReward","3",format!("Block rewards\n  - account {SENDER} awarded 0.000003 CCD\n")),
        (json!({"tag":"FinalizationRewards","finalizationRewards":[{"address":SENDER,"amount":"4"}],"remainder":"0"}),"finalizationReward","4",format!("Finalization rewards\n   - account {SENDER} awarded 0.000004 CCD\n")),
        (json!({"tag":"PaydayFoundationReward","foundationAccount":SENDER,"developmentCharge":"5"}),"paydayFoundationReward","5",format!("Foundation development charge payout: 0.000005 CCD to {SENDER}")),
        (json!({"tag":"PaydayAccountReward","account":SENDER,"transactionFees":"1","bakerReward":"2","finalizationReward":"3"}),"paydayAccountReward","6",format!("Reward payout of 0.000006 CCD to {SENDER}: \n  - 0.000001 CCD from transaction fees\n  - 0.000002 CCD from block rewards\n  - 0.000003 CCD from finalization rewards\n")),
        (json!({"tag":"PaydayPoolReward","poolOwner":null,"transactionFees":"1","bakerReward":"2","finalizationReward":"3"}),"paydayPoolReward","0","Payday rewards to passive delegators:\n  - 0.000001 CCD from transaction fees\n  - 0.000002 CCD from block rewards\n  - 0.000003 CCD from finalization rewards\n".into()),
        (json!({"tag":"ValidatorSuspended","bakerId":42,"account":SENDER}),"validatorSuspended","0",format!("Validator {SENDER} (ID 42) was suspended due to inactivity.")),
        (json!({"tag":"ValidatorPrimedForSuspension","bakerId":42,"account":SENDER}),"validatorPrimedForSuspension","0",format!("Validator {SENDER} (ID 42) was primed for suspension at the next pay day.")),
    ];
    for (reward, kind, total, text) in cases {
        let body = entry(json!({"Right":reward}), 2, SENDER, "").await;
        assert_eq!(body["details"]["type"], kind);
        assert_eq!(body["details"]["events"], json!([text]));
        assert_eq!(body["total"], total);
        assert_eq!(
            body["origin"]["type"],
            if kind.starts_with("validator") {
                "none"
            } else {
                "reward"
            }
        );
        assert!(body.get("cost").is_none());
    }
    let missing = entry(
        json!({"Right":{"tag":"BakingRewards","bakerRewards":[],"remainder":"0"}}),
        2,
        SENDER,
        "",
    )
    .await;
    assert!(missing["total"].is_null());
    let block = entry(json!({"Right":{"tag":"BlockReward","transactionFees":"1","oldGASAccount":"2","newGASAccount":"3","bakerReward":"4","foundationCharge":"5","baker":SENDER,"foundationAccount":SENDER}}),2,SENDER,"").await;
    assert_eq!(block["total"], "9");
    let mint = entry(json!({"Right":{"tag":"Mint","mintBakingReward":"1","mintFinalizationReward":"2","mintPlatformDevelopmentCharge":"3","foundationAccount":SENDER}}),2,SENDER,"").await;
    assert_eq!(mint["total"], "3");
    assert_eq!(mint["details"]["type"], "platformDevelopmentCharge");
    let accrued = entry(json!({"Right":{"tag":"BlockAccrueReward","transactionFees":"1","oldGASAccount":"2","newGASAccount":"3","bakerReward":"4","passiveReward":"5","foundationCharge":"6","bakerId":42}}),2,SENDER,"").await;
    assert_eq!(accrued["total"], "0");
    assert_eq!(accrued["details"]["type"], "blockAccrueReward");
}
