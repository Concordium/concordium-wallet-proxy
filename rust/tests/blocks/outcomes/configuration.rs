//! This module checks every legacy baker and delegation configuration event.
//! Expected keys and actor attribution follow the Haskell Event ToJSON instance.

use super::*;

#[tokio::test]
async fn configuration_events_preserve_every_tag_and_sender_attribution() {
    let mut key_fields = fixtures::key_fields(&fixtures::baker_keys());
    key_fields["stake"] = json!("1000");
    key_fields["restakeEarnings"] = json!(false);
    let mut input = vec![
        BakerEvent::BakerAdded {
            data: Box::new(BakerAddedEvent {
                keys_event: fixtures::baker_keys(),
                stake: Amount::from_micro_ccd(1000),
                restake_earnings: false,
            }),
        },
        BakerEvent::BakerKeysUpdated {
            data: Box::new(fixtures::baker_keys()),
        },
    ];
    let mut expected = vec![
        tag("BakerAdded", key_fields),
        tag(
            "BakerKeysUpdated",
            fixtures::key_fields(&fixtures::baker_keys()),
        ),
    ];
    let amount = Amount::from_micro_ccd(1000);
    let id = BAKER;
    input.extend([
        BakerEvent::BakerRemoved { baker_id: id },
        BakerEvent::BakerStakeIncreased {
            baker_id: id,
            new_stake: amount,
        },
        BakerEvent::BakerStakeDecreased {
            baker_id: id,
            new_stake: amount,
        },
        BakerEvent::BakerRestakeEarningsUpdated {
            baker_id: id,
            restake_earnings: false,
        },
        BakerEvent::BakerSetOpenStatus {
            baker_id: id,
            open_status: Upward::Known(OpenStatus::ClosedForAll),
        },
        BakerEvent::BakerSetMetadataURL {
            baker_id: id,
            metadata_url: "https://pool.example".to_string().try_into().unwrap(),
        },
        BakerEvent::BakerSetTransactionFeeCommission {
            baker_id: id,
            transaction_fee_commission: AmountFraction::new(10_000).unwrap(),
        },
        BakerEvent::BakerSetBakingRewardCommission {
            baker_id: id,
            baking_reward_commission: AmountFraction::new(20_000).unwrap(),
        },
        BakerEvent::BakerSetFinalizationRewardCommission {
            baker_id: id,
            finalization_reward_commission: AmountFraction::new(30_000).unwrap(),
        },
        BakerEvent::DelegationRemoved {
            delegator_id: DELEGATOR,
        },
        BakerEvent::BakerSuspended { baker_id: id },
        BakerEvent::BakerResumed { baker_id: id },
    ]);
    for (name, fields) in [
        ("BakerRemoved", json!({"bakerId":42})),
        (
            "BakerStakeIncreased",
            json!({"bakerId":42,"newStake":"1000"}),
        ),
        (
            "BakerStakeDecreased",
            json!({"bakerId":42,"newStake":"1000"}),
        ),
        (
            "BakerSetRestakeEarnings",
            json!({"bakerId":42,"restakeEarnings":false}),
        ),
        (
            "BakerSetOpenStatus",
            json!({"bakerId":42,"openStatus":"closedForAll"}),
        ),
        (
            "BakerSetMetadataURL",
            json!({"bakerId":42,"metadataURL":"https://pool.example"}),
        ),
        (
            "BakerSetTransactionFeeCommission",
            json!({"bakerId":42,"transactionFeeCommission":0.1}),
        ),
        (
            "BakerSetBakingRewardCommission",
            json!({"bakerId":42,"bakingRewardCommission":0.2}),
        ),
        (
            "BakerSetFinalizationRewardCommission",
            json!({"bakerId":42,"finalizationRewardCommission":0.3}),
        ),
        ("DelegationRemoved", json!({"delegatorId":42})),
        ("BakerSuspended", json!({"bakerId":42})),
        ("BakerResumed", json!({"bakerId":42})),
    ] {
        let mut fields = fields;
        fields["account"] = account();
        expected.push(tag(name, fields));
    }
    check(
        AccountTransactionEffects::BakerConfigured {
            data: input.into_iter().map(Upward::Known).collect(),
        },
        "configureBaker",
        json!(expected),
    )
    .await;
    let input = vec![
        DelegationEvent::DelegationStakeIncreased {
            delegator_id: DELEGATOR,
            new_stake: amount,
        },
        DelegationEvent::DelegationStakeDecreased {
            delegator_id: DELEGATOR,
            new_stake: amount,
        },
        DelegationEvent::DelegationSetRestakeEarnings {
            delegator_id: DELEGATOR,
            restake_earnings: false,
        },
        DelegationEvent::DelegationSetDelegationTarget {
            delegator_id: DELEGATOR,
            delegation_target: DelegationTarget::Passive,
        },
        DelegationEvent::DelegationSetDelegationTarget {
            delegator_id: DELEGATOR,
            delegation_target: DelegationTarget::Baker { baker_id: id },
        },
        DelegationEvent::DelegationAdded {
            delegator_id: DELEGATOR,
        },
        DelegationEvent::DelegationRemoved {
            delegator_id: DELEGATOR,
        },
        DelegationEvent::BakerRemoved { baker_id: id },
    ];
    let mut output = vec![];
    for (name, fields) in [
        (
            "DelegationStakeIncreased",
            json!({"delegatorId":42,"newStake":"1000"}),
        ),
        (
            "DelegationStakeDecreased",
            json!({"delegatorId":42,"newStake":"1000"}),
        ),
        (
            "DelegationSetRestakeEarnings",
            json!({"delegatorId":42,"restakeEarnings":false}),
        ),
        (
            "DelegationSetDelegationTarget",
            json!({"delegatorId":42,"delegationTarget":{"delegateType":"Passive"}}),
        ),
        (
            "DelegationSetDelegationTarget",
            json!({"delegatorId":42,"delegationTarget":{"delegateType":"Baker","bakerId":42}}),
        ),
        ("DelegationAdded", json!({"delegatorId":42})),
        ("DelegationRemoved", json!({"delegatorId":42})),
        ("BakerRemoved", json!({"bakerId":42})),
    ] {
        let mut fields = fields;
        fields["account"] = account();
        output.push(tag(name, fields));
    }
    check(
        AccountTransactionEffects::DelegationConfigured {
            data: input.into_iter().map(Upward::Known).collect(),
        },
        "configureDelegation",
        json!(output),
    )
    .await;
}
