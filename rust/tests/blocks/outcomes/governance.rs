//! This module checks free account creation and governance outcomes.
//! It fixes expected legacy update names and the historical committee min/max swap.

use super::*;
use concordium_rust_sdk::common::types::Ratio;

#[tokio::test]
async fn account_creation_keeps_null_sender_sponsor_and_zero_cost() {
    for credential_type in [CredentialType::Initial, CredentialType::Normal] {
        let mut summary = fixtures::account(AccountTransactionEffects::DataRegistered {
            data: vec![].try_into().unwrap(),
        });
        summary.details = Upward::Known(BlockItemSummaryDetails::AccountCreation(
            AccountCreationDetails {
                credential_type,
                address: SENDER,
                reg_id: fixtures::credential(),
            },
        ));
        let mut value = expected("summary");
        value["sender"] = Value::Null;
        value["cost"] = json!("0");
        value["type"] = json!({"type":"credentialDeploymentTransaction","contents":if matches!(credential_type, CredentialType::Initial) {"initial"}else{"normal"}});
        value["result"] = json!({"outcome":"success","events":[tag("AccountCreated",json!({"contents":account()})),tag("CredentialDeployed",json!({"regId":format!("c0{}","00".repeat(47)),"account":account()}))]});
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
            (200, json!([value]))
        );
    }
}

#[tokio::test]
async fn governance_key_versions_do_not_leak_new_authorization_fields() {
    let parameters: concordium_rust_sdk::types::chain_parameters::ChainParameters =
        crate::staking::fixtures::chain(1).try_into().unwrap();
    let mut auth: AuthorizationsV1 = parameters.keys.level_2_keys.unwrap().try_into().unwrap();
    let access = json!({"authorizedKeys":[0],"threshold":1});
    let mut v0 = json!({"keys":[{"schemeId":"Ed25519","verifyKey":"01".repeat(32)}]});
    for key in [
        "emergency",
        "protocol",
        "electionDifficulty",
        "euroPerEnergy",
        "microGTUPerEuro",
        "foundationAccount",
        "mintDistribution",
        "transactionFeeDistribution",
        "paramGASRewards",
        "poolParameters",
        "addAnonymityRevoker",
        "addIdentityProvider",
    ] {
        v0[key] = access.clone();
    }
    let mut v1 = v0.clone();
    v1["cooldownParameters"] = access.clone();
    v1["timeParameters"] = access.clone();
    let mut v2 = v1.clone();
    v2["createPLT"] = access;
    let cases = vec![
        (
            UpdatePayload::Root(RootUpdate::RootKeysUpdate(
                parameters.keys.root_keys.unwrap(),
            )),
            "root",
            "rootKeysUpdate",
            json!({"keys":[{"schemeId":"Ed25519","verifyKey":"01".repeat(32)}],"threshold":1}),
        ),
        (
            UpdatePayload::Level1(Level1Update::Level1KeysUpdate(
                parameters.keys.level_1_keys.unwrap(),
            )),
            "level1",
            "level1KeysUpdate",
            json!({"keys":[{"schemeId":"Ed25519","verifyKey":"01".repeat(32)}],"threshold":1}),
        ),
        (
            UpdatePayload::Root(RootUpdate::Level2KeysUpdate(Box::new(auth.v0.clone()))),
            "root",
            "level2KeysUpdate",
            v0.clone(),
        ),
        (
            UpdatePayload::Level1(Level1Update::Level2KeysUpdate(Box::new(auth.v0.clone()))),
            "level1",
            "level2KeysUpdate",
            v0,
        ),
        (
            UpdatePayload::Root(RootUpdate::Level2KeysUpdateV1(Box::new(auth.clone()))),
            "root",
            "level2KeysUpdateV1",
            v1.clone(),
        ),
        (
            UpdatePayload::Level1(Level1Update::Level2KeysUpdateV1(Box::new(auth.clone()))),
            "level1",
            "level2KeysUpdateV1",
            v1,
        ),
    ];
    auth.create_plt = Some(auth.cooldown_parameters.clone());
    let cases = cases.into_iter().chain([
        (
            UpdatePayload::Root(RootUpdate::Level2KeysUpdateV2(Box::new(auth.clone()))),
            "root",
            "level2KeysUpdateV2",
            v2.clone(),
        ),
        (
            UpdatePayload::Level1(Level1Update::Level2KeysUpdateV2(Box::new(auth))),
            "level1",
            "level2KeysUpdateV2",
            v2,
        ),
    ]);
    for (payload, category, kind, body) in cases {
        let mut summary = fixtures::account(AccountTransactionEffects::DataRegistered {
            data: vec![].try_into().unwrap(),
        });
        let update_type = match kind {
            "rootKeysUpdate" => "updateRootKeys",
            "level1KeysUpdate" => "updateLevel1Keys",
            _ => "updateLevel2Keys",
        };
        summary.details = Upward::Known(BlockItemSummaryDetails::Update(UpdateDetails {
            effective_time: TransactionTime { seconds: 123 },
            payload: Upward::Known(payload),
        }));
        let mut value = expected("summary");
        value["sender"] = Value::Null;
        value["cost"] = json!("0");
        value["type"] = json!({"type":"updateTransaction","contents":update_type});
        value["result"] = json!({"outcome":"success","events":[tag("UpdateEnqueued",json!({"effectiveTime":123,"payload":{"updateType":category,"update":{"typeOfUpdate":kind,"updatePayload":body}}}))]});
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
            (200, json!([value])),
            "{category}:{kind}"
        );
    }
}

#[tokio::test]
async fn update_payloads_preserve_legacy_names_exact_ratios_and_committee_quirk() {
    let timeout = TimeoutParameters::new(
        concordium_rust_sdk::smart_contracts::common::Duration::from_millis(1000),
        Ratio::new(3, 2).unwrap(),
        Ratio::new(3, 4).unwrap(),
    )
    .unwrap();
    let threshold = PartsPerHundredThousands::new(25000).unwrap();
    let rate: ExchangeRate = p::ExchangeRate {
        value: Some(p::Ratio {
            numerator: 1,
            denominator: 10,
        }),
    }
    .try_into()
    .unwrap();
    let reward_period_length = p::RewardPeriodLength {
        value: Some(p::Epoch { value: 2 }),
    }
    .try_into()
    .unwrap();
    let cases = vec![
        (
            UpdatePayload::Protocol(ProtocolUpdate {
                message: "upgrade".into(),
                specification_url: "https://protocol.example".into(),
                specification_hash: [9; 32].into(),
                specification_auxiliary_data: vec![0, 255],
            }),
            "updateProtocol",
            "protocol",
            json!({"message":"upgrade","specificationURL":"https://protocol.example","specificationHash":"09".repeat(32),"specificationAuxiliaryData":"00ff"}),
        ),
        (
            UpdatePayload::ElectionDifficulty(ElectionDifficulty::new(25000).unwrap()),
            "updateElectionDifficulty",
            "electionDifficulty",
            // The SDK scalar retains five decimal places; the numeric value is 0.25.
            serde_json::from_str("0.25000").unwrap(),
        ),
        (
            UpdatePayload::TransactionFeeDistribution(TransactionFeeDistribution {
                baker: AmountFraction::new(20000).unwrap(),
                gas_account: AmountFraction::new(30000).unwrap(),
            }),
            "updateTransactionFeeDistribution",
            "transactionFeeDistribution",
            json!({"baker":0.2,"gasAccount":0.3}),
        ),
        (
            UpdatePayload::GASRewards(GASRewards {
                baker: AmountFraction::new(10000).unwrap(),
                finalization_proof: AmountFraction::new(20000).unwrap(),
                account_creation: AmountFraction::new(30000).unwrap(),
                chain_update: AmountFraction::new(40000).unwrap(),
            }),
            "updateGASRewards",
            "gASRewards",
            json!({"baker":0.1,"finalizationProof":0.2,"accountCreation":0.3,"chainUpdate":0.4}),
        ),
        (
            UpdatePayload::GASRewardsCPV2(GASRewardsV1 {
                baker: AmountFraction::new(10000).unwrap(),
                account_creation: AmountFraction::new(30000).unwrap(),
                chain_update: AmountFraction::new(40000).unwrap(),
            }),
            "updateGASRewards",
            "gASRewardsCPV2",
            json!({"baker":0.1,"accountCreation":0.3,"chainUpdate":0.4}),
        ),
        (
            UpdatePayload::BakerStakeThreshold(BakerParameters {
                minimum_threshold_for_baking: Amount::from_micro_ccd(u64::MAX),
            }),
            "updatePoolParameters",
            "bakerStakeThreshold",
            json!({"minimumThresholdForBaking":"18446744073709551615"}),
        ),
        (
            UpdatePayload::CooldownParametersCPV1(CooldownParameters {
                pool_owner_cooldown: DurationSeconds { seconds: 3600 },
                delegator_cooldown: DurationSeconds { seconds: 7200 },
            }),
            "updateCooldownParameters",
            "cooldownParametersCPV1",
            json!({"poolOwnerCooldown":3600,"delegatorCooldown":7200}),
        ),
        (
            UpdatePayload::CreatePlt(CreatePlt {
                token_id: "USD".parse().unwrap(),
                token_module: [7; 32].into(),
                decimals: 6,
                initialization_parameters: vec![0xa0].into(),
            }),
            "updateCreatePLT",
            "createPLT",
            json!({"tokenId":"USD","tokenModule":"07".repeat(32),"decimals":6,"initializationParameters":"a0"}),
        ),
        (
            UpdatePayload::EuroPerEnergy(rate),
            "updateEuroPerEnergy",
            "euroPerEnergy",
            json!({"numerator":1,"denominator":10}),
        ),
        (
            UpdatePayload::MicroGTUPerEuro(rate),
            "updateMicroGTUPerEuro",
            "microGTUPerEuro",
            json!({"numerator":1,"denominator":10}),
        ),
        (
            UpdatePayload::FoundationAccount(RECEIVER),
            "updateFoundationAccount",
            "foundationAccount",
            json!("2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3"),
        ),
        (
            UpdatePayload::MintDistribution(MintDistributionV0 {
                mint_per_slot: MintRate {
                    mantissa: 25,
                    exponent: 4,
                },
                baking_reward: AmountFraction::new(20000).unwrap(),
                finalization_reward: AmountFraction::new(30000).unwrap(),
            }),
            "updateMintDistribution",
            "mintDistribution",
            json!({"mintPerSlot":0.0025,"bakingReward":0.2,"finalizationReward":0.3}),
        ),
        (
            UpdatePayload::MintDistributionCPV1(MintDistributionV1 {
                baking_reward: AmountFraction::new(20000).unwrap(),
                finalization_reward: AmountFraction::new(30000).unwrap(),
            }),
            "updateMintDistribution",
            "mintDistributionCPV1",
            json!({"bakingReward":0.2,"finalizationReward":0.3}),
        ),
        (
            UpdatePayload::TimeParametersCPV1(TimeParameters {
                reward_period_length,
                mint_per_payday: MintRate {
                    mantissa: 25,
                    exponent: 4,
                },
            }),
            "updateTimeParameters",
            "timeParametersCPV1",
            json!({"rewardPeriodLength":2,"mintPerPayday":0.0025}),
        ),
        (
            UpdatePayload::TimeoutParametersCPV2(timeout),
            "updateTimeoutParameters",
            "timeoutParameters",
            json!({"timeoutBase":1000,"timeoutIncrease":{"numerator":3,"denominator":2},"timeoutDecrease":{"numerator":3,"denominator":4}}),
        ),
        (
            UpdatePayload::MinBlockTimeCPV2(
                concordium_rust_sdk::smart_contracts::common::Duration::from_millis(500),
            ),
            "updateMinBlockTime",
            "minBlockTime",
            json!(500),
        ),
        (
            UpdatePayload::BlockEnergyLimitCPV2(Energy { energy: u64::MAX }),
            "updateBlockEnergyLimit",
            "blockEnergyLimit",
            json!(u64::MAX),
        ),
        (
            UpdatePayload::FinalizationCommitteeParametersCPV2(FinalizationCommitteeParameters {
                min_finalizers: 2,
                max_finalizers: 10,
                finalizers_relative_stake_threshold: threshold,
            }),
            "updateFinalizationCommitteeParameters",
            "finalizationCommitteeParameters",
            json!({"maximumFinalizers":2,"minimumFinalizers":10,"finalizerRelativeStakeThreshold":0.25}),
        ),
        (
            UpdatePayload::ValidatorScoreParametersCPV3(ValidatorScoreParameters {
                max_missed_rounds: u64::MAX,
            }),
            "updateValidatorScoreParameters",
            "validatorScoreParameters",
            json!({"maximumMissedRounds":u64::MAX}),
        ),
    ];
    for (payload, kind, name, body) in cases {
        let mut summary = fixtures::account(AccountTransactionEffects::DataRegistered {
            data: vec![].try_into().unwrap(),
        });
        summary.details = Upward::Known(BlockItemSummaryDetails::Update(UpdateDetails {
            effective_time: TransactionTime { seconds: 123 },
            payload: Upward::Known(payload),
        }));
        let mut value = expected("summary");
        value["sender"] = Value::Null;
        value["cost"] = json!("0");
        value["type"] = json!({"type":"updateTransaction","contents":kind});
        value["result"] = json!({"outcome":"success","events":[tag("UpdateEnqueued",json!({"effectiveTime":123,"payload":{"updateType":name,"update":body}}))]});
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
            (200, json!([value])),
            "{name}"
        );
    }
}
