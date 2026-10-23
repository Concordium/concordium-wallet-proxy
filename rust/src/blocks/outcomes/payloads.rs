//! This module projects legacy governance updates and rejection reasons.
//! It keeps historical payload names and known Haskell serialization quirks.

use super::{encode, select};
use crate::{
    compatibility::{fraction, mint_rate},
    network::NetworkFailure,
};
use concordium_rust_sdk::types::{AmountFraction, RejectReason, UpdatePayload};
use serde_json::{Value, json};

pub(super) fn update_type(payload: &UpdatePayload) -> Result<Value, NetworkFailure> {
    use concordium_rust_sdk::types::{Level1Update, RootUpdate};
    match payload {
        UpdatePayload::Root(root) => Ok(json!(match root {
            RootUpdate::RootKeysUpdate(_) => "updateRootKeys",
            RootUpdate::Level1KeysUpdate(_) => "updateLevel1Keys",
            _ => "updateLevel2Keys",
        })),
        UpdatePayload::Level1(level1) => Ok(json!(match level1 {
            Level1Update::Level1KeysUpdate(_) => "updateLevel1Keys",
            _ => "updateLevel2Keys",
        })),
        UpdatePayload::GASRewardsCPV2(_) => Ok(json!("updateGASRewards")),
        _ => encode(payload.update_type()),
    }
}

pub(super) fn update(payload: &UpdatePayload) -> Result<Value, NetworkFailure> {
    use UpdatePayload::*;
    let (name, value) = match payload {
        Protocol(value) => (
            "protocol",
            select(
                value,
                &[
                    "message",
                    "specificationURL",
                    "specificationHash",
                    "specificationAuxiliaryData",
                ],
            )?,
        ),
        ElectionDifficulty(value) => ("electionDifficulty", encode(value)?),
        EuroPerEnergy(value) => (
            "euroPerEnergy",
            json!({"numerator": value.numerator(), "denominator": value.denominator()}),
        ),
        MicroGTUPerEuro(value) => (
            "microGTUPerEuro",
            json!({"numerator": value.numerator(), "denominator": value.denominator()}),
        ),
        FoundationAccount(value) => ("foundationAccount", json!(value)),
        MintDistribution(value) => (
            "mintDistribution",
            json!({"mintPerSlot": mint_rate(value.mint_per_slot), "bakingReward": fraction(value.baking_reward), "finalizationReward": fraction(value.finalization_reward)}),
        ),
        TransactionFeeDistribution(value) => (
            "transactionFeeDistribution",
            json!({"baker": fraction(value.baker), "gasAccount": fraction(value.gas_account)}),
        ),
        GASRewards(value) => (
            "gASRewards",
            json!({"baker": fraction(value.baker), "finalizationProof": fraction(value.finalization_proof), "accountCreation": fraction(value.account_creation), "chainUpdate": fraction(value.chain_update)}),
        ),
        BakerStakeThreshold(value) => (
            "bakerStakeThreshold",
            json!({"minimumThresholdForBaking": value.minimum_threshold_for_baking.micro_ccd.to_string()}),
        ),
        Root(value) => ("root", super::keys::root(value)?),
        Level1(value) => ("level1", super::keys::level1(value)?),
        AddAnonymityRevoker(value) => (
            "addAnonymityRevoker",
            select(value, &["arIdentity", "arDescription", "arPublicKey"])?,
        ),
        AddIdentityProvider(value) => (
            "addIdentityProvider",
            select(
                value,
                &[
                    "ipIdentity",
                    "ipDescription",
                    "ipVerifyKey",
                    "ipCdiVerifyKey",
                ],
            )?,
        ),
        CooldownParametersCPV1(value) => (
            "cooldownParametersCPV1",
            json!({"poolOwnerCooldown": value.pool_owner_cooldown.seconds, "delegatorCooldown": value.delegator_cooldown.seconds}),
        ),
        PoolParametersCPV1(value) => (
            "poolParametersCPV1",
            select(
                value,
                &[
                    "passiveFinalizationCommission",
                    "passiveBakingCommission",
                    "passiveTransactionCommission",
                    "finalizationCommissionRange",
                    "bakingCommissionRange",
                    "transactionCommissionRange",
                    "minimumEquityCapital",
                    "capitalBound",
                    "leverageBound",
                ],
            )?,
        ),
        TimeParametersCPV1(value) => (
            "timeParametersCPV1",
            json!({"rewardPeriodLength": value.reward_period_length.reward_period_epochs().epoch, "mintPerPayday": mint_rate(value.mint_per_payday)}),
        ),
        MintDistributionCPV1(value) => (
            "mintDistributionCPV1",
            json!({"bakingReward": fraction(value.baking_reward), "finalizationReward": fraction(value.finalization_reward)}),
        ),
        GASRewardsCPV2(value) => (
            "gASRewardsCPV2",
            json!({"baker": fraction(value.baker), "accountCreation": fraction(value.account_creation), "chainUpdate": fraction(value.chain_update)}),
        ),
        TimeoutParametersCPV2(value) => (
            "timeoutParameters",
            json!({"timeoutBase": value.base.millis(), "timeoutIncrease": {"numerator": value.increase.numerator(), "denominator": value.increase.denominator()}, "timeoutDecrease": {"numerator": value.decrease.numerator(), "denominator": value.decrease.denominator()}}),
        ),
        MinBlockTimeCPV2(value) => ("minBlockTime", json!(value.millis())),
        BlockEnergyLimitCPV2(value) => ("blockEnergyLimit", json!(value)),
        FinalizationCommitteeParametersCPV2(value) => {
            let parts: u32 = value.finalizers_relative_stake_threshold.into();
            // Preserve the swapped min/max keys in Haskell's ToJSON instance.
            (
                "finalizationCommitteeParameters",
                json!({"maximumFinalizers": value.min_finalizers, "minimumFinalizers": value.max_finalizers, "finalizerRelativeStakeThreshold": fraction(AmountFraction::new_unchecked(parts))}),
            )
        }
        ValidatorScoreParametersCPV3(value) => (
            "validatorScoreParameters",
            json!({"maximumMissedRounds": value.max_missed_rounds}),
        ),
        CreatePlt(value) => (
            "createPLT",
            json!({"tokenId": value.token_id, "tokenModule": value.token_module, "decimals": value.decimals, "initializationParameters": value.initialization_parameters}),
        ),
        MaxLockDuration(_) => return Err(NetworkFailure::InvalidResponse),
    };
    Ok(json!({"updateType": name, "update": value}))
}

pub(super) fn reject(reason: &RejectReason) -> Result<Value, NetworkFailure> {
    if let RejectReason::TokenUpdateTransactionFailed { reject_reason } = reason {
        let mut details =
            json!({"tokenId": reject_reason.token_id, "type": reject_reason.reason_type});
        if let Some(bytes) = &reject_reason.details {
            details["details"] = json!(bytes);
        }
        return Ok(json!({"tag": "TokenUpdateTransactionFailed", "contents": details}));
    }
    let value = encode(reason)?;
    let tag = value
        .get("tag")
        .and_then(Value::as_str)
        .ok_or(NetworkFailure::InvalidResponse)?;
    // This list follows the legacy RejectReason enum, not all newer SDK reasons.
    if !matches!(
        tag,
        "ModuleNotWF"
            | "ModuleHashAlreadyExists"
            | "InvalidAccountReference"
            | "InvalidInitMethod"
            | "InvalidReceiveMethod"
            | "InvalidModuleReference"
            | "InvalidContractAddress"
            | "RuntimeFailure"
            | "AmountTooLarge"
            | "SerializationFailure"
            | "OutOfEnergy"
            | "RejectedInit"
            | "RejectedReceive"
            | "InvalidProof"
            | "AlreadyABaker"
            | "NotABaker"
            | "InsufficientBalanceForBakerStake"
            | "StakeUnderMinimumThresholdForBaking"
            | "BakerInCooldown"
            | "DuplicateAggregationKey"
            | "NonExistentCredentialID"
            | "KeyIndexAlreadyInUse"
            | "InvalidAccountThreshold"
            | "InvalidCredentialKeySignThreshold"
            | "InvalidEncryptedAmountTransferProof"
            | "InvalidTransferToPublicProof"
            | "EncryptedAmountSelfTransfer"
            | "InvalidIndexOnEncryptedTransfer"
            | "ZeroScheduledAmount"
            | "NonIncreasingSchedule"
            | "FirstScheduledReleaseExpired"
            | "ScheduledSelfTransfer"
            | "InvalidCredentials"
            | "DuplicateCredIDs"
            | "NonExistentCredIDs"
            | "RemoveFirstCredential"
            | "CredentialHolderDidNotSign"
            | "NotAllowedMultipleCredentials"
            | "NotAllowedToReceiveEncrypted"
            | "NotAllowedToHandleEncrypted"
            | "MissingBakerAddParameters"
            | "FinalizationRewardCommissionNotInRange"
            | "BakingRewardCommissionNotInRange"
            | "TransactionFeeCommissionNotInRange"
            | "AlreadyADelegator"
            | "InsufficientBalanceForDelegationStake"
            | "MissingDelegationAddParameters"
            | "InsufficientDelegationStake"
            | "DelegatorInCooldown"
            | "NotADelegator"
            | "DelegationTargetNotABaker"
            | "StakeOverMaximumThresholdForPool"
            | "PoolWouldBecomeOverDelegated"
            | "PoolClosed"
            | "NonExistentTokenId"
            | "TokenUpdateTransactionFailed"
    ) {
        return Err(NetworkFailure::InvalidResponse);
    }
    let mut output = json!({"tag": tag});
    for field in [
        "contents",
        "rejectReason",
        "contractAddress",
        "receiveName",
        "parameter",
    ] {
        if let Some(field_value) = value.get(field) {
            output[field] = field_value.clone();
        }
    }
    Ok(output)
}
