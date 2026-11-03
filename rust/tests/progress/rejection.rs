//! This module checks legacy English rejection wording in successful status responses.
//! It preserves historical typos and Haskell formatting for dynamic values.

use super::*;
use concordium_rust_sdk::{
    protocol_level_tokens as plt,
    types::{
        Address, BakerId, ContractAddress, RejectReason as Reason,
        smart_contracts::{OwnedContractName, OwnedReceiveName},
    },
};

async fn reject(reason: Reason, message: &str) {
    check(
        fixtures::account(Effect::None {
            transaction_type: None,
            reject_reason: Upward::Known(reason),
        }),
        json!({"outcome":"reject","rejectReason":message}),
    )
    .await;
}

#[tokio::test]
async fn static_rejection_descriptions_keep_legacy_language_and_quirks() {
    for (reason, message) in [
        (Reason::ModuleNotWF, "Typechecking of module failed"),
        (Reason::SerializationFailure, "Malformed transaction body"),
        (Reason::OutOfEnergy, "Insufficient energy"),
        (Reason::InvalidProof, "Invalid proof"),
        (
            Reason::RuntimeFailure,
            "Runtime failure when executing smart contract.",
        ),
        (
            Reason::KeyIndexAlreadyInUse,
            "The requested key index is already in use.",
        ),
        (
            Reason::InvalidCredentialKeySignThreshold,
            "The requested threshold would exceed the number of keys on the credential.",
        ),
        (
            Reason::InvalidAccountThreshold,
            "The requested account threshold would exceed the number of credentials.",
        ),
        (
            Reason::InvalidEncryptedAmountTransferProof,
            "The encrypted amount transfer has an invalid proof.",
        ),
        (
            Reason::InvalidTransferToPublicProof,
            "The secret to public transfer has an invalid proof.",
        ),
        (
            Reason::InvalidIndexOnEncryptedTransfer,
            "The provided encryped amount index is out of bounds.",
        ),
        (
            Reason::ZeroScheduledAmount,
            "Attempt to transfer 0 CCD with schedule.",
        ),
        (
            Reason::NonIncreasingSchedule,
            "Attempt to transfer amount with non-increasing schedule.",
        ),
        (
            Reason::FirstScheduledReleaseExpired,
            "The first scheduled release is in the past.",
        ),
        (
            Reason::InsufficientBalanceForBakerStake,
            "Sender account has insufficient balance to cover the requested stake.",
        ),
        (
            Reason::InsufficientBalanceForDelegationStake,
            "Sender account has insufficient balance to cover the requested stake.",
        ),
        (
            Reason::BakerInCooldown,
            "Request to make change to the validator while the validator is in the cooldown period.",
        ),
        (
            Reason::DelegatorInCooldown,
            "Request to make change to the validator while the validator is in the cooldown period.",
        ),
        (
            Reason::NonExistentCredentialID,
            "Credential ID does not exist.",
        ),
        (
            Reason::InvalidCredentials,
            "One or more of the credentials is invalid.",
        ),
        (
            Reason::RemoveFirstCredential,
            "Attempt to remove the first credential.",
        ),
        (
            Reason::CredentialHolderDidNotSign,
            "Credential holder did not sign the key update.",
        ),
        (
            Reason::StakeUnderMinimumThresholdForBaking,
            "Desired stake is below the minimum threshold.",
        ),
        (
            Reason::NotAllowedMultipleCredentials,
            "The account is not allowed to have multiple credentials.",
        ),
        (
            Reason::NotAllowedToReceiveEncrypted,
            "The account is not allowed to receive encrypted transfers.",
        ),
        (
            Reason::NotAllowedToHandleEncrypted,
            "The account is not allowed to handle encrypted amounts.",
        ),
        (
            Reason::MissingBakerAddParameters,
            "One or more arguments are missing in order to add a validator.",
        ),
        (
            Reason::FinalizationRewardCommissionNotInRange,
            "Finalization reward commission rate is not within the allowed range.",
        ),
        (
            Reason::BakingRewardCommissionNotInRange,
            "Block reward commission rate is not within the allowed range.",
        ),
        (
            Reason::TransactionFeeCommissionNotInRange,
            "Transaction fee commission rate is not within the allowed range.",
        ),
        (
            Reason::AlreadyADelegator,
            "Attempt to add validator that is already a validator.",
        ),
        (
            Reason::MissingDelegationAddParameters,
            "One or more arguments are missing in order to add a delegator.",
        ),
        (
            Reason::StakeOverMaximumThresholdForPool,
            "Stake is above maximum threshold.",
        ),
        (
            Reason::PoolWouldBecomeOverDelegated,
            "Delegation stake is too high, over-delegation is not allowed.",
        ),
        (
            Reason::PoolClosed,
            "Delegation to a closed pool is not allowed.",
        ),
        (
            Reason::InsufficientDelegationStake,
            "Adding a delegator with 0 stake is not allowed.",
        ),
        (
            Reason::DuplicateCredIDs { contents: vec![] },
            "One or more of the credential IDs is duplicate.",
        ),
        (
            Reason::NonExistentCredIDs { contents: vec![] },
            "One or more of the credential IDs does not exist.",
        ),
        (
            Reason::DelegationTargetNotABaker {
                target: BakerId { id: 42.into() },
            },
            "Delegation target is not a validator.",
        ),
        (
            Reason::EncryptedAmountSelfTransfer {
                contents: fixtures::SENDER,
            },
            "An encrypted amount transfer from the account to itself is not allowed.",
        ),
        (
            Reason::ScheduledSelfTransfer {
                contents: fixtures::SENDER,
            },
            "Attempt to transfer from account A to A with schedule.",
        ),
    ] {
        reject(reason, message).await;
    }
}

#[tokio::test]
async fn dynamic_rejection_descriptions_use_legacy_addresses_names_and_codes() {
    let address = ContractAddress {
        index: 7,
        subindex: 8,
    };
    reject(
        Reason::DuplicateAggregationKey {
            contents: Box::new(fixtures::baker_keys().aggregation_key),
        },
        "Duplicate aggregation key.",
    )
    .await;
    reject(
        Reason::InvalidAccountReference {
            contents: fixtures::SENDER,
        },
        &format!("The account {SENDER} does not exist"),
    )
    .await;
    reject(
        Reason::NotABaker {
            contents: fixtures::SENDER,
        },
        &format!("Account {SENDER} is not a validator."),
    )
    .await;
    reject(
        Reason::NotADelegator {
            address: fixtures::SENDER,
        },
        &format!("Account {SENDER} is not a delegator."),
    )
    .await;
    reject(
        Reason::AlreadyABaker {
            contents: BakerId { id: 42.into() },
        },
        "Validator with ID 42 already exists.",
    )
    .await;
    reject(
        Reason::InvalidContractAddress { contents: address },
        "No smart contract instance exists with address <7, 8>",
    )
    .await;
    reject(
        Reason::AmountTooLarge {
            contents: (Address::Contract(address), Amount::from_micro_ccd(1)),
        },
        "The sending account (<7, 8>) has insufficient funds",
    )
    .await;
    reject(
        Reason::AmountTooLarge {
            contents: (
                Address::Account(fixtures::SENDER),
                Amount::from_micro_ccd(u64::MAX),
            ),
        },
        &format!("The sending account ({SENDER}) has insufficient funds"),
    )
    .await;
    reject(
        Reason::ModuleHashAlreadyExists {
            contents: [5; 32].into(),
        },
        &format!("A module with the hash {} already exists", "05".repeat(32)),
    )
    .await;
    reject(
        Reason::InvalidModuleReference {
            contents: [5; 32].into(),
        },
        &format!("Module does not exist: {}", "05".repeat(32)),
    )
    .await;
    reject(
        Reason::InvalidInitMethod {
            contents: (
                [5; 32].into(),
                OwnedContractName::new("init_test".into()).unwrap(),
            ),
        },
        &format!(
            "Init method \"init_test\" does not exist in module {}.",
            "05".repeat(32)
        ),
    )
    .await;
    reject(
        Reason::InvalidReceiveMethod {
            contents: (
                [5; 32].into(),
                OwnedReceiveName::new("test.receive".into()).unwrap(),
            ),
        },
        &format!(
            "Receive method \"test.receive\" does not exist for module {}.",
            "05".repeat(32)
        ),
    )
    .await;
    reject(
        Reason::RejectedInit { reject_reason: -2 },
        "Failed contract initialization due to contract logic with error code -2",
    )
    .await;
    reject(
        Reason::RejectedReceive {
            reject_reason: i32::MIN,
            contract_address: address,
            receive_name: OwnedReceiveName::new("test.receive".into()).unwrap(),
            parameter: vec![0, 255].try_into().unwrap(),
        },
        "Failed contract receive invocation due to contract logic with reason -2147483648",
    )
    .await;
    reject(
        Reason::NonExistentTokenId {
            token_id: "USD".parse().unwrap(),
        },
        "Non existing plt token id USD.",
    )
    .await;
}

#[tokio::test]
async fn token_rejection_records_preserve_optional_details_and_byte_string_show_escapes() {
    for (details, expected) in [
        (None, "Nothing"),
        (Some(vec![0xa0].into()), "Just a0"),
        (Some(vec![].into()), "Just "),
    ] {
        reject(Reason::TokenUpdateTransactionFailed { reject_reason:plt::EncodedTokenModuleRejectReason { token_id:"USD".parse().unwrap(), reason_type:"reason".parse().unwrap(),details } }, &format!("Token update transaction failed with reason: TokenModuleRejectReason {{tmrrTokenId = USD, tmrrType = \"reason\", tmrrDetails = {expected}}}.")).await;
    }
    reject(Reason::TokenUpdateTransactionFailed { reject_reason:plt::EncodedTokenModuleRejectReason { token_id:"USD".parse().unwrap(),reason_type:"é1".parse().unwrap(),details:None } }, "Token update transaction failed with reason: TokenModuleRejectReason {tmrrTokenId = USD, tmrrType = \"\\195\\169\\&1\", tmrrDetails = Nothing}.").await;
}

#[tokio::test]
async fn unknown_summaries_effects_and_reasons_fail_without_partial_outcomes() {
    let mut unknown = transfer();
    unknown.details = Upward::Unknown(());
    let mut unknown_effect = transfer();
    if let Upward::Known(concordium_rust_sdk::types::BlockItemSummaryDetails::AccountTransaction(
        account,
    )) = &mut unknown_effect.details
    {
        account.effects = Upward::Unknown(());
    }
    for summary in [
        unknown,
        unknown_effect,
        fixtures::account(Effect::None {
            transaction_type: None,
            reject_reason: Upward::Unknown(()),
        }),
        fixtures::account(Effect::None {
            transaction_type: None,
            reject_reason: Upward::Known(Reason::NonExistentLockId {
                lock_id: plt::LockId {
                    account_index: 42,
                    sequence_number: 1,
                    creation_order: 0,
                },
            }),
        }),
    ] {
        assert_eq!(
            request(
                progress::routes(node(finalized(summary))),
                &format!("/v0/submissionStatus/{HASH}")
            )
            .await
            .0,
            500
        );
    }
}
