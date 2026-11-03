//! This module renders the legacy English descriptions of rejected transactions.
//! Unsupported future rejection variants fail rather than inventing wallet-facing descriptions.

use crate::{
    compatibility::{show_bytes, show_text},
    network::NetworkFailure,
};
use concordium_rust_sdk::types::{Address, RejectReason as Reason};

/// Render a known rejection reason with the existing English wording and quirks.
pub(super) fn description(reason: &Reason) -> Result<String, NetworkFailure> {
    let value = match reason {
        Reason::ModuleNotWF => "Typechecking of module failed".into(),
        Reason::ModuleHashAlreadyExists { contents } => {
            format!("A module with the hash {contents} already exists")
        }
        Reason::InvalidAccountReference { contents } => {
            format!("The account {contents} does not exist")
        }
        Reason::InvalidModuleReference { contents } => format!("Module does not exist: {contents}"),
        Reason::InvalidContractAddress { contents } => format!(
            "No smart contract instance exists with address {}",
            contract(contents)
        ),
        Reason::AmountTooLarge {
            contents: (address, _),
        } => format!(
            "The sending account ({}) has insufficient funds",
            match address {
                Address::Account(address) => address.to_string(),
                Address::Contract(address) => contract(address),
            }
        ),
        Reason::SerializationFailure => "Malformed transaction body".into(),
        Reason::OutOfEnergy => "Insufficient energy".into(),
        Reason::RejectedInit { reject_reason } => format!(
            "Failed contract initialization due to contract logic with error code {reject_reason}"
        ),
        Reason::RejectedReceive { reject_reason, .. } => format!(
            "Failed contract receive invocation due to contract logic with reason {reject_reason}"
        ),
        Reason::InvalidProof => "Invalid proof".into(),
        Reason::InvalidInitMethod {
            contents: (module, name),
        } => format!(
            "Init method {} does not exist in module {module}.",
            show_text(&name.to_string())
        ),
        Reason::InvalidReceiveMethod {
            contents: (module, name),
        } => format!(
            "Receive method {} does not exist for module {module}.",
            show_text(&name.to_string())
        ),
        Reason::RuntimeFailure => "Runtime failure when executing smart contract.".into(),
        Reason::DuplicateAggregationKey { .. } => "Duplicate aggregation key.".into(),
        Reason::KeyIndexAlreadyInUse => "The requested key index is already in use.".into(),
        Reason::InvalidCredentialKeySignThreshold => {
            "The requested threshold would exceed the number of keys on the credential.".into()
        }
        Reason::InvalidAccountThreshold => {
            "The requested account threshold would exceed the number of credentials.".into()
        }
        Reason::InvalidEncryptedAmountTransferProof => {
            "The encrypted amount transfer has an invalid proof.".into()
        }
        Reason::EncryptedAmountSelfTransfer { .. } => {
            "An encrypted amount transfer from the account to itself is not allowed.".into()
        }
        Reason::InvalidTransferToPublicProof => {
            "The secret to public transfer has an invalid proof.".into()
        }
        Reason::InvalidIndexOnEncryptedTransfer => {
            "The provided encryped amount index is out of bounds.".into()
        }
        Reason::ZeroScheduledAmount => "Attempt to transfer 0 CCD with schedule.".into(),
        Reason::NonIncreasingSchedule => {
            "Attempt to transfer amount with non-increasing schedule.".into()
        }
        Reason::FirstScheduledReleaseExpired => {
            "The first scheduled release is in the past.".into()
        }
        Reason::ScheduledSelfTransfer { .. } => {
            "Attempt to transfer from account A to A with schedule.".into()
        }
        Reason::AlreadyABaker { contents } => {
            format!("Validator with ID {} already exists.", contents.id.index)
        }
        Reason::NotABaker { contents } => format!("Account {contents} is not a validator."),
        Reason::InsufficientBalanceForBakerStake
        | Reason::InsufficientBalanceForDelegationStake => {
            "Sender account has insufficient balance to cover the requested stake.".into()
        }
        Reason::BakerInCooldown | Reason::DelegatorInCooldown => {
            "Request to make change to the validator while the validator is in the cooldown period."
                .into()
        }
        Reason::NonExistentCredentialID => "Credential ID does not exist.".into(),
        Reason::InvalidCredentials => "One or more of the credentials is invalid.".into(),
        Reason::DuplicateCredIDs { .. } => "One or more of the credential IDs is duplicate.".into(),
        Reason::NonExistentCredIDs { .. } => {
            "One or more of the credential IDs does not exist.".into()
        }
        Reason::RemoveFirstCredential => "Attempt to remove the first credential.".into(),
        Reason::CredentialHolderDidNotSign => {
            "Credential holder did not sign the key update.".into()
        }
        Reason::StakeUnderMinimumThresholdForBaking => {
            "Desired stake is below the minimum threshold.".into()
        }
        Reason::NotAllowedMultipleCredentials => {
            "The account is not allowed to have multiple credentials.".into()
        }
        Reason::NotAllowedToReceiveEncrypted => {
            "The account is not allowed to receive encrypted transfers.".into()
        }
        Reason::NotAllowedToHandleEncrypted => {
            "The account is not allowed to handle encrypted amounts.".into()
        }
        Reason::MissingBakerAddParameters => {
            "One or more arguments are missing in order to add a validator.".into()
        }
        Reason::FinalizationRewardCommissionNotInRange => {
            "Finalization reward commission rate is not within the allowed range.".into()
        }
        Reason::BakingRewardCommissionNotInRange => {
            "Block reward commission rate is not within the allowed range.".into()
        }
        Reason::TransactionFeeCommissionNotInRange => {
            "Transaction fee commission rate is not within the allowed range.".into()
        }
        Reason::AlreadyADelegator => "Attempt to add validator that is already a validator.".into(),
        Reason::MissingDelegationAddParameters => {
            "One or more arguments are missing in order to add a delegator.".into()
        }
        Reason::NotADelegator { address } => format!("Account {address} is not a delegator."),
        Reason::DelegationTargetNotABaker { .. } => "Delegation target is not a validator.".into(),
        Reason::StakeOverMaximumThresholdForPool => "Stake is above maximum threshold.".into(),
        Reason::PoolWouldBecomeOverDelegated => {
            "Delegation stake is too high, over-delegation is not allowed.".into()
        }
        Reason::PoolClosed => "Delegation to a closed pool is not allowed.".into(),
        Reason::InsufficientDelegationStake => {
            "Adding a delegator with 0 stake is not allowed.".into()
        }
        Reason::NonExistentTokenId { token_id } => format!("Non existing plt token id {token_id}."),
        Reason::TokenUpdateTransactionFailed { reject_reason } => {
            let details = match &reject_reason.details {
                None => "Nothing".into(),
                Some(bytes) => format!(
                    "Just {}",
                    bytes
                        .as_ref()
                        .iter()
                        .map(|byte| format!("{byte:02x}"))
                        .collect::<String>()
                ),
            };
            format!(
                "Token update transaction failed with reason: TokenModuleRejectReason {{tmrrTokenId = {}, tmrrType = {}, tmrrDetails = {details}}}.",
                reject_reason.token_id,
                show_bytes(reject_reason.reason_type.as_ref().as_bytes())
            )
        }
        // The pinned Haskell client cannot decode these P11 reject reasons.
        Reason::NonExistentLockId { .. }
        | Reason::LockExpired { .. }
        | Reason::LockFundNotAuthorized { .. }
        | Reason::LockSendNotAuthorized { .. }
        | Reason::LockReleaseNotAuthorized { .. }
        | Reason::LockCancelNotAuthorized { .. }
        | Reason::LockTokenNotPermitted { .. }
        | Reason::LockRecipientNotPermitted { .. }
        | Reason::LockDurationTooLong { .. } => return Err(NetworkFailure::InvalidResponse),
    };
    Ok(value)
}

/// Format the Haskell contract-address representation.
fn contract(address: &concordium_rust_sdk::types::ContractAddress) -> String {
    format!("<{}, {}>", address.index, address.subindex)
}
