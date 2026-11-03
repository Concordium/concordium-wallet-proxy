//! This module decodes persisted rejection reasons, including historical lock names.
//! It does not rename current node rejection variants or change submission-status behavior.

use concordium_rust_sdk::{
    base::protocol_level_locks::LockId,
    types::{LockAccountRejectReasonDetails, LockTokenRejectReasonDetails, RejectReason},
};
use serde::{Deserialize, Serialize};

/// This value retains current rejection schemas and the historical lock schema.
#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub(super) enum Rejection {
    /// These reasons use the historical lock operation names from persisted summaries.
    Lock(
        /// This value validates lock identifiers and associated accounts or tokens.
        LockRejection,
    ),
    /// These reasons share the existing English rejection converter.
    Standard(
        /// This value retains typed SDK-compatible legacy reasons.
        RejectReason,
    ),
}

/// These reasons match the Haskell revision referenced by the proxy's lock-support commit.
#[derive(Deserialize, Serialize)]
#[serde(tag = "tag")]
pub(super) enum LockRejection {
    /// The lock identifier does not exist.
    NonExistentLockId {
        /// This identifier is absent from the chain.
        contents: LockId,
    },
    /// The lock has expired.
    LockExpired {
        /// This identifier selects the expired lock.
        contents: LockId,
    },
    /// The account cannot fund this lock.
    LockFundNotAuthorized {
        /// These details identify the account and lock.
        contents: LockAccountRejectReasonDetails,
    },
    /// The account cannot send from this lock.
    LockSendNotAuthorized {
        /// These details identify the account and lock.
        contents: LockAccountRejectReasonDetails,
    },
    /// The account cannot return funds from this historical lock.
    LockReturnNotAuthorized {
        /// These details identify the account and lock.
        contents: LockAccountRejectReasonDetails,
    },
    /// The account cannot cancel this lock.
    LockCancelNotAuthorized {
        /// These details identify the account and lock.
        contents: LockAccountRejectReasonDetails,
    },
    /// This token is not permitted by the lock.
    LockTokenNotPermitted {
        /// These details identify the token and lock.
        contents: LockTokenRejectReasonDetails,
    },
    /// The recipient cannot receive funds from this lock.
    LockRecipientNotPermitted {
        /// These details identify the account and lock.
        contents: LockAccountRejectReasonDetails,
    },
}

impl Rejection {
    /// Render raw legacy JSON without the SDK's extra null token-details property.
    pub fn raw_value(&self) -> anyhow::Result<serde_json::Value> {
        let mut value = serde_json::to_value(self)?;
        if matches!(self, Self::Standard(RejectReason::TokenUpdateTransactionFailed { reject_reason }) if reject_reason.details.is_none())
        {
            value
                .get_mut("contents")
                .and_then(serde_json::Value::as_object_mut)
                .ok_or_else(|| anyhow::anyhow!("Invalid encoded token rejection"))?
                .remove("details");
        }
        Ok(value)
    }

    /// Render persisted lock reasons or delegate to the unchanged shared descriptions.
    pub fn description(&self) -> anyhow::Result<String> {
        Ok(match self {
            Self::Standard(reason) => crate::compatibility::rejection::description(reason)
                .map_err(|_| anyhow::anyhow!("Unsupported legacy rejection"))?,
            Self::Lock(reason) => match reason {
                LockRejection::NonExistentLockId { contents } => {
                    format!("Non existing lock id {contents}.")
                }
                LockRejection::LockExpired { contents } => format!("Lock {contents} has expired."),
                LockRejection::LockTokenNotPermitted { contents } => format!(
                    "Token {} is not permitted by lock {}.",
                    contents.token_id, contents.lock_id
                ),
                LockRejection::LockFundNotAuthorized { contents } => format!(
                    "Account {} is not authorized to fund lock {}.",
                    contents.account, contents.lock_id
                ),
                LockRejection::LockSendNotAuthorized { contents } => format!(
                    "Account {} is not authorized to send from lock {}.",
                    contents.account, contents.lock_id
                ),
                LockRejection::LockReturnNotAuthorized { contents } => format!(
                    "Account {} is not authorized to return from lock {}.",
                    contents.account, contents.lock_id
                ),
                LockRejection::LockCancelNotAuthorized { contents } => format!(
                    "Account {} is not authorized to cancel lock {}.",
                    contents.account, contents.lock_id
                ),
                LockRejection::LockRecipientNotPermitted { contents } => format!(
                    "Account {} is not a permitted recipient for lock {}.",
                    contents.account, contents.lock_id
                ),
            },
        })
    }
}
