//! This module names persisted summary types for historical wallet responses.
//! Memo omission changes the response type but not its original description.

use concordium_rust_sdk::{
    id::types::CredentialType,
    types::{TransactionType, UpdateType},
};
use serde::{Deserialize, Serialize};

/// This value retains a transaction type that was removed from the current SDK.
#[derive(Deserialize, Serialize)]
pub(super) enum LegacyType {
    /// This type identifies persisted meta updates, not current node submissions.
    #[serde(rename = "metaUpdate")]
    MetaUpdate,
}

/// This value decodes both current and historical account transaction names.
#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub(super) enum AccountKind {
    /// This type is available in the current SDK.
    Current(
        /// This value validates the standard transaction name.
        TransactionType,
    ),
    /// This type is retained only in legacy history.
    Legacy(
        /// This value validates the historical transaction name.
        LegacyType,
    ),
}

/// This value identifies the legacy transaction-summary category.
#[derive(Deserialize, Serialize)]
#[serde(tag = "type", content = "contents")]
pub(super) enum Kind {
    /// This category has an optional account transaction type.
    #[serde(rename = "accountTransaction")]
    Account(
        /// This type is absent for malformed payloads.
        Option<AccountKind>,
    ),
    /// This category deploys an initial or normal credential.
    #[serde(rename = "credentialDeploymentTransaction")]
    Credential(
        /// This value selects the credential description.
        CredentialType,
    ),
    /// This category enqueues a chain update.
    #[serde(rename = "updateTransaction")]
    Update(
        /// This value selects the update description.
        UpdateType,
    ),
}

impl Kind {
    /// Read the persisted transaction type name without changing its memo version.
    pub fn account_name(&self) -> Option<String> {
        match self {
            Self::Account(Some(kind)) => {
                serde_json::to_value(kind).ok()?.as_str().map(str::to_owned)
            }
            _ => None,
        }
    }

    /// Select the rendered type and English description.
    pub fn labels(&self, memos: bool) -> anyhow::Result<(String, &'static str)> {
        let (name, description) = match self {
            Self::Account(None) => (
                "Malformed account transaction".into(),
                "Malformed transaction body",
            ),
            Self::Account(Some(kind)) => {
                let name = serde_json::to_value(kind)?
                    .as_str()
                    .ok_or_else(|| anyhow::anyhow!("Invalid transaction type"))?
                    .to_owned();
                let description = match name.as_str() {
                    "deployModule" => "Deploy module",
                    "initContract" => "Initialize smart contract",
                    "update" => "Invoke smart contract",
                    "transfer" => "Transfer",
                    "transferWithMemo" => "Transfer with a memo",
                    "updateBakerStake" => "Update validator stake",
                    "updateBakerKeys" => "Update validator keys",
                    "updateBakerRestakeEarnings" => "Update whether to restake validator earnings",
                    "addBaker" => "Add validator",
                    "removeBaker" => "Remove validator",
                    "updateCredentialKeys" => "Update credential keys",
                    "encryptedAmountTransfer" => "Shielded transfer",
                    "encryptedAmountTransferWithMemo" => "Shielded transfer with a memo",
                    "transferToEncrypted" => "Shielded amount",
                    "transferToPublic" => "Unshielded amount",
                    "transferWithSchedule" => "Transfer with schedule",
                    "transferWithScheduleAndMemo" => "Transfer with schedule and a memo",
                    "updateCredentials" => "Update account credentials",
                    "registerData" => "Register data on the chain",
                    "configureBaker" => "Configure validator",
                    "configureDelegation" => "Configure delegation",
                    "tokenUpdate" => "Token update",
                    "metaUpdate" => "Meta update",
                    _ => anyhow::bail!("Unsupported history transaction type: {name}"),
                };
                let rendered = match (name.as_str(), memos) {
                    ("transferWithMemo", false) => "transfer",
                    ("encryptedAmountTransferWithMemo", false) => "encryptedAmountTransfer",
                    ("transferWithScheduleAndMemo", false) => "transferWithSchedule",
                    ("updateCredentialKeys", _) => "updateAccountKeys",
                    _ => &name,
                };
                (rendered.to_owned(), description)
            }
            Self::Credential(kind) => (
                "deployCredential".into(),
                match kind {
                    CredentialType::Initial => "Deploy initial account credential",
                    CredentialType::Normal => "Deploy account credential",
                },
            ),
            Self::Update(kind) => {
                let name = serde_json::to_value(kind)?;
                let description = match name.as_str() {
                    Some("updateProtocol") => "Protocol update",
                    Some("updateElectionDifficulty") => "Update election difficulty",
                    Some("updateEuroPerEnergy") => "Update Euro per Energy exchange rate",
                    Some("updateMicroGTUPerEuro") => "Update micro CCD per Euro exchange rate",
                    Some("updateFoundationAccount") => "Update the foundation account address",
                    Some("updateMintDistribution") => "Update parameters of the mint distribution",
                    Some("updateTransactionFeeDistribution") => {
                        "Update transaction fee distribution"
                    }
                    Some("updateGASRewards") => "Update parameters for GAS rewards distribution",
                    Some("updatePoolParameters") => "Update staking pool parameters",
                    Some("updateTimeParameters") => "Update time parameters",
                    Some("updateCooldownParameters") => "Update cooldown parameters",
                    Some("updateAddAnonymityRevoker") => "Add anonymity revoker",
                    Some("updateAddIdentityProvider") => "Add identity provider",
                    Some("updateRootKeys") => "Update root keys",
                    Some("updateLevel1Keys") => "Update level 1 keys",
                    Some("updateLevel2Keys") => "Update level 2 keys",
                    Some("updateTimeoutParameters") => "Update timeout parameters",
                    Some("updateMinBlockTime") => "Update minimum block time",
                    Some("updateBlockEnergyLimit") => "Update block energy limit",
                    Some("updateFinalizationCommitteeParameters") => {
                        "Update finalization committee parameters"
                    }
                    Some("updateValidatorScoreParameters") => "Update validator score parameters",
                    Some("updateCreatePLT") => "Update create new PLT token",
                    _ => anyhow::bail!("Unsupported history chain update: {name}"),
                };
                ("chainUpdate".into(), description)
            }
        };
        Ok((name, description))
    }
}
