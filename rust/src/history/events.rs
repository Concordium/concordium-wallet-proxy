//! This module decodes persisted events and renders their English descriptions.
//! SDK field types validate binary data; legacy event tags and wording remain explicit.

use concordium_rust_sdk::{
    common::types::Amount,
    id::types::AccountAddress,
    types::{
        Address, AmountFraction, BakerAddedEvent, BakerKeysEvent, ContractAddress,
        ContractInitializedEvent, CredentialRegistrationID, DelegationTarget,
        EncryptedAmountRemovedEvent, EncryptedSelfAmountAddedEvent, Memo, NewEncryptedAmountEvent,
        OpenStatus, PartsPerHundredThousands, RegisteredData,
        smart_contracts::{ContractEvent, InstanceUpdatedEvent, ModuleReference},
    },
};
use serde::{Serialize, de::DeserializeOwned};
use serde_json::{Value, json};

/// This event retains normalized legacy fields and the localized description.
pub(super) struct Event {
    /// These fields have passed the legacy event decoder.
    pub value: Value,
    /// This text is the English event description.
    pub description: String,
}

/// Read and normalize a required field with its chain type.
pub(super) fn field<T: DeserializeOwned + Serialize>(
    value: &mut Value,
    name: &str,
) -> anyhow::Result<T> {
    let field = value
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("Missing {name}"))?
        .clone();
    let decoded: T = serde_json::from_value(field)?;
    value[name] = serde_json::to_value(&decoded)?;
    Ok(decoded)
}

/// Read a normalized account address.
pub(super) fn account(value: &Value, name: &str) -> anyhow::Result<AccountAddress> {
    Ok(serde_json::from_value(value[name].clone())?)
}

/// Format micro-CCD with the six fractional digits used by the legacy service.
pub(super) fn ccd(amount: Amount) -> String {
    format!(
        "{}.{:06} CCD",
        amount.micro_ccd / 1_000_000,
        amount.micro_ccd % 1_000_000
    )
}

/// Format an account or contract without its tagged JSON representation.
fn address(value: Address) -> String {
    match value {
        Address::Account(account) => account.to_string(),
        Address::Contract(contract) => instance(contract),
    }
}

/// Format a contract address with the legacy punctuation.
fn instance(value: ContractAddress) -> String {
    format!("<{}, {}>", value.index, value.subindex)
}

impl Event {
    /// Validate every required event field before producing wallet-facing text.
    pub fn decode(mut value: Value) -> anyhow::Result<Self> {
        let tag = value["tag"]
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("Missing event tag"))?
            .to_owned();
        let description = match tag.as_str() {
            "ModuleDeployed" => format!(
                "Deployed module {}",
                field::<ModuleReference>(&mut value, "contents")?
            ),
            "ContractInitialized" => {
                let data: ContractInitializedEvent = serde_json::from_value(value.clone())?;
                // Supplemented legacy summaries require the initialization parameter.
                anyhow::ensure!(
                    data.parameter.is_some(),
                    "Missing supplemented initialization parameter"
                );
                value = serde_json::to_value(&data)?;
                format!(
                    "Initialized smart contract {} at address {} with balance {}. {} events were logged",
                    data.init_name.as_contract_name().contract_name(),
                    instance(data.address),
                    ccd(data.amount),
                    data.events.len()
                )
            }
            "Updated" => {
                let data: InstanceUpdatedEvent = serde_json::from_value(value.clone())?;
                value = serde_json::to_value(&data)?;
                let name = data.receive_name.to_string();
                let (contract, function) = name
                    .split_once('.')
                    .ok_or_else(|| anyhow::anyhow!("Invalid receive name"))?;
                format!(
                    "Invoked smart contract: source={}, target={}, contract= {}, function= {}, amount={}, message={}",
                    address(data.instigator),
                    instance(data.address),
                    contract,
                    function,
                    ccd(data.amount),
                    data.message
                )
            }
            "Transferred" => {
                let from = field::<Address>(&mut value, "from")?;
                let amount = field::<Amount>(&mut value, "amount")?;
                let to = field::<Address>(&mut value, "to")?;
                format!(
                    "Transferred {} from {} to {}",
                    ccd(amount),
                    address(from),
                    address(to)
                )
            }
            "AccountCreated" => format!(
                "Created account with address {}",
                field::<AccountAddress>(&mut value, "contents")?
            ),
            "CredentialDeployed" => {
                field::<CredentialRegistrationID>(&mut value, "regId")?;
                format!(
                    "Deployed a credential to account {}",
                    field::<AccountAddress>(&mut value, "account")?
                )
            }
            "CredentialKeysUpdated" => format!(
                "Updated keys of credential with ID {}",
                field::<CredentialRegistrationID>(&mut value, "credId")?
            ),
            "BakerAdded" => {
                let data: BakerAddedEvent = serde_json::from_value(value.clone())?;
                value = serde_json::to_value(&data)?;
                format!(
                    "Added validator {} (ID {}) and initial stake {}",
                    data.keys_event.account,
                    data.keys_event.baker_id.id.index,
                    ccd(data.stake)
                )
            }
            "BakerKeysUpdated" => {
                let data: BakerKeysEvent = serde_json::from_value(value.clone())?;
                value = serde_json::to_value(&data)?;
                format!(
                    "Validator {} (ID {}) keys updated.",
                    data.account, data.baker_id.id.index
                )
            }
            "BakerRemoved"
            | "BakerStakeIncreased"
            | "BakerStakeDecreased"
            | "BakerSetRestakeEarnings"
            | "BakerSetOpenStatus"
            | "BakerSetMetadataURL"
            | "BakerSetTransactionFeeCommission"
            | "BakerSetBakingRewardCommission"
            | "BakerSetFinalizationRewardCommission"
            | "BakerSuspended"
            | "BakerResumed"
            | "DelegationStakeIncreased"
            | "DelegationStakeDecreased"
            | "DelegationSetRestakeEarnings"
            | "DelegationSetDelegationTarget"
            | "DelegationAdded"
            | "DelegationRemoved" => staking(&tag, &mut value)?,
            "NewEncryptedAmount" => {
                let data: NewEncryptedAmountEvent = serde_json::from_value(value.clone())?;
                value = serde_json::to_value(&data)?;
                format!("New encrypted amount added to account {}", data.receiver)
            }
            "EncryptedAmountsRemoved" => {
                let data: EncryptedAmountRemovedEvent = serde_json::from_value(value.clone())?;
                value = serde_json::to_value(&data)?;
                format!("Consumed encrypted amounts on account {}", data.account)
            }
            "EncryptedSelfAmountAdded" => {
                let data: EncryptedSelfAmountAddedEvent = serde_json::from_value(value.clone())?;
                value = serde_json::to_value(&data)?;
                format!("Updated shielded balance of account {}", data.account)
            }
            "AmountAddedByDecryption" => {
                let amount = field::<Amount>(&mut value, "amount")?;
                format!(
                    "Unshielded {} on account {}",
                    ccd(amount),
                    field::<AccountAddress>(&mut value, "account")?
                )
            }
            "TransferredWithSchedule" => {
                let from = field::<AccountAddress>(&mut value, "from")?;
                let to = field::<AccountAddress>(&mut value, "to")?;
                field::<Vec<(u64, Amount)>>(&mut value, "amount")?;
                format!("Transferred with schedule from {from} to {to}")
            }
            "CredentialsUpdated" => {
                let account = field::<AccountAddress>(&mut value, "account")?;
                field::<Vec<CredentialRegistrationID>>(&mut value, "newCredIds")?;
                field::<Vec<CredentialRegistrationID>>(&mut value, "removedCredIds")?;
                field::<concordium_rust_sdk::base::contracts_common::AccountThreshold>(
                    &mut value,
                    "newThreshold",
                )?;
                format!("Credentials on account {account} updated.")
            }
            "DataRegistered" => {
                field::<RegisteredData>(&mut value, "data")?;
                "Data registered on the chain.".into()
            }
            "TransferMemo" => {
                field::<Memo>(&mut value, "memo")?;
                format!(
                    "Memo '{}' included in a transfer.",
                    value["memo"]
                        .as_str()
                        .ok_or_else(|| anyhow::anyhow!("Invalid memo"))?
                )
            }
            "Interrupted" => {
                let contract = field::<ContractAddress>(&mut value, "address")?;
                field::<Vec<ContractEvent>>(&mut value, "events")?;
                format!(
                    "Execution of {} triggered an operation.",
                    instance(contract)
                )
            }
            "Resumed" => {
                let contract = field::<ContractAddress>(&mut value, "address")?;
                let success = field::<bool>(&mut value, "success")?;
                format!(
                    "Operation {} and execution of {} resumed.",
                    if success { "succeeded" } else { "failed" },
                    instance(contract)
                )
            }
            "Upgraded" => {
                let contract = field::<ContractAddress>(&mut value, "address")?;
                let from = field::<ModuleReference>(&mut value, "from")?;
                let to = field::<ModuleReference>(&mut value, "to")?;
                format!(
                    "Smart contract instance at {} was upgraded from {from} to {to}.",
                    instance(contract)
                )
            }
            "UpdateEnqueued" => {
                field::<u64>(&mut value, "effectiveTime")?;
                super::payload::validate(
                    value
                        .get("payload")
                        .ok_or_else(|| anyhow::anyhow!("Missing update payload"))?,
                )?;
                "Chain update event enqueued.".into()
            }
            "TokenModuleEvent" | "TokenTransfer" | "TokenMint" | "TokenBurn" | "TokenCreated"
            | "LockCreated" | "LockDestroyed" => super::tokens::description(&tag, &mut value)?,
            _ => anyhow::bail!("Unsupported legacy history event: {tag}"),
        };
        value["tag"] = json!(tag);
        Ok(Self { value, description })
    }

    /// Read the validated event tag.
    pub fn tag(&self) -> &str {
        self.value["tag"].as_str().expect("Validated event tag")
    }
}

/// Render validator and delegation events with the legacy terminology and precedence.
fn staking(tag: &str, value: &mut Value) -> anyhow::Result<String> {
    let delegation = tag.starts_with("Delegation");
    let id = field::<u64>(value, if delegation { "delegatorId" } else { "bakerId" })?;
    let account = field::<AccountAddress>(value, "account")?;
    let who = format!("{account} (ID {id})");
    let role = if delegation { "delegator" } else { "validator" };
    Ok(match tag {
        "BakerRemoved" | "DelegationRemoved" => format!("Removed {role} {who}"),
        "DelegationAdded" => format!("Added delegator {who}"),
        "BakerStakeIncreased"
        | "BakerStakeDecreased"
        | "DelegationStakeIncreased"
        | "DelegationStakeDecreased" => format!(
            "Stake of {role} {who} {} to {}",
            if tag.ends_with("Increased") {
                "increased"
            } else {
                "decreased"
            },
            ccd(field::<Amount>(value, "newStake")?)
        ),
        "BakerSetRestakeEarnings" | "DelegationSetRestakeEarnings" => format!(
            "{} {who}{}",
            if delegation { "Delegator" } else { "Validator" },
            if field::<bool>(value, "restakeEarnings")? {
                " set to restake earnings."
            } else {
                " unset restaking of earnings."
            }
        ),
        "BakerSetOpenStatus" => {
            let status = field::<OpenStatus>(value, "openStatus")?;
            let text = match status {
                OpenStatus::OpenForAll => "OpenForAll",
                OpenStatus::ClosedForNew => "ClosedForNew",
                OpenStatus::ClosedForAll => "ClosedForAll",
            };
            format!("Open status of validator {who} set to {text}")
        }
        "BakerSetMetadataURL" => format!(
            "Metadata URL of validator {who} set to {}",
            field::<String>(value, "metadataURL")?
        ),
        "BakerSetTransactionFeeCommission"
        | "BakerSetBakingRewardCommission"
        | "BakerSetFinalizationRewardCommission" => {
            let (field_name, label) = match tag {
                "BakerSetTransactionFeeCommission" => {
                    ("transactionFeeCommission", "Transaction fee")
                }
                "BakerSetBakingRewardCommission" => ("bakingRewardCommission", "Block reward"),
                _ => ("finalizationRewardCommission", "Finalization reward"),
            };
            let fraction = field::<AmountFraction>(value, field_name)?;
            let parts: PartsPerHundredThousands = fraction.into();
            let parts: u32 = parts.into();
            let text = if parts > 0 && parts < 10_000 {
                let digits = parts.to_string();
                let exponent = 6 - digits.len();
                let tail = digits[1..].trim_end_matches('0');
                format!(
                    "{}.{}e-{exponent}",
                    &digits[..1],
                    if tail.is_empty() { "0" } else { tail }
                )
            } else {
                let text = format!("{}.{:05}", parts / 100_000, parts % 100_000);
                let trimmed = text.trim_end_matches('0');
                if trimmed.ends_with('.') {
                    format!("{trimmed}0")
                } else {
                    trimmed.to_owned()
                }
            };
            format!("{label} commission of validator {who} set to {text}")
        }
        "DelegationSetDelegationTarget" => {
            let target = field::<DelegationTarget>(value, "delegationTarget")?;
            let target = match target {
                DelegationTarget::Passive => "passive delegation".into(),
                DelegationTarget::Baker { baker_id } => {
                    format!("staking pool {}", baker_id.id.index)
                }
            };
            format!("Delegator {who} set delegation target to {target}")
        }
        "BakerSuspended" => format!("Validator {who} was suspended."),
        "BakerResumed" => format!("Validator {who} was resumed."),
        _ => anyhow::bail!("Unsupported staking event"),
    })
}
