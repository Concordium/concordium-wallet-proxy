//! This module decodes persisted rewards and validator suspension outcomes.
//! It retains canonical reward-map lookup, legacy multiline text, and wrapping amounts.

use super::{
    events::{ccd, field},
    view::same_account,
};
use concordium_rust_sdk::{common::types::Amount, id::types::AccountAddress};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::BTreeMap;

/// This record is one element in a persisted account-to-amount map.
#[derive(Deserialize, Serialize)]
struct AccountAmount {
    /// This address is the exact canonical account key.
    address: AccountAddress,
    /// This amount is the account's reward.
    amount: Amount,
}

/// Convert one special outcome to its complete account-history fields.
pub(super) fn value(mut reward: Value, account: AccountAddress) -> anyhow::Result<Value> {
    let tag = reward["tag"]
        .as_str()
        .ok_or_else(|| anyhow::anyhow!("Missing reward tag"))?
        .to_owned();
    let amount = |v: &mut Value, name| field::<Amount>(v, name);
    let address = |v: &mut Value, name| field::<AccountAddress>(v, name);
    let zero = Amount::zero();
    let (kind, short, text, total) = match tag.as_str() {
        "BakingRewards" | "FinalizationRewards" => {
            let baking = tag == "BakingRewards";
            let entries = field::<Vec<AccountAmount>>(
                &mut reward,
                if baking {
                    "bakerRewards"
                } else {
                    "finalizationRewards"
                },
            )?;
            amount(&mut reward, "remainder")?;
            let amounts: BTreeMap<_, _> = entries
                .into_iter()
                .map(|entry| (entry.address, entry.amount))
                .collect();
            let short = if baking {
                "Block rewards"
            } else {
                "Finalization rewards"
            };
            let mut text = format!("{short}\n{}", if baking { "" } else { " " });
            for (account, amount) in &amounts {
                text.push_str(&format!("  - account {account} awarded {}\n", ccd(*amount)));
            }
            (
                if baking {
                    "bakingReward"
                } else {
                    "finalizationReward"
                },
                short,
                text,
                amounts.get(&account).copied(),
            )
        }
        "Mint" => {
            let baking = amount(&mut reward, "mintBakingReward")?;
            let finalization = amount(&mut reward, "mintFinalizationReward")?;
            let development = amount(&mut reward, "mintPlatformDevelopmentCharge")?;
            let foundation = address(&mut reward, "foundationAccount")?;
            (
                "platformDevelopmentCharge",
                "New CCD minted",
                format!(
                    "New CCD minted\n   - {} to the validator reward account\n  - {} to the finalization reward account\n  - {} as the platform development charge to account {foundation}\n",
                    ccd(baking),
                    ccd(finalization),
                    ccd(development)
                ),
                Some(development),
            )
        }
        "BlockReward" => {
            let fees = amount(&mut reward, "transactionFees")?;
            let old = amount(&mut reward, "oldGASAccount")?;
            let new = amount(&mut reward, "newGASAccount")?;
            let baking = amount(&mut reward, "bakerReward")?;
            let charge = amount(&mut reward, "foundationCharge")?;
            let baker = address(&mut reward, "baker")?;
            let foundation = address(&mut reward, "foundationAccount")?;
            let total = match (
                same_account(account, baker),
                same_account(account, foundation),
            ) {
                (true, true) => add(baking, charge),
                (true, false) => baking,
                _ => charge,
            };
            (
                "blockReward",
                "Block rewards",
                format!(
                    "Block rewards\n  - {} transaction fees in the block\n  - {} was the old balance of the GAS account\n  - {} is the new balance of the GAS account\n  - {} awarded to the block validator to address {baker}\n  - {} awarded to the foundation account {foundation}\n",
                    ccd(fees),
                    ccd(old),
                    ccd(new),
                    ccd(baking),
                    ccd(charge)
                ),
                Some(total),
            )
        }
        "PaydayFoundationReward" => {
            let foundation = address(&mut reward, "foundationAccount")?;
            let charge = amount(&mut reward, "developmentCharge")?;
            (
                "paydayFoundationReward",
                "Foundation development charge payout",
                format!(
                    "Foundation development charge payout: {} to {foundation}",
                    ccd(charge)
                ),
                Some(charge),
            )
        }
        "PaydayAccountReward" | "PaydayPoolReward" => {
            let fees = amount(&mut reward, "transactionFees")?;
            let baking = amount(&mut reward, "bakerReward")?;
            let finalization = amount(&mut reward, "finalizationReward")?;
            let total = add(add(fees, baking), finalization);
            let lines = format!(
                "  - {} from transaction fees\n  - {} from block rewards\n  - {} from finalization rewards\n",
                ccd(fees),
                ccd(baking),
                ccd(finalization)
            );
            if tag == "PaydayAccountReward" {
                let receiver = address(&mut reward, "account")?;
                (
                    "paydayAccountReward",
                    "Reward payout",
                    format!("Reward payout of {} to {receiver}: \n{lines}", ccd(total)),
                    Some(total),
                )
            } else {
                if reward.get("poolOwner").is_none() {
                    reward["poolOwner"] = Value::Null;
                }
                let pool = field::<Option<u64>>(&mut reward, "poolOwner")?;
                let name = pool.map_or_else(
                    || "passive delegators".into(),
                    |id| format!("pool with ID {id}"),
                );
                (
                    "paydayPoolReward",
                    "Pool rewards",
                    format!("Payday rewards to {name}:\n{lines}"),
                    Some(zero),
                )
            }
        }
        "BlockAccrueReward" => {
            let fees = amount(&mut reward, "transactionFees")?;
            let old = amount(&mut reward, "oldGASAccount")?;
            let new = amount(&mut reward, "newGASAccount")?;
            let baking = amount(&mut reward, "bakerReward")?;
            let passive = amount(&mut reward, "passiveReward")?;
            let charge = amount(&mut reward, "foundationCharge")?;
            let baker = field::<u64>(&mut reward, "bakerId")?;
            (
                "blockAccrueReward",
                "Accrual of block rewards",
                format!(
                    "Block rewards\n  - {} transaction fees in the block\n  - {} was the old balance of the GAS account\n  - {} is the new balance of the GAS account\n  - {} accrued to pool with ID {baker}\n  - {} accrued by passive delegation\n  - {} accrued to the foundation\n",
                    ccd(fees),
                    ccd(old),
                    ccd(new),
                    ccd(baking),
                    ccd(passive),
                    ccd(charge)
                ),
                Some(zero),
            )
        }
        "ValidatorPrimedForSuspension" | "ValidatorSuspended" => {
            let baker = field::<u64>(&mut reward, "bakerId")?;
            let account = address(&mut reward, "account")?;
            if tag == "ValidatorSuspended" {
                (
                    "validatorSuspended",
                    "Validator suspended",
                    format!("Validator {account} (ID {baker}) was suspended due to inactivity."),
                    Some(zero),
                )
            } else {
                (
                    "validatorPrimedForSuspension",
                    "Validator primed for suspension",
                    format!(
                        "Validator {account} (ID {baker}) was primed for suspension at the next pay day."
                    ),
                    Some(zero),
                )
            }
        }
        _ => anyhow::bail!("Unsupported legacy reward: {tag}"),
    };
    Ok(
        json!({"origin":{"type":if tag.starts_with("Validator") { "none" } else { "reward" }},"total":total,"details":{"type":kind,"outcome":"success","description":short,"events":[text]}}),
    )
}

/// Add amounts with the Word64 behavior of the legacy service.
fn add(left: Amount, right: Amount) -> Amount {
    Amount::from_micro_ccd(left.micro_ccd.wrapping_add(right.micro_ccd))
}
