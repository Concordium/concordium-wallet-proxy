//! This module selects the Haskell chain-parameter response schema.
//! It keeps legacy field names, version-dependent omissions, and exact mint rates.

use crate::{compatibility::fraction, network::NetworkFailure};
use concordium_rust_sdk::types::{MintRate, chain_parameters::ChainParameters};
use serde_json::{Value, json};

fn required<T: Copy>(value: Option<T>) -> Result<T, NetworkFailure> {
    value.ok_or(NetworkFailure::InvalidResponse)
}

fn mint(rate: MintRate) -> serde_json::Number {
    let digits = format!(
        "{:0>width$}",
        rate.mantissa,
        width = usize::from(rate.exponent) + 1
    );
    let split = digits.len() - usize::from(rate.exponent);
    let text = if rate.exponent == 0 {
        digits
    } else {
        format!("{}.{}", &digits[..split], &digits[split..])
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    };
    text.parse()
        .expect("Integer mantissa and exponent produce a JSON decimal")
}

pub(super) fn value(p: &ChainParameters, foundation_index: u64) -> Result<Value, NetworkFailure> {
    let old = p.cooldown_parameters.baker_cooldown_epochs.is_some();
    let bft = p.min_block_time.is_some();
    let euro = required(p.euro_per_energy)?;
    let ccd = required(p.micro_ccd_per_euro)?;
    let mut mint_distribution = json!({"bakingReward": fraction(required(p.mint_distribution.baking_reward)?), "finalizationReward": fraction(required(p.mint_distribution.finalization_reward)?)});
    let mut gas = json!({"baker": fraction(required(p.gas_rewards.baker)?), "accountCreation": fraction(required(p.gas_rewards.account_creation)?), "chainUpdate": fraction(required(p.gas_rewards.chain_update)?)});
    if old {
        mint_distribution["mintPerSlot"] = json!(mint(required(p.mint_per_slot)?));
    }
    if !bft {
        gas["finalizationProof"] = json!(fraction(required(p.gas_rewards.finalization_proof)?));
    }
    let mut value = json!({
        "euroPerEnergy": {"numerator": euro.numerator(), "denominator": euro.denominator()},
        "microGTUPerEuro": {"numerator": ccd.numerator(), "denominator": ccd.denominator()},
        "accountCreationLimit": required(p.account_creation_limit)?,
        "foundationAccountIndex": foundation_index,
        "rewardParameters": {
            "mintDistribution": mint_distribution,
            "transactionFeeDistribution": {"baker": fraction(required(p.transaction_fee_distribution.baker)?), "gasAccount": fraction(required(p.transaction_fee_distribution.gas_account)?)},
            "gASRewards": gas
        }
    });
    if !bft {
        value["electionDifficulty"] = json!(required(p.election_difficulty)?);
    }
    if old {
        value["bakerCooldownEpochs"] =
            json!(required(p.cooldown_parameters.baker_cooldown_epochs)?);
        value["minimumThresholdForBaking"] = json!(
            required(p.staking_parameters.minimum_equity_capital)?
                .micro_ccd
                .to_string()
        );
    } else {
        delegation(&mut value, p)?;
    }
    if bft {
        let timeout = &p.timeout_parameters;
        let increase = required(timeout.increase)?;
        let decrease = required(timeout.decrease)?;
        let committee = &p.finalization_committee_parameters;
        value["timeoutBase"] = json!(required(timeout.base)?.millis());
        value["timeoutIncrease"] =
            json!({"numerator": increase.numerator(), "denominator": increase.denominator()});
        value["timeoutDecrease"] =
            json!({"numerator": decrease.numerator(), "denominator": decrease.denominator()});
        value["minBlockTime"] = json!(required(p.min_block_time)?.millis());
        value["blockEnergyLimit"] = json!(required(p.block_energy_limit)?);
        value["minimumFinalizers"] = json!(required(committee.min_finalizers)?);
        value["maximumFinalizers"] = json!(required(committee.max_finalizers)?);
        let threshold: u32 = required(committee.finalizers_relative_stake_threshold)?.into();
        value["finalizerRelativeStakeThreshold"] = json!(fraction(
            concordium_rust_sdk::types::AmountFraction::new_unchecked(threshold)
        ));
    }
    if let Some(rounds) = p.validator_max_missed_rounds {
        value["maximumMissedRounds"] = json!(rounds);
    }
    Ok(value)
}

fn delegation(value: &mut Value, p: &ChainParameters) -> Result<(), NetworkFailure> {
    let stake = &p.staking_parameters;
    value["poolOwnerCooldown"] =
        json!(required(p.cooldown_parameters.pool_owner_cooldown)?.seconds);
    value["delegatorCooldown"] = json!(required(p.cooldown_parameters.delegator_cooldown)?.seconds);
    value["passiveFinalizationCommission"] =
        json!(fraction(required(stake.passive_finalization_commission)?));
    value["passiveBakingCommission"] = json!(fraction(required(stake.passive_baking_commission)?));
    value["passiveTransactionCommission"] =
        json!(fraction(required(stake.passive_transaction_commission)?));
    for (name, range) in [
        (
            "finalizationCommissionRange",
            stake.finalization_commission_range,
        ),
        ("bakingCommissionRange", stake.baking_commission_range),
        (
            "transactionCommissionRange",
            stake.transaction_commission_range,
        ),
    ] {
        let range = required(range)?;
        value[name] = json!({"min": fraction(range.min), "max": fraction(range.max)});
    }
    value["minimumEquityCapital"] = json!(
        required(stake.minimum_equity_capital)?
            .micro_ccd
            .to_string()
    );
    value["capitalBound"] = json!(fraction(required(stake.capital_bound)?.bound));
    let leverage = required(stake.leverage_bound)?;
    value["leverageBound"] =
        json!({"numerator": leverage.numerator, "denominator": leverage.denominator});
    value["rewardPeriodLength"] = json!(
        required(p.reward_period_length)?
            .reward_period_epochs()
            .epoch
    );
    value["mintPerPayday"] = json!(mint(required(p.mint_per_payday)?));
    Ok(())
}

#[cfg(test)]
mod tests {
    //! These tests check exact mint decimals beyond the SDK Serde decimal range.

    use super::*;

    #[test]
    fn mint_rates_preserve_zero_full_mantissa_and_maximum_exponent() {
        assert_eq!(
            mint(MintRate {
                mantissa: 0,
                exponent: 255
            })
            .to_string(),
            "0"
        );
        assert_eq!(
            mint(MintRate {
                mantissa: u32::MAX,
                exponent: 0
            })
            .to_string(),
            "4294967295"
        );
        assert_eq!(
            mint(MintRate {
                mantissa: 1,
                exponent: 255
            })
            .to_string(),
            format!("0.{}1", "0".repeat(254))
        );
        assert_eq!(
            mint(MintRate {
                mantissa: 2500,
                exponent: 6
            })
            .to_string(),
            "0.0025"
        );
    }

    #[test]
    fn missing_required_parameters_fail_instead_of_emitting_null_fields() {
        assert!(matches!(
            value(&ChainParameters::default(), 0),
            Err(NetworkFailure::InvalidResponse)
        ));
    }
}
