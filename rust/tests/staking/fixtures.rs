//! This module builds synthetic chain-parameter protobuf responses.
//! It supplies all legacy required fields, including update keys not exposed by HTTP.

use concordium_rust_sdk::v2::generated as p;

fn fraction(parts: u32) -> Option<p::AmountFraction> {
    Some(p::AmountFraction {
        parts_per_hundred_thousand: parts,
    })
}
fn rate(numerator: u64, denominator: u64) -> Option<p::ExchangeRate> {
    Some(p::ExchangeRate {
        value: Some(p::Ratio {
            numerator,
            denominator,
        }),
    })
}
fn access() -> Option<p::AccessStructure> {
    Some(p::AccessStructure {
        access_public_keys: vec![p::UpdateKeysIndex { value: 0 }],
        access_threshold: Some(p::UpdateKeysThreshold { value: 1 }),
    })
}
fn keys() -> Vec<p::UpdatePublicKey> {
    vec![p::UpdatePublicKey { value: vec![1; 32] }]
}
fn higher_keys() -> Option<p::HigherLevelKeys> {
    Some(p::HigherLevelKeys {
        keys: keys(),
        threshold: Some(p::UpdateKeysThreshold { value: 1 }),
    })
}
fn auth() -> p::AuthorizationsV0 {
    p::AuthorizationsV0 {
        keys: keys(),
        emergency: access(),
        protocol: access(),
        parameter_consensus: access(),
        parameter_euro_per_energy: access(),
        parameter_micro_ccd_per_euro: access(),
        parameter_foundation_account: access(),
        parameter_mint_distribution: access(),
        parameter_transaction_fee_distribution: access(),
        parameter_gas_rewards: access(),
        pool_parameters: access(),
        add_anonymity_revoker: access(),
        add_identity_provider: access(),
    }
}
fn auth_v1() -> Option<p::AuthorizationsV1> {
    Some(p::AuthorizationsV1 {
        v0: Some(auth()),
        parameter_cooldown: access(),
        parameter_time: access(),
        ..Default::default()
    })
}
fn fees() -> Option<p::TransactionFeeDistribution> {
    Some(p::TransactionFeeDistribution {
        baker: fraction(40_000),
        gas_account: fraction(50_000),
    })
}
fn gas() -> Option<p::GasRewards> {
    Some(p::GasRewards {
        baker: fraction(10_000),
        finalization_proof: fraction(20_000),
        account_creation: fraction(30_000),
        chain_update: fraction(40_000),
    })
}
fn gas_v2() -> Option<p::GasRewardsCpv2> {
    Some(p::GasRewardsCpv2 {
        baker: fraction(10_000),
        account_creation: fraction(30_000),
        chain_update: fraction(40_000),
    })
}
fn mint() -> Option<p::MintDistributionCpv1> {
    Some(p::MintDistributionCpv1 {
        baking_reward: fraction(20_000),
        finalization_reward: fraction(30_000),
    })
}
fn cooldown() -> Option<p::CooldownParametersCpv1> {
    Some(p::CooldownParametersCpv1 {
        pool_owner_cooldown: Some(p::DurationSeconds { value: 3600 }),
        delegator_cooldown: Some(p::DurationSeconds { value: 7200 }),
    })
}
fn time() -> Option<p::TimeParametersCpv1> {
    Some(p::TimeParametersCpv1 {
        reward_period_length: Some(p::RewardPeriodLength {
            value: Some(p::Epoch { value: 2 }),
        }),
        mint_per_payday: Some(p::MintRate {
            mantissa: 25,
            exponent: 4,
        }),
    })
}
fn pools() -> Option<p::PoolParametersCpv1> {
    let range = Some(p::InclusiveRangeAmountFraction {
        min: fraction(0),
        max: fraction(100_000),
    });
    Some(p::PoolParametersCpv1 {
        passive_finalization_commission: fraction(10_000),
        passive_baking_commission: fraction(20_000),
        passive_transaction_commission: fraction(30_000),
        commission_bounds: Some(p::CommissionRanges {
            finalization: range,
            baking: range,
            transaction: range,
        }),
        minimum_equity_capital: Some(p::Amount { value: u64::MAX }),
        capital_bound: Some(p::CapitalBound {
            value: fraction(25_000),
        }),
        leverage_bound: Some(p::LeverageFactor {
            value: Some(p::Ratio {
                numerator: 3,
                denominator: 1,
            }),
        }),
    })
}
fn consensus() -> Option<p::ConsensusParametersV1> {
    Some(p::ConsensusParametersV1 {
        timeout_parameters: Some(p::TimeoutParameters {
            timeout_base: Some(p::Duration { value: 1000 }),
            timeout_increase: Some(p::Ratio {
                numerator: 3,
                denominator: 2,
            }),
            timeout_decrease: Some(p::Ratio {
                numerator: 3,
                denominator: 4,
            }),
        }),
        min_block_time: Some(p::Duration { value: 500 }),
        block_energy_limit: Some(p::Energy {
            value: 9_007_199_254_740_995,
        }),
    })
}
fn committee() -> Option<p::FinalizationCommitteeParameters> {
    Some(p::FinalizationCommitteeParameters {
        minimum_finalizers: 2,
        maximum_finalizers: 10,
        finalizer_relative_stake_threshold: fraction(25_000),
    })
}
fn foundation() -> Option<p::AccountAddress> {
    Some(p::AccountAddress { value: vec![1; 32] })
}

/// Build a pending pool reduction or removal at a controlled time.
pub fn pending(remove: bool, time: u64) -> p::PoolPendingChange {
    let effective_time = Some(p::Timestamp { value: time });
    p::PoolPendingChange {
        change: Some(if remove {
            p::pool_pending_change::Change::Remove(p::pool_pending_change::Remove {
                effective_time,
            })
        } else {
            p::pool_pending_change::Change::Reduce(p::pool_pending_change::Reduce {
                reduced_equity_capital: Some(p::Amount { value: 1000 }),
                effective_time,
            })
        }),
    }
}

/// Build a complete response for chain-parameter versions zero through three.
pub fn chain(version: u8) -> p::ChainParameters {
    use p::chain_parameters::Parameters;
    let election = Some(p::ElectionDifficulty {
        value: fraction(25_000),
    });
    let limit = Some(p::CredentialsPerBlockLimit { value: 5 });
    let parameters = match version {
        0 => Parameters::V0(p::ChainParametersV0 {
            election_difficulty: election,
            euro_per_energy: rate(1, 10),
            micro_ccd_per_euro: rate(500, 1),
            baker_cooldown_epochs: Some(p::Epoch { value: 8 }),
            account_creation_limit: limit,
            mint_distribution: Some(p::MintDistributionCpv0 {
                mint_per_slot: Some(p::MintRate {
                    mantissa: 1,
                    exponent: 4,
                }),
                baking_reward: fraction(20_000),
                finalization_reward: fraction(30_000),
            }),
            transaction_fee_distribution: fees(),
            gas_rewards: gas(),
            foundation_account: foundation(),
            minimum_threshold_for_baking: Some(p::Amount { value: u64::MAX }),
            root_keys: higher_keys(),
            level1_keys: higher_keys(),
            level2_keys: Some(auth()),
        }),
        1 => Parameters::V1(p::ChainParametersV1 {
            election_difficulty: election,
            euro_per_energy: rate(1, 10),
            micro_ccd_per_euro: rate(500, 1),
            cooldown_parameters: cooldown(),
            time_parameters: time(),
            account_creation_limit: limit,
            mint_distribution: mint(),
            transaction_fee_distribution: fees(),
            gas_rewards: gas(),
            foundation_account: foundation(),
            pool_parameters: pools(),
            root_keys: higher_keys(),
            level1_keys: higher_keys(),
            level2_keys: auth_v1(),
        }),
        2 => Parameters::V2(p::ChainParametersV2 {
            consensus_parameters: consensus(),
            euro_per_energy: rate(1, 10),
            micro_ccd_per_euro: rate(500, 1),
            cooldown_parameters: cooldown(),
            time_parameters: time(),
            account_creation_limit: limit,
            mint_distribution: mint(),
            transaction_fee_distribution: fees(),
            gas_rewards: gas_v2(),
            foundation_account: foundation(),
            pool_parameters: pools(),
            root_keys: higher_keys(),
            level1_keys: higher_keys(),
            level2_keys: auth_v1(),
            finalization_committee_parameters: committee(),
        }),
        3 => Parameters::V3(p::ChainParametersV3 {
            consensus_parameters: consensus(),
            euro_per_energy: rate(1, 10),
            micro_ccd_per_euro: rate(500, 1),
            cooldown_parameters: cooldown(),
            time_parameters: time(),
            account_creation_limit: limit,
            mint_distribution: mint(),
            transaction_fee_distribution: fees(),
            gas_rewards: gas_v2(),
            foundation_account: foundation(),
            pool_parameters: pools(),
            root_keys: higher_keys(),
            level1_keys: higher_keys(),
            level2_keys: auth_v1(),
            finalization_committee_parameters: committee(),
            validator_score_parameters: Some(p::ValidatorScoreParameters {
                maximum_missed_rounds: u64::MAX,
            }),
            max_lock_duration: Some(p::Duration { value: 123_456 }),
        }),
        _ => panic!("Unsupported fixture version"),
    };
    p::ChainParameters {
        parameters: Some(parameters),
    }
}
