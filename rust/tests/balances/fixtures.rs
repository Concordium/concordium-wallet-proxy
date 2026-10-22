//! This module builds controlled SDK protobuf inputs for CCD balance tests.
//! Its constants also define the source-derived HTTP expectations.

use concordium_rust_sdk::{common::to_bytes, types::BakerKeyPairs, v2::generated as p};
use rand::{SeedableRng, rngs::StdRng};

/// This value identifies the account in every controlled request.
pub const ADDRESS: [u8; 32] = [1; 32];
/// This timestamp starts the controlled reward period.
pub const NEXT: u64 = 1_767_225_600_000;

/// Encode the point at infinity using the chain's compressed curve format.
pub fn point() -> Vec<u8> {
    let mut value = vec![0; 48];
    value[0] = 0xc0;
    value
}

/// Build a zero encrypted amount with four compressed identity points.
pub fn encrypted() -> p::EncryptedAmount {
    p::EncryptedAmount {
        value: point().repeat(4),
    }
}

/// Build an account with precise large amounts, releases, and all cooldown phases.
pub fn account() -> p::AccountInfo {
    p::AccountInfo {
        sequence_number: Some(p::SequenceNumber {
            value: 9_007_199_254_740_995,
        }),
        amount: Some(p::Amount {
            value: 9_007_199_254_741_993,
        }),
        schedule: Some(p::ReleaseSchedule {
            total: Some(p::Amount { value: 4000 }),
            schedules: vec![
                p::Release {
                    timestamp: Some(p::Timestamp { value: NEXT + 100 }),
                    amount: Some(p::Amount { value: 1000 }),
                    transactions: vec![
                        p::TransactionHash { value: vec![2; 32] },
                        p::TransactionHash { value: vec![3; 32] },
                    ],
                },
                p::Release {
                    timestamp: Some(p::Timestamp { value: NEXT + 200 }),
                    amount: Some(p::Amount { value: 3000 }),
                    transactions: vec![p::TransactionHash { value: vec![4; 32] }],
                },
            ],
        }),
        threshold: Some(p::AccountThreshold { value: 1 }),
        encrypted_balance: Some(p::EncryptedBalance {
            self_amount: Some(encrypted()),
            start_index: 9_007_199_254_740_993,
            ..Default::default()
        }),
        encryption_key: Some(p::EncryptionKey {
            value: point().repeat(2),
        }),
        index: Some(p::AccountIndex { value: 42 }),
        address: Some(p::AccountAddress {
            value: ADDRESS.to_vec(),
        }),
        cooldowns: (0..3)
            .map(|status| p::Cooldown {
                end_time: Some(p::Timestamp {
                    value: NEXT + status as u64,
                }),
                amount: Some(p::Amount {
                    value: 100 * (status as u64 + 1),
                }),
                status,
            })
            .collect(),
        available_balance: Some(p::Amount {
            value: 9_007_199_254_737_993,
        }),
        ..Default::default()
    }
}

/// Build passive or pool-targeted delegation state.
pub fn delegation(
    target: Option<u64>,
    pending: Option<p::StakePendingChange>,
) -> p::AccountStakingInfo {
    p::AccountStakingInfo {
        staking_info: Some(p::account_staking_info::StakingInfo::Delegator(
            p::account_staking_info::Delegator {
                staked_amount: Some(p::Amount { value: 2000 }),
                restake_earnings: true,
                target: Some(p::DelegationTarget {
                    target: Some(match target {
                        Some(value) => p::delegation_target::Target::Baker(p::BakerId { value }),
                        None => p::delegation_target::Target::Passive(p::Empty {}),
                    }),
                }),
                pending_change: pending,
            },
        )),
    }
}

/// Generate stable validator keys without a public node or production secrets.
pub fn keys() -> BakerKeyPairs {
    BakerKeyPairs::generate(&mut StdRng::seed_from_u64(42))
}

/// Build validator state with deterministic public keys.
pub fn validator(pending: Option<p::StakePendingChange>) -> p::AccountStakingInfo {
    let keys = keys();
    p::AccountStakingInfo {
        staking_info: Some(p::account_staking_info::StakingInfo::Baker(
            p::account_staking_info::Baker {
                staked_amount: Some(p::Amount { value: 2000 }),
                restake_earnings: false,
                baker_info: Some(p::BakerInfo {
                    baker_id: Some(p::BakerId { value: 42 }),
                    election_key: Some(p::BakerElectionVerifyKey {
                        value: to_bytes(&keys.election_verify),
                    }),
                    signature_key: Some(p::BakerSignatureVerifyKey {
                        value: to_bytes(&keys.signature_verify),
                    }),
                    aggregation_key: Some(p::BakerAggregationVerifyKey {
                        value: to_bytes(&keys.aggregation_verify),
                    }),
                }),
                pool_info: Some(pool_metadata()),
                pending_change: pending,
                is_suspended: true,
            },
        )),
    }
}

/// Build the three commission rates and pool metadata.
pub fn pool_metadata() -> p::BakerPoolInfo {
    p::BakerPoolInfo {
        open_status: 0,
        url: "https://pool.example".into(),
        commission_rates: Some(p::CommissionRates {
            finalization: Some(p::AmountFraction {
                parts_per_hundred_thousand: 10_000,
            }),
            baking: Some(p::AmountFraction {
                parts_per_hundred_thousand: 20_000,
            }),
            transaction: Some(p::AmountFraction {
                parts_per_hundred_thousand: 30_000,
            }),
        }),
    }
}

/// Build pool status with independent optional suspension flags.
pub fn pool(suspended: Option<bool>, primed: Option<bool>) -> p::PoolInfoResponse {
    p::PoolInfoResponse {
        baker: Some(p::BakerId { value: 42 }),
        address: Some(p::AccountAddress {
            value: ADDRESS.to_vec(),
        }),
        equity_capital: Some(p::Amount { value: 2000 }),
        delegated_capital: Some(p::Amount { value: 100 }),
        delegated_capital_cap: Some(p::Amount { value: 500 }),
        pool_info: Some(pool_metadata()),
        all_pool_total_capital: Some(p::Amount { value: 10000 }),
        current_payday_info: Some(p::PoolCurrentPaydayInfo {
            blocks_baked: 1,
            finalization_live: true,
            transaction_fees_earned: Some(p::Amount { value: 0 }),
            effective_stake: Some(p::Amount { value: 2100 }),
            lottery_power: 0.5,
            baker_equity_capital: Some(p::Amount { value: 2000 }),
            delegated_capital: Some(p::Amount { value: 100 }),
            commission_rates: pool_metadata().commission_rates,
            is_primed_for_suspension: primed,
            ..Default::default()
        }),
        is_suspended: suspended,
        ..Default::default()
    }
}

/// Build a pending stake reduction or removal at a controlled time.
pub fn pending(remove: bool, time: u64) -> p::StakePendingChange {
    p::StakePendingChange {
        change: Some(if remove {
            p::stake_pending_change::Change::Remove(p::Timestamp { value: time })
        } else {
            p::stake_pending_change::Change::Reduce(p::stake_pending_change::Reduce {
                new_stake: Some(p::Amount { value: 1000 }),
                effective_time: Some(p::Timestamp { value: time }),
            })
        }),
    }
}

/// Build tokenomics using either the legacy or payday-bearing representation.
pub fn tokenomics(payday: bool, protocol: i32) -> p::TokenomicsInfo {
    let amount = Some(p::Amount { value: 1_000_000 });
    p::TokenomicsInfo {
        tokenomics: Some(if payday {
            p::tokenomics_info::Tokenomics::V1(p::tokenomics_info::V1 {
                total_amount: amount,
                total_encrypted_amount: amount,
                baking_reward_account: amount,
                finalization_reward_account: amount,
                gas_account: amount,
                foundation_transaction_rewards: amount,
                next_payday_time: Some(p::Timestamp { value: NEXT }),
                next_payday_mint_rate: Some(p::MintRate {
                    mantissa: 1,
                    exponent: 4,
                }),
                total_staked_capital: amount,
                protocol_version: protocol,
            })
        } else {
            p::tokenomics_info::Tokenomics::V0(p::tokenomics_info::V0 {
                total_amount: amount,
                total_encrypted_amount: amount,
                baking_reward_account: amount,
                finalization_reward_account: amount,
                gas_account: amount,
                protocol_version: protocol,
            })
        }),
    }
}
