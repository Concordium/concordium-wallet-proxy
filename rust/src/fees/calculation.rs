//! This module calculates legacy energy and rounded micro-CCD fees.
//! Arithmetic retains the Haskell Word32 payload and Word64 energy wrap points.

use super::{FeeError, first, numbers};
use concordium_rust_sdk::{
    base::transactions::cost,
    types::{ProtocolVersion, chain_parameters::EnergyRate},
};
use num::{BigUint, ToPrimitive};

/// Calculate energy from the first occurrence of each query parameter.
#[allow(deprecated)] // The legacy endpoint still estimates encrypted transactions.
pub(super) fn energy(
    query: &[(String, String)],
    protocol: ProtocolVersion,
) -> Result<u64, FeeError> {
    let signatures = numbers::int(first(query, "numSignatures").unwrap_or("1"))
        .filter(|n| *n > 0)
        .ok_or_else(|| parse("numSignatures"))? as u64;
    let kind =
        first(query, "type").ok_or_else(|| FeeError::Parse("Missing `type` value.".into()))?;
    let has = |name| first(query, name).is_some();
    let mut sponsored = has("sponsored");
    let (payload, specific) = match kind {
        "simpleTransfer" | "encryptedTransfer" => {
            let memo = match first(query, "memoSize") {
                None => 0,
                Some(text) => {
                    let size = numbers::word32(text).ok_or_else(|| parse("memoSize"))?;
                    if protocol == ProtocolVersion::P1 {
                        return Err(FeeError::MemoUnsupported);
                    }
                    size.wrapping_add(2)
                }
            };
            if kind == "simpleTransfer" {
                (41u32.wrapping_add(memo), cost::SIMPLE_TRANSFER.energy)
            } else {
                (2617u32.wrapping_add(memo), cost::ENCRYPTED_TRANSFER.energy)
            }
        }
        "transferToSecret" => {
            sponsored = false;
            (9, cost::TRANSFER_TO_ENCRYPTED.energy)
        }
        "transferToPublic" => (1405, cost::TRANSFER_TO_PUBLIC.energy),
        "registerDelegation" => (
            if has("passive") { 13 } else { 21 },
            cost::CONFIGURE_DELEGATION.energy,
        ),
        "updateDelegation" => (
            3 + 8 * u32::from(has("amount"))
                + u32::from(has("restake"))
                + if has("target") {
                    if has("passive") { 1 } else { 9 }
                } else {
                    0
                },
            cost::CONFIGURE_DELEGATION.energy,
        ),
        "removeDelegation" => (11, cost::CONFIGURE_DELEGATION.energy),
        "registerBaker" => (
            validator_size(query, kind, Some(metadata(query)?.unwrap_or(2048))),
            cost::CONFIGURE_BAKER_WITH_KEYS.energy,
        ),
        "updateBakerStake" => (
            validator_size(query, kind, None),
            cost::CONFIGURE_BAKER_WITHOUT_KEYS.energy,
        ),
        "updateBakerPool" => (
            validator_size(query, kind, metadata(query)?),
            cost::CONFIGURE_BAKER_WITHOUT_KEYS.energy,
        ),
        "updateBakerKeys" => {
            sponsored = false;
            (355, cost::CONFIGURE_BAKER_WITH_KEYS.energy)
        }
        "removeBaker" => {
            sponsored = false;
            (11, cost::CONFIGURE_BAKER_WITHOUT_KEYS.energy)
        }
        "configureBaker" => {
            sponsored = false;
            (
                validator_size(query, kind, metadata(query)?),
                if has("keys") {
                    cost::CONFIGURE_BAKER_WITH_KEYS.energy
                } else {
                    cost::CONFIGURE_BAKER_WITHOUT_KEYS.energy
                },
            )
        }
        other => {
            return Err(FeeError::Parse(format!(
                "Could not parse transaction type: {other}"
            )));
        }
    };
    // SDK base_cost takes u32 signatures; Haskell accepts a positive 64-bit Int.
    Ok(signatures
        .wrapping_mul(100)
        .wrapping_add(60 + u64::from(payload) + if sponsored { 34 } else { 0 })
        .wrapping_add(specific))
}

/// Read the signed metadata length and convert it to a Word32 payload component.
fn metadata(query: &[(String, String)]) -> Result<Option<u32>, FeeError> {
    first(query, "metadataSize")
        .map(|text| {
            numbers::int(text)
                .map(|n| n as u32)
                .ok_or_else(|| parse("metadataSize"))
        })
        .transpose()
}

/// Calculate the configure-validator payload for one legacy alias.
fn validator_size(query: &[(String, String)], kind: &str, metadata: Option<u32>) -> u32 {
    let has = |name| first(query, name).is_some();
    let all = kind == "registerBaker";
    let configure = kind == "configureBaker";
    let stake = configure || kind == "updateBakerStake";
    let pool = configure || kind == "updateBakerPool";
    let flag = |name, selected| u32::from(all || (selected && has(name)));
    3u32.wrapping_add(8 * flag("amount", stake))
        .wrapping_add(flag("restake", stake))
        .wrapping_add(flag("openStatus", pool))
        .wrapping_add(352 * flag("keys", configure))
        .wrapping_add(metadata.map_or(0, |n| n.wrapping_add(2)))
        .wrapping_add(4 * flag("transactionCommission", pool))
        .wrapping_add(4 * flag("bakerRewardCommission", pool))
        .wrapping_add(4 * flag("finalizationRewardCommission", pool))
        .wrapping_add(u32::from(configure && has("suspended")))
}

/// Round the exact rational price upward and retain Amount's Word64 conversion.
pub(super) fn cost(rate: &EnergyRate, energy: u64) -> u64 {
    let numerator = BigUint::from(*rate.micro_ccd_per_energy.numer()) * energy;
    let denominator = BigUint::from(*rate.micro_ccd_per_energy.denom());
    let rounded = (numerator + &denominator - 1u32) / denominator;
    (rounded % (BigUint::from(1u32) << 64usize))
        .to_u64()
        .expect("Word64 remainder")
}

/// Format a malformed numeric parameter error.
fn parse(name: &str) -> FeeError {
    FeeError::Parse(format!("Could not parse `{name}` value."))
}
