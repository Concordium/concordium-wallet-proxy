//! This module validates legacy enqueued update payloads without an SDK precision limit.
//! Mint rates retain the Haskell Word32 mantissa and full Word8 exponent range.

use concordium_rust_sdk::types::UpdatePayload;
use serde_json::{Value, json};

/// Validate a persisted payload without using its SDK serialization as a response.
pub(super) fn validate(payload: &Value) -> anyhow::Result<()> {
    let mut checked = payload.clone();
    let rate_name = match payload["updateType"].as_str() {
        Some("mintDistribution") => Some("mintPerSlot"),
        Some("timeParametersCPV1") => Some("mintPerPayday"),
        _ => None,
    };
    if let Some(name) = rate_name {
        let rate = payload["update"]
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("Missing {name}"))?;
        mint_rate(rate)?;
        // The SDK uses rust_decimal, which cannot read every valid legacy rate.
        // Validate the remaining payload with a representable rate; no payload
        // contents or substituted rate are returned by account-history responses.
        checked["update"][name] = json!(0);
    }
    let _: UpdatePayload = serde_json::from_value(checked)?;
    Ok(())
}

/// Check the normalized Scientific coefficient and exponent used by Haskell.
fn mint_rate(value: &Value) -> anyhow::Result<()> {
    let number = value
        .as_number()
        .ok_or_else(|| anyhow::anyhow!("Mint rate is not a number"))?;
    let text = number.to_string();
    let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((&text, "0"));
    let mut exponent: i64 = exponent.parse()?;
    let negative = mantissa.starts_with('-');
    let mantissa = mantissa.strip_prefix('-').unwrap_or(mantissa);
    let decimals = mantissa
        .split_once('.')
        .map_or(0, |(_, fraction)| fraction.len());
    exponent = exponent
        .checked_sub(i64::try_from(decimals)?)
        .ok_or_else(|| anyhow::anyhow!("Mint exponent overflow"))?;
    let digits = mantissa.replace('.', "");
    let digits = digits.trim_start_matches('0');
    if digits.is_empty() {
        return Ok(());
    }
    let normalized = digits.trim_end_matches('0');
    exponent = exponent
        .checked_add(i64::try_from(digits.len() - normalized.len())?)
        .ok_or_else(|| anyhow::anyhow!("Mint exponent overflow"))?;
    anyhow::ensure!(
        !negative && (-255..=0).contains(&exponent),
        "Invalid legacy mint exponent or sign"
    );
    let _: u32 = normalized.parse()?;
    Ok(())
}
