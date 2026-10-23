//! This module supplies response conversions shared by wallet-facing features.
//! It preserves exact fractions, payday rounding, and UTC timestamps.

use crate::network::NetworkFailure;
use chrono::{DateTime, TimeDelta, Utc};
use concordium_rust_sdk::types::{AmountFraction, PartsPerHundredThousands};

/// Convert an amount fraction to an exact JSON decimal.
pub(crate) fn fraction(value: AmountFraction) -> serde_json::Number {
    let parts: PartsPerHundredThousands = value.into();
    let parts: u32 = parts.into();
    let text = format!("{}.{:05}", parts / 100_000, parts % 100_000);
    text.trim_end_matches('0')
        .trim_end_matches('.')
        .parse()
        .expect("Integer parts produce a valid JSON decimal")
}

/// Find the first payday at or after the effective time.
pub(crate) fn first_payday(
    next: DateTime<Utc>,
    effective: DateTime<Utc>,
    epoch: TimeDelta,
    epochs: u64,
) -> Result<DateTime<Utc>, NetworkFailure> {
    if effective <= next {
        return Ok(next);
    }
    let epoch =
        u64::try_from(epoch.num_milliseconds()).map_err(|_| NetworkFailure::InvalidResponse)?;
    // Haskell multiplies the epoch count and Duration as Word64 before rounding.
    let length = i128::from(epoch.wrapping_mul(epochs));
    if length <= 0 {
        return Err(NetworkFailure::InvalidResponse);
    }
    let delta = i128::from(effective.signed_duration_since(next).num_milliseconds());
    let intervals = (delta + length - 1) / length;
    let millis = i64::try_from(intervals * length).map_err(|_| NetworkFailure::InvalidResponse)?;
    next.checked_add_signed(
        TimeDelta::try_milliseconds(millis).ok_or(NetworkFailure::InvalidResponse)?,
    )
    .ok_or(NetworkFailure::InvalidResponse)
}

/// Format a UTC time without trailing fractional zeros, as Aeson does.
pub(crate) fn format_time(time: DateTime<Utc>) -> String {
    let mut value = time.to_rfc3339_opts(chrono::SecondsFormat::Nanos, true);
    while value.ends_with("0Z") {
        value.remove(value.len() - 2);
    }
    if value.ends_with(".Z") {
        value.remove(value.len() - 2);
    }
    value
}

#[cfg(test)]
mod tests {
    //! These tests check legacy payday arithmetic without wall-clock time.

    use super::*;

    #[test]
    fn payday_rounding_preserves_word64_period_multiplication() {
        let next = DateTime::from_timestamp_millis(0).unwrap();
        let effective = DateTime::from_timestamp_millis(1).unwrap();
        let epoch = TimeDelta::milliseconds((1_i64 << 62) + 1);
        assert_eq!(
            first_payday(next, effective, epoch, 4).unwrap(),
            DateTime::from_timestamp_millis(4).unwrap()
        );
        assert!(first_payday(next, effective, TimeDelta::zero(), 2).is_err());
        assert_eq!(
            first_payday(next, next, TimeDelta::zero(), 0).unwrap(),
            next
        );
    }
}
