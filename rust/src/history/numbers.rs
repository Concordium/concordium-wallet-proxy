//! This module normalizes integral legacy JSON numbers before typed decoding.
//! Aeson accepts integral scientific notation where Serde integer visitors require integer tokens.

use serde_json::{Number, Value};

/// Normalize exact integral numbers that fit the legacy signed or unsigned 64-bit fields.
pub(super) fn normalize(value: &mut Value) {
    match value {
        Value::Array(values) => values.iter_mut().for_each(normalize),
        Value::Object(values) => values.values_mut().for_each(normalize),
        Value::Number(number) => {
            if let Some(integer) = integer(number) {
                *number = integer;
            }
        }
        _ => {}
    }
}

/// Convert a decimal number without rounding fractional digits or allocating large powers.
fn integer(number: &Number) -> Option<Number> {
    let text = number.to_string();
    let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((&text, "0"));
    let exponent: i64 = exponent.parse().ok()?;
    let decimals = mantissa
        .split_once('.')
        .map_or(0, |(_, digits)| digits.len());
    let shift = exponent.checked_sub(i64::try_from(decimals).ok()?)?;
    let negative = mantissa.starts_with('-');
    let digits = mantissa
        .strip_prefix('-')
        .unwrap_or(mantissa)
        .replace('.', "");
    let mut digits = digits.trim_start_matches('0').to_owned();
    if digits.is_empty() {
        return Some(0.into());
    }
    if shift < 0 {
        let removed = usize::try_from(shift.checked_neg()?).ok()?;
        let split = digits.len().checked_sub(removed)?;
        if !digits[split..].bytes().all(|digit| digit == b'0') {
            return None;
        }
        digits.truncate(split);
    } else {
        let added = usize::try_from(shift).ok()?;
        if digits.len().checked_add(added)? > 20 {
            return None;
        }
        digits.extend(std::iter::repeat_n('0', added));
    }
    if negative {
        format!("-{digits}").parse::<i64>().ok().map(Into::into)
    } else {
        digits.parse::<u64>().ok().map(Into::into)
    }
}
