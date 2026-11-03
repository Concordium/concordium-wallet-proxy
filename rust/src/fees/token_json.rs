//! This module reads token operation counts with Aeson's integer and duplicate-key rules.
//! It formats source-derived type errors without using floating-point numbers.

mod syntax;

use num::{BigInt, ToPrimitive, Zero};
use serde::{
    Deserialize, Deserializer,
    de::{MapAccess, Visitor},
};
use serde_json::value::RawValue;
use std::{collections::BTreeMap, fmt};

/// This type retains the first value of each duplicate JSON object key.
struct FirstObject(
    /// These values remain untyped until the whole object has been read.
    BTreeMap<String, Box<RawValue>>,
);
impl<'de> Deserialize<'de> for FirstObject {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        /// This visitor reads an object without replacing duplicate values.
        struct ObjectVisitor;
        impl<'de> Visitor<'de> for ObjectVisitor {
            type Value = FirstObject;
            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("operation-count object")
            }
            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Self::Value, M::Error> {
                let mut values = BTreeMap::new();
                while let Some((key, value)) = map.next_entry::<String, Box<RawValue>>()? {
                    values.entry(key).or_insert(value);
                }
                Ok(FirstObject(values))
            }
        }
        deserializer.deserialize_map(ObjectVisitor)
    }
}

/// Read a JSON object of bounded Int counts before checking trailing input.
pub(super) fn counts(raw: &str) -> Result<BTreeMap<String, i64>, String> {
    syntax::validate(raw)?;
    let mut decoder = serde_json::Deserializer::from_str(raw);
    let value = Box::<RawValue>::deserialize(&mut decoder).map_err(|_| syntax::error(raw))?;
    if kind(&value) != "Object" {
        return Err(format!(
            "Error in $: parsing Map ~Text failed, expected Object, but encountered {}",
            kind(&value)
        ));
    }
    let FirstObject(values) =
        FirstObject::deserialize(&mut serde_json::Deserializer::from_str(raw))
            .map_err(|_| syntax::error(raw))?;
    let mut result = BTreeMap::new();
    for (name, value) in values {
        let count = if kind(&value) == "Number" {
            integer(value.get()).ok_or_else(|| {
                format!(
                    "value is either floating or will cause over or underflow {}",
                    scientific(value.get())
                )
            })
        } else {
            Err(format!("expected Number, but encountered {}", kind(&value)))
        }
        .map_err(|detail| format!("Error in ${}: parsing Int failed, {detail}", path(&name)))?;
        result.insert(name, count);
    }
    decoder.end().map_err(|_| "Trailing garbage".to_string())?;
    Ok(result)
}

/// Read an exact scientific number when it fits a signed 64-bit integer.
fn integer(text: &str) -> Option<i64> {
    let (coefficient, exponent) = parts(text)?;
    if coefficient.is_zero() {
        return Some(0);
    }
    if exponent >= 0 {
        if exponent > 19 {
            return None;
        }
        (coefficient * BigInt::from(10).pow(exponent as u32)).to_i64()
    } else {
        if exponent.unsigned_abs() > text.len() as u64 {
            return None;
        }
        let divisor = BigInt::from(10).pow(exponent.unsigned_abs().try_into().ok()?);
        if (&coefficient % &divisor).is_zero() {
            (coefficient / divisor).to_i64()
        } else {
            None
        }
    }
}

/// Split a decimal scientific number into an integer coefficient and base-10 exponent.
fn parts(text: &str) -> Option<(BigInt, i64)> {
    let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((text, "0"));
    let fractional = mantissa
        .split_once('.')
        .map_or(0, |(_, digits)| digits.len());
    let coefficient = BigInt::parse_bytes(mantissa.replace('.', "").as_bytes(), 10)?;
    Some((
        coefficient,
        super::numbers::int(exponent.strip_prefix('+').unwrap_or(exponent))?
            .wrapping_sub(fractional as i64),
    ))
}

/// Format a Scientific value with the legacy generic decimal/exponent thresholds.
fn scientific(text: &str) -> String {
    let Some((coefficient, exponent)) = parts(text) else {
        return text.into();
    };
    if coefficient.is_zero() {
        return "0.0".into();
    }
    let coefficient = coefficient.to_string();
    let (sign, digits) = coefficient
        .strip_prefix('-')
        .map_or(("", coefficient.as_str()), |digits| ("-", digits));
    let position = i128::from(exponent) + digits.len() as i128;
    let digits = digits.trim_end_matches('0');
    if !(0..=7).contains(&position) {
        format!(
            "{sign}{}.{}e{}",
            &digits[..1],
            if digits.len() == 1 { "0" } else { &digits[1..] },
            position - 1
        )
    } else if position == 0 {
        format!("{sign}0.{digits}")
    } else {
        let mut padded = digits.to_string();
        while padded.len() < position as usize {
            padded.push('0');
        }
        let (whole, fraction) = padded.split_at(position as usize);
        format!(
            "{sign}{whole}.{}",
            if fraction.is_empty() { "0" } else { fraction }
        )
    }
}

/// Format a JSON type name as Aeson does.
fn kind(value: &RawValue) -> &'static str {
    match value.get().as_bytes()[0] {
        b'n' => "Null",
        b't' | b'f' => "Boolean",
        b'\"' => "String",
        b'[' => "Array",
        b'{' => "Object",
        _ => "Number",
    }
}

/// Format the key component in an Aeson error path.
fn path(key: &str) -> String {
    if key.chars().next().is_some_and(char::is_alphabetic) && key.chars().all(char::is_alphanumeric)
    {
        format!(".{key}")
    } else {
        format!("['{}']", key.replace('\\', "\\\\").replace('\'', "\\'"))
    }
}

pub(super) use crate::compatibility::show_text as show;
