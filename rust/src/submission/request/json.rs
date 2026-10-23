//! This module retains first duplicate keys and integral scientific JSON numbers.
//! It normalizes indexed maps before SDK deserialization without using floats.

use anyhow::Context;
use num::{BigInt, ToPrimitive, Zero};
use serde::{
    Deserialize, Deserializer,
    de::{MapAccess, Visitor},
};
use serde_json::{Value, value::RawValue};
use std::{collections::BTreeMap, fmt};

/// This object retains the first raw JSON value for each key.
struct Object(
    /// These entries are sorted by the original text key.
    BTreeMap<String, Box<RawValue>>,
);
impl<'de> Deserialize<'de> for Object {
    fn deserialize<D: Deserializer<'de>>(decoder: D) -> Result<Self, D::Error> {
        /// This visitor collects object entries without replacing duplicate keys.
        struct ObjectVisitor;
        impl<'de> Visitor<'de> for ObjectVisitor {
            type Value = Object;
            fn expecting(&self, output: &mut fmt::Formatter<'_>) -> fmt::Result {
                output.write_str("JSON object")
            }
            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Object, M::Error> {
                let mut entries = BTreeMap::new();
                while let Some((key, value)) = map.next_entry()? {
                    entries.entry(key).or_insert(value);
                }
                Ok(Object(entries))
            }
        }
        decoder.deserialize_map(ObjectVisitor)
    }
}

/// Read one JSON value without requiring a JSON content type or consuming trailing input.
pub(super) fn read(body: &[u8]) -> anyhow::Result<Value> {
    let value = Box::<RawValue>::deserialize(&mut serde_json::Deserializer::from_slice(body))?;
    normalize(&value, 0)
}

/// Normalize a value while keeping nested duplicate keys and exact integers.
fn normalize(raw: &RawValue, depth: usize) -> anyhow::Result<Value> {
    anyhow::ensure!(
        depth < 128,
        "Submission JSON exceeds the supported nesting depth"
    );
    match raw.get().as_bytes()[0] {
        b'{' => {
            let Object(entries) = serde_json::from_str(raw.get())?;
            let mut values = serde_json::Map::new();
            for (key, value) in entries {
                values.insert(key, normalize(&value, depth + 1)?);
            }
            Ok(Value::Object(values))
        }
        b'[' => {
            let entries: Vec<Box<RawValue>> = serde_json::from_str(raw.get())?;
            entries
                .iter()
                .map(|value| normalize(value, depth + 1))
                .collect::<anyhow::Result<Vec<_>>>()
                .map(Value::Array)
        }
        _ => {
            if let Some(integer) = integer(raw.get()) {
                if let Some(number) = integer.to_u64() {
                    return Ok(number.into());
                }
                if let Some(number) = integer.to_i64() {
                    return Ok(number.into());
                }
            }
            Ok(serde_json::from_str(raw.get())?)
        }
    }
}

/// Normalize numeric JSON map keys to their bounded unsigned decimal form.
pub(super) fn indices(value: &mut Value, maximum: u64) -> anyhow::Result<()> {
    let original = std::mem::take(value.as_object_mut().context("Invalid indexed map")?);
    let mut normalized = serde_json::Map::new();
    for (key, value) in original {
        let index = integer(&key)
            .and_then(|integer| integer.to_u64())
            .filter(|index| *index <= maximum)
            .context("Invalid map index")?;
        normalized.entry(index.to_string()).or_insert(value);
    }
    *value = Value::Object(normalized);
    Ok(())
}

/// Read a bounded-size decimal coefficient and scientific exponent exactly.
fn integer(text: &str) -> Option<BigInt> {
    let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((text, "0"));
    let unsigned = mantissa.strip_prefix(['+', '-']).unwrap_or(mantissa);
    let (whole, fraction) = unsigned.split_once('.').unwrap_or((unsigned, ""));
    let exponent_digits = exponent.strip_prefix(['+', '-']).unwrap_or(exponent);
    if whole.is_empty()
        || !whole.bytes().all(|byte| byte.is_ascii_digit())
        || (unsigned.contains('.') && fraction.is_empty())
        || !fraction.bytes().all(|byte| byte.is_ascii_digit())
        || exponent_digits.is_empty()
        || !exponent_digits.bytes().all(|byte| byte.is_ascii_digit())
    {
        return None;
    }
    let fractional = fraction.len();
    let coefficient = BigInt::parse_bytes(mantissa.replace('.', "").as_bytes(), 10)?;
    if coefficient.is_zero() {
        return Some(coefficient);
    }
    let exponent = BigInt::parse_bytes(exponent.as_bytes(), 10)?;
    let modulus = BigInt::from(1u32) << 64usize;
    let exponent = ((exponent % &modulus + &modulus) % modulus).to_u64()? as i64;
    let exponent = exponent.wrapping_sub(fractional as i64);
    if exponent >= 0 {
        if exponent > 20 {
            return None;
        }
        Some(coefficient * BigInt::from(10).pow(exponent as u32))
    } else {
        if exponent.unsigned_abs() > text.len() as u64 {
            return None;
        }
        let divisor = BigInt::from(10).pow(exponent.unsigned_abs().try_into().ok()?);
        if (&coefficient % &divisor).is_zero() {
            Some(coefficient / divisor)
        } else {
            None
        }
    }
}
