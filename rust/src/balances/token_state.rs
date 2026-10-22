//! This module projects token-module CBOR through the SDK's decoder API.
//! It retains known legacy fields and hex fallback, but intentionally drops extensions.

use concordium_rust_sdk::{
    common::cbor::{
        self, CborDecoder, CborDeserialize, CborMapDecoder, CborSerializationError,
        CborSerializationResult, CborTagDecoder, DataItemHeader, value::Value as CborValue,
    },
    id::types::AccountAddress,
};
use serde_json::{Map, Value, json};
use std::collections::{BTreeMap, BTreeSet};

/// Render known global properties, or the original hex when decoding fails.
pub(super) fn module(bytes: &[u8]) -> Value {
    cbor::cbor_decode::<Module>(bytes)
        .map(|value| value.0)
        .unwrap_or_else(|_| json!(hex(bytes)))
}

/// Render known account properties, or the original hex when decoding fails.
pub(super) fn account(bytes: &[u8]) -> Value {
    cbor::cbor_decode::<Account>(bytes)
        .map(|value| value.0)
        .unwrap_or_else(|_| json!(hex(bytes)))
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}
fn invalid(message: &str) -> CborSerializationError {
    CborSerializationError::invalid_data(message)
}

/// This type selects the known legacy token-state properties.
#[derive(Clone, Copy)]
enum Scope {
    /// This scope includes global token properties.
    Global,
    /// This scope includes account allow-list and deny-list properties.
    Account,
}

/// This type contains the projected global token state.
struct Module(
    /// This value contains known JSON properties.
    Value,
);
impl CborDeserialize for Module {
    fn deserialize<C: CborDecoder>(decoder: C) -> CborSerializationResult<Self> {
        state(decoder, Scope::Global).map(Self)
    }
}

/// This type contains the projected account token state.
struct Account(
    /// This value contains known JSON properties.
    Value,
);
impl CborDeserialize for Account {
    fn deserialize<C: CborDecoder>(decoder: C) -> CborSerializationResult<Self> {
        state(decoder, Scope::Account).map(Self)
    }
}

/// This type enforces the legacy outer map's definite text keys.
struct Key(
    /// This string contains the key.
    String,
);
impl CborDeserialize for Key {
    fn deserialize<C: CborDecoder>(mut decoder: C) -> CborSerializationResult<Self> {
        if !matches!(
            decoder.peek_data_item_header()?,
            DataItemHeader::Text(Some(_))
        ) {
            return Err(invalid("Expected a definite text key"));
        }
        String::deserialize(decoder).map(Self)
    }
}

fn state<C: CborDecoder>(decoder: C, scope: Scope) -> CborSerializationResult<Value> {
    let mut map = decoder.decode_map()?;
    let mut seen = BTreeSet::new();
    let mut json = Map::new();
    while let Some(Key(key)) = map.deserialize_key()? {
        if !seen.insert(key.clone()) {
            return Err(invalid("Duplicate token-state key"));
        }
        let value = match (scope, key.as_str()) {
            (_, "allowList" | "denyList") | (Scope::Global, "paused" | "mintable" | "burnable") => {
                Some(json!(map.deserialize_value::<bool>()?))
            }
            (Scope::Global, "name") => Some(json!(map.deserialize_value::<String>()?)),
            (Scope::Global, "metadata") => Some(map.deserialize_value::<Metadata>()?.0),
            (Scope::Global, "governanceAccount") => Some(map.deserialize_value::<Holder>()?.0),
            _ => {
                // Approved: omit legacy `_additional` instead of retaining extensions.
                map.skip_value()?;
                None
            }
        };
        if let Some(value) = value {
            json.insert(key, value);
        }
    }
    Ok(Value::Object(json))
}

/// This type retains the shape needed to validate a known nested property.
struct Field {
    /// This value is decoded by the SDK rather than a private term reader.
    value: CborValue,
    /// This flag identifies indefinite bytes, which legacy checksum/address fields reject.
    indefinite_bytes: bool,
}
impl CborDeserialize for Field {
    fn deserialize<C: CborDecoder>(mut decoder: C) -> CborSerializationResult<Self> {
        let indefinite_bytes = matches!(
            decoder.peek_data_item_header()?,
            DataItemHeader::Bytes(None)
        );
        Ok(Self {
            value: CborValue::deserialize(decoder)?,
            indefinite_bytes,
        })
    }
}
impl Field {
    fn bytes(self) -> CborSerializationResult<[u8; 32]> {
        match self.value {
            CborValue::Bytes(bytes) if !self.indefinite_bytes => {
                bytes.0.try_into().map_err(|_| invalid("Expected 32 bytes"))
            }
            _ => Err(invalid("Expected a definite byte string")),
        }
    }
}

/// This type contains metadata URL properties without additional fields.
struct Metadata(
    /// This value contains the URL and optional checksum.
    Value,
);
impl CborDeserialize for Metadata {
    fn deserialize<C: CborDecoder>(decoder: C) -> CborSerializationResult<Self> {
        let mut map = decoder.decode_map()?;
        let mut fields = BTreeMap::new();
        while let Some(key) = map.deserialize_key::<String>()? {
            if key == "url" || key == "checksumSha256" {
                fields.insert(key, map.deserialize_value::<Field>()?);
            } else {
                map.skip_value()?;
            }
        }
        // Haskell's nested metadata map resolves duplicate keys before validation.
        let CborValue::Text(url) = fields
            .remove("url")
            .ok_or_else(|| invalid("Missing metadata URL"))?
            .value
        else {
            return Err(invalid("Expected a metadata URL string"));
        };
        let mut json = json!({"url":url});
        if let Some(checksum) = fields.remove("checksumSha256") {
            json["checksumSha256"] = json!(hex(&checksum.bytes()?));
        }
        Ok(Self(json))
    }
}

/// This type contains the legacy governance-account properties.
struct Holder(
    /// This value contains the address and optional CCD coin information.
    Value,
);
impl CborDeserialize for Holder {
    fn deserialize<C: CborDecoder>(decoder: C) -> CborSerializationResult<Self> {
        let fields = decoder
            .decode_tagged_expect(40307)?
            .deserialize::<HolderFields>()?;
        let address = fields
            .0
            .get(&3)
            .ok_or_else(|| invalid("Missing governance address"))?;
        let mut json = json!({"type":"account", "address":AccountAddress(match &address.value {
            CborValue::Bytes(bytes) if !address.indefinite_bytes => bytes.0.as_slice().try_into().map_err(|_| invalid("Expected a 32-byte account address"))?,
            _ => return Err(invalid("Expected a definite account-address byte string")),
        })});
        if let Some(coin) = fields.0.get(&1) {
            let CborValue::Tag(40305, coin) = &coin.value else {
                return Err(invalid("Expected CCD coin information"));
            };
            let CborValue::Map(entries) = coin.as_ref() else {
                return Err(invalid("Expected a coin-info map"));
            };
            let mut coin_type = None;
            for (key, value) in entries {
                if !matches!(key, CborValue::Positive(1)) {
                    return Err(invalid("Unknown coin-info key"));
                }
                coin_type = Some(value);
            }
            if !matches!(coin_type, Some(CborValue::Positive(919))) {
                return Err(invalid("Expected CCD coin type"));
            }
            json["coinInfo"] = json!("CCD");
        }
        Ok(Self(json))
    }
}

/// This type retains last-wins values for the two allowed governance map keys.
struct HolderFields(
    /// These fields contain optional coin information and the address.
    BTreeMap<u64, Field>,
);
impl CborDeserialize for HolderFields {
    fn deserialize<C: CborDecoder>(decoder: C) -> CborSerializationResult<Self> {
        let mut map = decoder.decode_map()?;
        let mut fields = BTreeMap::new();
        while let Some(key) = map.deserialize_key::<u64>()? {
            if key != 1 && key != 3 {
                return Err(invalid("Unknown governance-account key"));
            }
            fields.insert(key, map.deserialize_value()?);
        }
        Ok(Self(fields))
    }
}
