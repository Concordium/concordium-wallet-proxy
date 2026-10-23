//! This module validates contract fee inputs in legacy order.
//! It retains unrestricted parameter bytes and Word64 energy-buffer arithmetic.

use super::{FeeError, calculation, first, numbers};
use concordium_rust_sdk::{
    smart_contracts::common::{
        AccountAddress, Address, Amount, ContractAddress, OwnedParameter, OwnedReceiveName,
    },
    types::{Energy, smart_contracts::ContractContext},
};

/// This type supplies an invocation and its fee calculation settings.
pub(super) struct ContractEstimate {
    /// This context uses the sender, target, and energy limit from the legacy handler.
    pub context: ContractContext,
    /// This value is the transaction's minimum energy cost.
    pub minimum: u64,
    /// This signed percentage is converted to Word64 before multiplication.
    pub buffer: i64,
}

impl ContractEstimate {
    /// Add execution energy and its buffer with legacy wrap points.
    pub fn energy(&self, used: u64) -> u64 {
        self.minimum
            .wrapping_add(used.wrapping_add(used.wrapping_mul(self.buffer as u64) / 100))
    }
}

/// Validate invocation inputs before any contract query.
pub(super) fn prepare(
    query: &[(String, String)],
    signatures: u64,
    sponsored: bool,
) -> Result<ContractEstimate, FeeError> {
    let sender = sender(required(query, "sender")?)?;
    let index = word(query, "contractIndex")?;
    let subindex = word(query, "contractSubindex")?;
    let amount = word(query, "amount")?;
    let method = OwnedReceiveName::new(required(query, "receiveName")?.into())
        .map_err(|_| FeeError::Parse("Invalid receive name.".into()))?;
    let parameter = parameter(required(query, "parameter")?)?;
    let buffer = first(query, "executionNRGBuffer").map_or(Ok(20), |text| {
        numbers::int(text).ok_or_else(|| parse("executionNRGBuffer"))
    })?;
    let payload = (29usize
        .wrapping_add(method.as_receive_name().get_chain_name().len())
        .wrapping_add(parameter.len())) as u32;
    Ok(ContractEstimate {
        context: ContractContext {
            invoker: Some(Address::Account(sender)),
            contract: ContractAddress { index, subindex },
            amount: Amount::from_micro_ccd(amount),
            method,
            // Haskell wraps raw bytes without a transaction parameter-size check.
            // The SDK invoke request forwards these bytes without serialization.
            parameter: OwnedParameter::new_unchecked(parameter),
            energy: Some(Energy { energy: 500_000 }),
        },
        minimum: calculation::minimum(payload, signatures, sponsored),
        buffer,
    })
}

/// Read a required query parameter without treating an empty value as absent.
pub(super) fn required<'a>(query: &'a [(String, String)], name: &str) -> Result<&'a str, FeeError> {
    first(query, name).ok_or_else(|| FeeError::Parse(format!("Missing `{name}` value.")))
}

/// Read one required Word64 value.
fn word(query: &[(String, String)], name: &str) -> Result<u64, FeeError> {
    numbers::word64(required(query, name)?).ok_or_else(|| parse(name))
}

/// Format a numeric conversion failure.
fn parse(name: &str) -> FeeError {
    FeeError::Parse(format!("Could not parse `{name}` value."))
}

/// Decode the checksum before checking address length and version.
fn sender(text: &str) -> Result<AccountAddress, FeeError> {
    let bytes = bs58::decode(text)
        .with_check(None)
        .into_vec()
        .map_err(|_| sender_error("Base 58 checksum invalid."))?;
    if bytes.len() != 33 {
        return Err(sender_error("Wrong address length."));
    }
    if bytes[0] != 1 {
        return Err(sender_error("Unknown base58 check version byte."));
    }
    Ok(AccountAddress(
        bytes[1..].try_into().expect("32-byte account address"),
    ))
}

/// Format a legacy sender error with its checksum or shape detail.
fn sender_error(detail: &str) -> FeeError {
    FeeError::Parse(format!("Could not parse `sender` value: {detail}"))
}

/// Decode hexadecimal bytes with the legacy Base16 error order and offsets.
fn parameter(text: &str) -> Result<Vec<u8>, FeeError> {
    let bytes = text.as_bytes();
    let failure =
        |message| FeeError::Parse(format!("Could not parse `parameter` value: {message}"));
    if !bytes.len().is_multiple_of(2) {
        return Err(failure("invalid bytestring size".into()));
    }
    let mut result = Vec::with_capacity(bytes.len() / 2);
    for (index, pair) in bytes.as_chunks::<2>().0.iter().enumerate() {
        let mut byte = 0;
        for (offset, digit) in pair.iter().enumerate() {
            let digit = char::from(*digit).to_digit(16).ok_or_else(|| {
                failure(format!(
                    "invalid character at offset: {}",
                    2 * index + offset
                ))
            })?;
            byte = byte * 16 + digit as u8;
        }
        result.push(byte);
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legacy_parameter_wrapper_has_no_transaction_size_validation() {
        let query = [
            (
                "sender".into(),
                "2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3".into(),
            ),
            ("contractIndex".into(), "0".into()),
            ("contractSubindex".into(), "0".into()),
            ("amount".into(), "0".into()),
            ("receiveName".into(), ".".into()),
            ("parameter".into(), "ab".repeat(65_536)),
        ];
        let estimate =
            prepare(&query, 1, false).unwrap_or_else(|_| panic!("Legacy bytes must remain valid"));
        assert_eq!(estimate.context.parameter.as_ref(), vec![0xab; 65_536]);
        assert_eq!(estimate.minimum, 160 + 29 + 1 + 65_536);
    }
}
