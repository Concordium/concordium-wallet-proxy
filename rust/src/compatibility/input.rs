//! This module reads legacy account addresses and hexadecimal input.
//! It retains checksum-before-shape validation and Base16 byte offsets.

use concordium_rust_sdk::id::types::AccountAddress;

/// Decode an account checksum before checking length and version.
pub(crate) fn account(text: &str) -> Result<AccountAddress, &'static str> {
    let bytes = bs58::decode(text)
        .with_check(None)
        .into_vec()
        .map_err(|_| "Base 58 checksum invalid.")?;
    if bytes.len() != 33 {
        return Err("Wrong address length.");
    }
    if bytes[0] != 1 {
        return Err("Unknown base58 check version byte.");
    }
    Ok(AccountAddress(
        bytes[1..].try_into().expect("32-byte account address"),
    ))
}

/// Decode Base16 without a token or transaction length restriction.
pub(crate) fn hex(text: &str) -> Result<Vec<u8>, String> {
    let bytes = text.as_bytes();
    if !bytes.len().is_multiple_of(2) {
        return Err("invalid bytestring size".into());
    }
    bytes
        .as_chunks::<2>()
        .0
        .iter()
        .enumerate()
        .map(|(index, pair)| {
            let mut value = 0;
            for (offset, digit) in pair.iter().enumerate() {
                let digit = char::from(*digit).to_digit(16).ok_or_else(|| {
                    format!("invalid character at offset: {}", 2 * index + offset)
                })?;
                value = value * 16 + digit as u8;
            }
            Ok(value)
        })
        .collect()
}

/// Encode raw bytes without a collection-length prefix.
pub(crate) fn hex_encode(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// Read a bounded unsigned path number, including the legacy negative-zero form.
pub(crate) fn path_word(text: &str) -> Option<u64> {
    if let Some(digits) = text.strip_prefix('-') {
        return (!digits.is_empty() && digits.bytes().all(|digit| digit == b'0')).then_some(0);
    }
    text.parse().ok()
}
