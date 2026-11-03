//! This module reads the legacy CIS-2 wire formats without imposing newer SDK codec bounds.
//! It permits trailing return bytes, wrapped token-length prefixes, and 37-byte unsigned balances.

use concordium_rust_sdk::id::types::AccountAddress;
use num::BigUint;

/// This record contains a metadata URL and its optional SHA256 checksum.
pub(super) struct Metadata {
    /// This URL is UTF-8 text; the proxy does not fetch or validate its destination.
    pub url: String,
    /// These bytes contain the optional checksum.
    pub checksum: Option<[u8; 32]>,
}

/// Encode metadata IDs or balance pairs with the legacy unchecked token-length prefix.
pub(super) fn parameter(tokens: &[Vec<u8>], account: Option<AccountAddress>) -> Vec<u8> {
    let mut bytes = (tokens.len() as u16).to_le_bytes().to_vec();
    for token in tokens {
        bytes.push(token.len() as u8);
        bytes.extend(token);
        if let Some(account) = account {
            bytes.push(0);
            bytes.extend(account.0);
        }
    }
    bytes
}

/// Read every declared metadata entry, optionally requiring a singleton result.
pub(super) fn metadata(bytes: &[u8], expected: Option<usize>) -> Option<Vec<Metadata>> {
    let mut reader = Reader(bytes);
    let length = usize::from(reader.word()?);
    if expected.is_some_and(|expected| expected != length) {
        return None;
    }
    (0..length)
        .map(|_| {
            let length = usize::from(reader.word()?);
            let url = std::str::from_utf8(reader.take(length)?).ok()?.to_owned();
            let checksum = match reader.byte()? {
                0 => None,
                1 => Some(reader.take(32)?.try_into().ok()?),
                _ => return None,
            };
            Some(Metadata { url, checksum })
        })
        .collect()
}

/// Read balances only when the declared count matches the requested count.
pub(super) fn balances(bytes: &[u8], expected: usize) -> Option<Vec<BigUint>> {
    let mut reader = Reader(bytes);
    if usize::from(reader.word()?) != expected {
        return None;
    }
    (0..expected)
        .map(|_| {
            let mut amount = BigUint::from(0_u8);
            for position in 0..37 {
                let byte = reader.byte()?;
                amount += BigUint::from(byte & 127) << (position * 7);
                if byte & 128 == 0 {
                    return Some(amount);
                }
            }
            None
        })
        .collect()
}

/// This reader consumes required bytes without requiring exhaustion of the return value.
struct Reader<'a>(
    /// These bytes remain unread.
    &'a [u8],
);
impl<'a> Reader<'a> {
    /// Read exactly the requested bytes.
    fn take(&mut self, length: usize) -> Option<&'a [u8]> {
        let result = self.0.get(..length)?;
        self.0 = &self.0[length..];
        Some(result)
    }
    /// Read one byte.
    fn byte(&mut self) -> Option<u8> {
        Some(self.take(1)?[0])
    }
    /// Read a little-endian array or text length.
    fn word(&mut self) -> Option<u16> {
        Some(u16::from_le_bytes(self.take(2)?.try_into().ok()?))
    }
}
