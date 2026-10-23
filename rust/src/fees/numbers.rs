//! This module reads integer query values with legacy integer syntax.
//! It retains decimal, hexadecimal, octal, parentheses, whitespace, and modular conversion.

use num::{BigInt, ToPrimitive};

/// Read an Int with the deployed 64-bit representation.
pub(super) fn int(text: &str) -> Option<i64> {
    Some(bits(text, 64)? as i64)
}

/// Read a Word32 with modular conversion.
pub(super) fn word32(text: &str) -> Option<u32> {
    Some(bits(text, 32)? as u32)
}

/// Read a Word64 with modular conversion.
pub(super) fn word64(text: &str) -> Option<u64> {
    bits(text, 64)
}

/// Read an integer and reduce it to the requested unsigned representation.
fn bits(text: &str, width: usize) -> Option<u64> {
    let mut text = text.trim();
    while let Some(inner) = text.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
        text = inner.trim();
    }
    let (negative, text) = match text.strip_prefix('-') {
        Some(rest) => (true, rest.trim_start()),
        None => (false, text),
    };
    let (radix, digits) = if text.starts_with("0x") || text.starts_with("0X") {
        (16, &text[2..])
    } else if text.starts_with("0o") || text.starts_with("0O") {
        (8, &text[2..])
    } else {
        (10, text)
    };
    if digits.is_empty()
        || !digits
            .bytes()
            .all(|c| c.is_ascii() && char::from(c).is_digit(radix))
    {
        return None;
    }
    let value = BigInt::parse_bytes(digits.as_bytes(), radix)?;
    let value = if negative { -value } else { value };
    let modulus = BigInt::from(1u32) << width;
    ((value % &modulus + &modulus) % modulus).to_u64()
}
