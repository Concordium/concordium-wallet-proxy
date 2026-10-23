//! This module formats JSON syntax failures with the pinned Aeson decoder's grammar.
//! An explicit stack keeps diagnostic scanning independent of Serde's error strings.

/// This state identifies the next expected JSON grammar element.
enum Expected {
    /// Read one JSON value.
    Value,
    /// Read the first array value or the closing bracket.
    ArrayStart,
    /// Read a comma or the closing array bracket.
    ArrayNext,
    /// Read the first object key or the closing brace.
    ObjectStart,
    /// Read a key after an object comma.
    ObjectKey,
    /// Read the colon after a key.
    Colon,
    /// Read a comma or the closing object brace.
    ObjectNext,
}

/// This scanner retains the original UTF-8 bytes for legacy diagnostic excerpts.
struct Scanner<'a> {
    /// These bytes contain the JSON query value.
    bytes: &'a [u8],
    /// This index identifies the next unread byte.
    offset: usize,
}

/// Validate one JSON value without checking trailing input before type conversion.
pub(super) fn validate(raw: &str) -> Result<(), String> {
    Scanner {
        bytes: raw.as_bytes(),
        offset: 0,
    }
    .scan()
}

/// Describe a backend mismatch after the source-compatible syntax check.
pub(super) fn error(raw: &str) -> String {
    validate(raw)
        .err()
        .unwrap_or_else(|| "JSON decoder compatibility limit".into())
}

impl Scanner<'_> {
    /// Scan the first value with an explicit continuation stack.
    fn scan(&mut self) -> Result<(), String> {
        let mut stack = vec![Expected::Value];
        while let Some(expected) = stack.pop() {
            self.space();
            match expected {
                Expected::Value => match self.peek() {
                    Some(b'{') => {
                        self.offset += 1;
                        stack.push(Expected::ObjectStart);
                    }
                    Some(b'[') => {
                        self.offset += 1;
                        stack.push(Expected::ArrayStart);
                    }
                    Some(b'"') => self.string()?,
                    Some(b'-' | b'0'..=b'9') => self.number()?,
                    Some(b'n') if self.literal(b"null") => {}
                    Some(b't') if self.literal(b"true") => {}
                    Some(b'f') if self.literal(b"false") => {}
                    _ => return Err(self.unexpected("JSON value")),
                },
                Expected::ArrayStart => match self.peek() {
                    None => return Err(self.unexpected("JSON value or ]")),
                    Some(b']') => self.offset += 1,
                    _ => {
                        stack.push(Expected::ArrayNext);
                        stack.push(Expected::Value);
                    }
                },
                Expected::ArrayNext => match self.peek() {
                    Some(b']') => self.offset += 1,
                    Some(b',') => {
                        self.offset += 1;
                        stack.push(Expected::ArrayNext);
                        stack.push(Expected::Value);
                    }
                    _ => return Err(self.unexpected(", or ]")),
                },
                Expected::ObjectStart | Expected::ObjectKey => match self.peek() {
                    Some(b'}') if matches!(expected, Expected::ObjectStart) => self.offset += 1,
                    Some(b'"') => {
                        self.string()?;
                        stack.push(Expected::Colon);
                    }
                    _ => {
                        return Err(self.unexpected(
                            if matches!(expected, Expected::ObjectStart) {
                                "record key literal or }"
                            } else {
                                "key literal"
                            },
                        ));
                    }
                },
                Expected::Colon => {
                    if self.peek() != Some(b':') {
                        return Err(self.unexpected(":"));
                    }
                    self.offset += 1;
                    stack.push(Expected::ObjectNext);
                    stack.push(Expected::Value);
                }
                Expected::ObjectNext => match self.peek() {
                    Some(b'}') => self.offset += 1,
                    Some(b',') => {
                        self.offset += 1;
                        stack.push(Expected::ObjectKey);
                    }
                    _ => return Err(self.unexpected(", or }")),
                },
            }
        }
        Ok(())
    }

    /// Read a string with Aeson's control-character and escape-validation order.
    fn string(&mut self) -> Result<(), String> {
        let start = self.offset;
        self.offset += 1;
        let mut escaped = false;
        while let Some(byte) = self.peek() {
            self.offset += 1;
            match byte {
                b'"' => {
                    if serde_json::from_slice::<String>(&self.bytes[start..self.offset]).is_err() {
                        return Err("Cannot decode input: Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream".into());
                    }
                    return Ok(());
                }
                b'\\' => {
                    escaped = true;
                    if self.peek().is_none() {
                        break;
                    }
                    self.offset += 1;
                }
                byte if byte >= 0x80 => escaped = true,
                byte if byte < 0x20 && !escaped => {
                    return Err("Unespected control character while parsing string literal".into());
                }
                _ => {}
            }
        }
        Err("Unexpected end-of-input while parsing string literal".into())
    }

    /// Read a number with the legacy leading-zero and exponent diagnostics.
    fn number(&mut self) -> Result<(), String> {
        if self.peek() == Some(b'-') {
            self.offset += 1;
        }
        self.require_digit(false)?;
        if self.peek() == Some(b'0') {
            self.offset += 1;
            if self.peek().is_some_and(|b| b.is_ascii_digit()) {
                return Err("Number literal with leading zero".into());
            }
        } else {
            self.digits();
        }
        if self.peek() == Some(b'.') {
            self.offset += 1;
            self.require_digit(false)?;
            self.digits();
        }
        if matches!(self.peek(), Some(b'e' | b'E')) {
            self.offset += 1;
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.offset += 1;
            }
            self.require_digit(true)?;
            self.digits();
        }
        Ok(())
    }

    /// Check the mandatory first digit of one number component.
    fn require_digit(&self, character: bool) -> Result<(), String> {
        match self.peek() {
            Some(b'0'..=b'9') => Ok(()),
            None => Err("Unexpected end-of-input while parsing number literal".into()),
            Some(byte) => {
                let detail = if character {
                    let shown = super::show(&char::from(byte).to_string());
                    format!(
                        "'{}'",
                        match byte {
                            b'\'' => "\\'",
                            b'\"' => "\"",
                            _ => &shown[1..shown.len() - 1],
                        }
                    )
                } else {
                    byte.to_string()
                };
                Err(format!("Unexpected {detail} while parsing number literal"))
            }
        }
    }

    /// Advance over ASCII decimal digits.
    fn digits(&mut self) {
        while self.peek().is_some_and(|b| b.is_ascii_digit()) {
            self.offset += 1;
        }
    }

    /// Consume one matching literal without reading past it.
    fn literal(&mut self, literal: &[u8]) -> bool {
        if self.bytes[self.offset..].starts_with(literal) {
            self.offset += literal.len();
            true
        } else {
            false
        }
    }

    /// Skip only the four whitespace bytes accepted by JSON.
    fn space(&mut self) {
        while matches!(self.peek(), Some(b' ' | b'\t' | b'\r' | b'\n')) {
            self.offset += 1;
        }
    }

    /// Read the current byte without advancing.
    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.offset).copied()
    }

    /// Format a grammar expectation with at most 30 original bytes.
    fn unexpected(&self, expected: &str) -> String {
        if self.offset == self.bytes.len() {
            format!("Unexpected end-of-input, expecting {expected}")
        } else {
            let excerpt: String = self.bytes[self.offset..]
                .iter()
                .take(30)
                .map(|byte| char::from(*byte))
                .collect();
            format!("Unexpected {}, expecting {expected}", super::show(&excerpt))
        }
    }
}
