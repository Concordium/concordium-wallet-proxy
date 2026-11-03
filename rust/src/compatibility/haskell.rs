//! This module formats Haskell Show strings for legacy wallet-facing descriptions.
//! Text uses character escapes; byte strings apply the same escapes to individual bytes.

/// Quote text with the Haskell Show escape rules.
pub(crate) fn show_text(text: &str) -> String {
    let mut output = String::from("\"");
    let mut chars = text.chars().peekable();
    while let Some(character) = chars.next() {
        match character {
            '\"' => output.push_str("\\\""),
            '\\' => output.push_str("\\\\"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            '\x07' => output.push_str("\\a"),
            '\x08' => output.push_str("\\b"),
            '\x0b' => output.push_str("\\v"),
            '\x0c' => output.push_str("\\f"),
            c if c > '\x7f' => {
                output.push_str(&format!("\\{}", c as u32));
                if chars.peek().is_some_and(char::is_ascii_digit) {
                    output.push_str("\\&");
                }
            }
            c if c.is_ascii_control() => {
                let names = [
                    "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT",
                    "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                    "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US",
                ];
                output.push('\\');
                output.push_str(if c == '\x7f' {
                    "DEL"
                } else {
                    names[c as usize]
                });
                if c == '\x0e' && chars.peek() == Some(&'H') {
                    output.push_str("\\&");
                }
            }
            c => output.push(c),
        }
    }
    output.push('\"');
    output
}

/// Quote each byte as a Haskell byte-string character.
pub(crate) fn show_bytes(bytes: &[u8]) -> String {
    show_text(
        &bytes
            .iter()
            .map(|byte| char::from(*byte))
            .collect::<String>(),
    )
}
