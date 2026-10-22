//! This module supplies response conversions shared by wallet-facing features.
//! It preserves the Haskell service's UTC timestamp representation.

use chrono::{DateTime, Utc};

/// Format a UTC time without trailing fractional zeros, as Aeson does.
pub(crate) fn format_time(time: DateTime<Utc>) -> String {
    let mut value = time.to_rfc3339_opts(chrono::SecondsFormat::Nanos, true);
    while value.ends_with("0Z") {
        value.remove(value.len() - 2);
    }
    if value.ends_with(".Z") {
        value.remove(value.len() - 2);
    }
    value
}
