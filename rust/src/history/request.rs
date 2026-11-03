//! This module reads legacy history filters after canonical account lookup.
//! It retains first-value precedence, defaulted cursors, and ordered validation errors.

use super::{HistoryOrder, HistoryQuery, HistoryVersion, RewardFilter};
use crate::compatibility::numbers;

/// These parameters control row selection and response detail rendering.
pub(super) struct Request {
    /// These filters run in PostgreSQL before paging.
    pub query: HistoryQuery,
    /// This flag includes the raw rejection object when the parameter is present.
    pub raw_rejection: bool,
}

impl Request {
    /// Read query values in the validation order used by the legacy handler.
    pub fn parse(
        version: HistoryVersion,
        values: &[(String, String)],
    ) -> Result<Self, &'static str> {
        let get = |name: &str| {
            values
                .iter()
                .find(|(key, _)| key == name)
                .map(|(_, value)| value.as_str())
        };
        let bound = |name, message, time: bool| {
            get(name)
                .map(|text| {
                    let value = if time {
                        numbers::word64(text)
                    } else {
                        numbers::int(text).map(|v| v as u64)
                    };
                    value
                        .map(|value| {
                            if time {
                                value.wrapping_mul(1000)
                            } else {
                                value
                            }
                        })
                        .ok_or(message)
                })
                .transpose()
        };
        let excluded = |name, message| match get(name) {
            None | Some("y") => Ok(false),
            Some("n") => Ok(true),
            _ => Err(message),
        };
        let time_from = bound(
            "blockTimeFrom",
            "Unsupported 'blockTimeFrom' parameter.",
            true,
        )?;
        let time_to = bound("blockTimeTo", "Unsupported 'blockTimeTo' parameter.", true)?;
        let exclude_block_rewards =
            excluded("blockRewards", "Unsupported 'blockReward' parameter.")?;
        let exclude_finalization_rewards = excluded(
            "finalizationRewards",
            "Unsupported 'finalizationReward' parameter.",
        )?;
        let exclude_baking_rewards =
            excluded("bakingRewards", "Unsupported 'bakingReward' parameter.")?;
        let only_encrypted = match get("onlyEncrypted") {
            None | Some("n") => false,
            Some("y") => true,
            _ => return Err("Unsupported 'onlyEncrypted' parameter."),
        };
        let rewards = match get("includeRewards") {
            None | Some("all") => RewardFilter::All,
            Some("allButFinalization") => RewardFilter::AllButFinalization,
            Some("none") => RewardFilter::None,
            _ => return Err("Unsupported 'includeRewards' parameter."),
        };
        let height_from = bound(
            "blockHeightFrom",
            "Unsupported 'blockHeightFrom' parameter.",
            false,
        )?;
        let height_to = bound(
            "blockHeightTo",
            "Unsupported 'blockHeightTo' parameter.",
            false,
        )?;
        Ok(Self {
            query: HistoryQuery {
                version,
                order: if get("order").is_some_and(|v| v.to_lowercase().starts_with('d')) {
                    HistoryOrder::Descending
                } else {
                    HistoryOrder::Ascending
                },
                from: get("from").and_then(cursor),
                limit: get("limit")
                    .and_then(numbers::int)
                    .unwrap_or(20)
                    .clamp(0, 1000) as u16,
                time_from,
                time_to,
                height_from,
                height_to,
                rewards,
                exclude_block_rewards,
                exclude_finalization_rewards,
                exclude_baking_rewards,
                only_encrypted,
            },
            raw_rejection: get("includeRawRejectReason").is_some(),
        })
    }
}

/// Read a signed decimal PathPiece without the wider Read integer syntax.
fn cursor(text: &str) -> Option<i64> {
    // PathPiece checks Int64 bounds instead of applying Read's modular conversion.
    text.parse().ok()
}
