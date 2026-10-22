//! This module loads wallet update ranges and serves wallet settings and terms.
//! Version numbers are unsigned integers, not semantic versions.

use anyhow::{bail, Context};
use axum::{
    extract::{Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use serde::Deserialize;
use serde_json::json;
use std::{collections::HashMap, path::Path, sync::Arc};

/// These settings contain both wallet generations and the optional terms fields.
#[derive(Clone, Default)]
pub struct WalletSettings {
    /// These entries select generation 0 and generation 1.
    updates: [Platforms; 2],
    /// This value identifies the current terms.
    version: Option<String>,
    /// This URL points to the current terms.
    url: Option<String>,
}

/// These settings select a platform within one wallet generation.
#[derive(Clone, Default, Deserialize)]
struct Platforms {
    /// These settings apply to iOS.
    ios: Option<Update>,
    /// These settings apply to Android.
    android: Option<Update>,
}

/// These fields describe update guidance from a configuration file.
#[derive(Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Update {
    /// This text lists versions that require an update.
    #[serde(default)]
    force_update_versions: Option<String>,
    /// This text lists versions for which an update is suggested.
    #[serde(default)]
    suggest_update_versions: Option<String>,
    /// This URL points to the required update.
    url: Option<String>,
    /// This URL overrides the required URL for suggestions.
    suggest_url: Option<String>,
    /// These parsed intervals require an update.
    #[serde(skip)]
    forced: Vec<Interval>,
    /// These parsed intervals suggest an update.
    #[serde(skip)]
    suggested: Vec<Interval>,
}

/// This type stores inclusive lower and upper bounds.
#[derive(Clone)]
struct Interval {
    /// This bound defaults to zero for an open lower end.
    lower: u64,
    /// This bound defaults to the largest version for an open upper end.
    upper: u64,
}

fn ranges(text: &str) -> anyhow::Result<Vec<Interval>> {
    if text.is_empty() {
        return Ok(Vec::new());
    }
    text.split(',')
        .map(|part| {
            if part == "*" {
                return Ok(Interval {
                    lower: 0,
                    upper: u64::MAX,
                });
            }
            if part == "-" {
                bail!("A range must have at least one numeric endpoint");
            }
            let number = |s: &str| -> anyhow::Result<u64> {
                if s.is_empty() || !s.bytes().all(|c| c.is_ascii_digit()) {
                    bail!("Invalid version range");
                }
                Ok(s.parse()?)
            };
            let (lower, upper) = match part.split_once('-') {
                Some((a, b)) => (
                    if a.is_empty() { 0 } else { number(a)? },
                    if b.is_empty() { u64::MAX } else { number(b)? },
                ),
                None => {
                    let n = number(part)?;
                    (n, n)
                }
            };
            if lower > upper {
                bail!("Range lower bound must not exceed upper bound");
            }
            Ok(Interval { lower, upper })
        })
        .collect()
}

impl WalletSettings {
    /// Load both optional update files and retain the independent terms fields.
    pub fn load(config: &crate::config::WalletConfig) -> anyhow::Result<Self> {
        fn load(path: Option<&Path>) -> anyhow::Result<Platforms> {
            let Some(path) = path else {
                return Ok(Platforms::default());
            };
            let mut platforms: Platforms =
                serde_json::from_slice(&std::fs::read(path).with_context(|| {
                    format!("Cannot read update configuration {}", path.display())
                })?)?;
            for update in [&mut platforms.ios, &mut platforms.android]
                .into_iter()
                .flatten()
            {
                update.forced = ranges(update.force_update_versions.as_deref().unwrap_or(""))
                    .context("Invalid forced update range")?;
                update.suggested = ranges(update.suggest_update_versions.as_deref().unwrap_or(""))
                    .context("Invalid suggest update range")?;
                if update.url.is_none()
                    && (!update.forced.is_empty() || !update.suggested.is_empty())
                {
                    bail!("If non-empty ranges are given then the URL must be present.");
                }
            }
            Ok(platforms)
        }
        Ok(Self {
            updates: [load(config.v0.as_deref())?, load(config.v1.as_deref())?],
            version: config.tc_version.clone(),
            url: config.tc_url.clone(),
        })
    }
}

pub(crate) fn routes(settings: WalletSettings) -> Router {
    Router::new()
        .route("/v0/appSettings", get(settings_zero))
        .route("/v1/appSettings", get(settings_one))
        .route("/v0/termsAndConditionsVersion", get(terms))
        .with_state(Arc::new(settings))
}

async fn settings_zero(
    State(settings): State<Arc<WalletSettings>>,
    Query(query): Query<HashMap<String, String>>,
) -> Response {
    settings_response(&settings.updates[0], query)
}
async fn settings_one(
    State(settings): State<Arc<WalletSettings>>,
    Query(query): Query<HashMap<String, String>>,
) -> Response {
    settings_response(&settings.updates[1], query)
}

fn settings_response(platforms: &Platforms, query: HashMap<String, String>) -> Response {
    let platform = query.get("platform");
    let version = query
        .get("appVersion")
        .and_then(|v| v.trim().parse::<u64>().ok());
    let message = match (platform, version) {
        (None, None) => Some("'platform' field is not present and 'version' field is either not present or not readable."),
        (None, Some(_)) => Some("'platform' field is not present."),
        (Some(_), None) => Some("'version' parameter is not present or readable. It must be a non-negative integer."),
        (Some(p), Some(_)) if p != "android" && p != "ios" => Some("'param' should either be 'android' or 'ios'"),
        _ => None,
    };
    if let Some(message) = message {
        return crate::http::error(
            StatusCode::BAD_REQUEST,
            1,
            &format!("Parsing input failed: {message}"),
        );
    }
    let update = if platform.unwrap() == "ios" {
        &platforms.ios
    } else {
        &platforms.android
    };
    let version = version.unwrap();
    if let Some(update) = update {
        let matches = |ranges: &[Interval]| {
            ranges
                .iter()
                .any(|r| r.lower <= version && version <= r.upper)
        };
        if matches(&update.forced) {
            return Json(json!({"status":"needsUpdate", "url":update.url})).into_response();
        }
        if matches(&update.suggested) {
            return Json(json!({"status":"warning", "url":update.suggest_url.as_ref().or(update.url.as_ref())})).into_response();
        }
    }
    Json(json!({"status":"ok"})).into_response()
}

async fn terms(State(settings): State<Arc<WalletSettings>>) -> Response {
    match (&settings.version, &settings.url) {
        (Some(version), Some(url)) => Json(json!({"version":version,"url":url})).into_response(),
        _ => crate::http::error(StatusCode::NOT_FOUND, 1, "Not found"),
    }
}
