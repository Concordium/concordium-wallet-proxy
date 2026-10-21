//! This module checks node and database health in the required order.
//! It compares block age with the configured limit and prepares the HTTP response data.

use crate::{
    database::History,
    node::{Node, NodeFailure},
};
use chrono::{DateTime, Utc};
use serde::Serialize;
use std::sync::Arc;

/// This interface supplies the current time for block age checks.
pub trait Clock: Send + Sync {
    /// Get the current UTC time.
    fn now(&self) -> DateTime<Utc>;
}

/// This type reads UTC time from the system clock.
pub struct SystemClock;
impl Clock for SystemClock {
    fn now(&self) -> DateTime<Utc> {
        Utc::now()
    }
}

/// This type contains the resources and age limit for a health check.
#[derive(Clone)]
pub struct Health {
    /// This interface supplies the last finalized block time.
    pub node: Arc<dyn Node>,
    /// This interface checks access to transaction-history records.
    pub history: Arc<dyn History>,
    /// This clock supplies the current UTC time.
    pub clock: Arc<dyn Clock>,
    /// This validated limit sets the maximum block age, in seconds. A block at the limit is stale.
    pub tolerance_seconds: i64,
}

/// This type contains the existing wallet-facing health response.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct HealthResponse {
    /// This flag is true when the database check passes and the block age is below the limit.
    pub healthy: bool,
    /// This message explains an unhealthy result. Healthy responses omit it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<&'static str>,
    /// This string contains the block time in UTC. Responses without a block result omit it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_final_time: Option<String>,
    /// This string identifies the service package version.
    pub version: &'static str,
}

/// This type identifies failures that require an HTTP error response.
#[derive(Debug, thiserror::Error)]
pub enum HealthError {
    /// The node query failed before it supplied a usable health result.
    #[error(transparent)]
    Node(
        /// This error identifies the node failure.
        #[from]
        NodeFailure,
    ),
    /// The transaction-history database check failed.
    #[error(transparent)]
    Database(
        /// This error identifies the database or record decoding failure.
        #[from]
        anyhow::Error,
    ),
}

// Aeson's UTCTime rendering omits trailing fractional zeros; Chrono's
// default serializer instead groups fractional digits in threes.
fn format_time(time: DateTime<Utc>) -> String {
    let mut value = time.to_rfc3339_opts(chrono::SecondsFormat::Nanos, true);
    while value.ends_with("0Z") {
        value.remove(value.len() - 2);
    }
    if value.ends_with(".Z") {
        value.remove(value.len() - 2);
    }
    value
}

impl Health {
    /// Check the node, then the database, then the block age.
    /// Do not check the database when the node supplies no block result.
    pub async fn check(&self) -> Result<HealthResponse, HealthError> {
        let Some(time) = self.node.last_final_time().await? else {
            return Ok(HealthResponse {
                healthy: false,
                reason: Some("Could not get response from GRPC."),
                last_final_time: None,
                version: env!("CARGO_PKG_VERSION"),
            });
        };
        self.history.check().await?;
        let age = self.clock.now().signed_duration_since(time);
        // The comparison is strict and retains the clock's subsecond precision.
        let threshold = chrono::TimeDelta::try_seconds(self.tolerance_seconds)
            .expect("Validated health tolerance");
        let healthy = age < threshold;
        Ok(HealthResponse {
            healthy,
            reason: if healthy {
                None
            } else {
                Some("The last final block is too old.")
            },
            last_final_time: Some(format_time(time)),
            version: env!("CARGO_PKG_VERSION"),
        })
    }
}
