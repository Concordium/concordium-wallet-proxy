//! This module creates optional Transak purchase sessions with a shared access token.
//! Refresh coordination and the one-time 401 retry follow the legacy service.

mod client;
pub mod config;
mod cors;
mod routes;
mod uri;

use crate::health::Clock;
use client::{AccessToken, Client};
use concordium_rust_sdk::id::types::AccountAddress;
use config::{Endpoints, Settings};
use num::{BigInt, BigRational};
pub use routes::routes;
use std::{sync::Arc, time::Duration};
use tokio::sync::Mutex;

/// This service shares the HTTP client, configuration, clock, and token lock.
pub struct Transak<C> {
    /// These settings supply credentials and wallet-session parameters.
    settings: Settings,
    /// This client supplies refresh and widget-session operations.
    client: Client,
    /// This clock determines whether the cached token is close to expiry.
    clock: C,
    /// This lock coordinates refreshes and retains the old token on failure or cancellation.
    token: Mutex<AccessToken>,
}

/// This error selects the two legacy public upstream-failure responses.
#[derive(Debug)]
pub enum SessionFailure {
    /// Refresh, session status, response decoding, or transport failed.
    BadGateway,
    /// The whole operation, including waiting for a refresh lock, took ten seconds.
    Timeout,
}

impl<C: Clock> Transak<C> {
    /// Create a client with an initially expired token. Do not contact Transak at startup.
    pub fn new(settings: Settings, endpoints: Endpoints, clock: C) -> anyhow::Result<Self> {
        Ok(Self {
            settings,
            client: Client::new(endpoints)?,
            clock,
            token: Mutex::new(AccessToken::default()),
        })
    }
    /// Create one session with a ten-second bound over refreshes, waits, and the optional retry.
    pub async fn create(
        &self,
        ip: &str,
        address: AccountAddress,
    ) -> Result<String, SessionFailure> {
        tokio::time::timeout(Duration::from_secs(10), self.attempt(ip, address))
            .await
            .map_err(|_| SessionFailure::Timeout)?
            .map_err(|_| SessionFailure::BadGateway)
    }
    async fn attempt(&self, ip: &str, address: AccountAddress) -> anyhow::Result<String> {
        let token = self.access_token().await?;
        match self
            .client
            .session(&self.settings, &token, ip, address)
            .await?
        {
            Ok(url) => Ok(url),
            Err(reqwest::StatusCode::UNAUTHORIZED) => {
                // Do not force a refresh: another request may already have refreshed the token.
                let token = self.access_token().await?;
                self.client
                    .session(&self.settings, &token, ip, address)
                    .await?
                    .map_err(|_| anyhow::anyhow!("Transak session retry failed"))
            }
            Err(_) => anyhow::bail!("Transak session failed"),
        }
    }
    async fn access_token(&self) -> anyhow::Result<String> {
        let mut token = self.token.lock().await;
        let now = self.clock.now();
        let now = BigRational::from_integer(BigInt::from(now.timestamp()) + 60)
            + BigRational::new(now.timestamp_subsec_nanos().into(), 1_000_000_000.into());
        if now > token.expiry {
            // Keep the cached value untouched until the refresh and decoding both succeed.
            *token = self.client.refresh(&self.settings).await?;
        }
        Ok(token.value.clone())
    }
}

/// Build the configured service with production/staging endpoints and a shared clock.
pub fn configured<C: Clock>(
    settings: Option<Settings>,
    clock: C,
) -> anyhow::Result<Option<Arc<Transak<C>>>> {
    settings
        .map(|settings| {
            let endpoints = Endpoints::for_staging(settings.use_staging);
            Transak::new(settings, endpoints, clock).map(Arc::new)
        })
        .transpose()
}
