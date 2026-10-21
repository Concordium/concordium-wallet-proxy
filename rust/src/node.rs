//! This module connects to the Concordium node through the Rust SDK.
//! It supplies cryptographic parameters and the time of the last finalized block.

use crate::config::NodeConfig;
use anyhow::Context;
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use concordium_rust_sdk::{
    endpoints::{QueryError, RPCError},
    types::CryptographicParameters,
    v2::{BlockIdentifier, Client},
};
use std::{error::Error, time::Duration};
use tonic::transport::{ClientTlsConfig, Endpoint};

/// This type identifies node failures that prevent a health result.
#[derive(Debug, thiserror::Error)]
pub enum NodeFailure {
    /// The client could not communicate with the node.
    #[error("Cannot communicate with node via GRPC endpoint.")]
    Transport,
    /// The client could not decode or validate the node response.
    #[error("Invalid node response")]
    InvalidResponse,
}

/// This interface supplies the node operations for startup and health checks.
#[async_trait]
pub trait Node: Send + Sync {
    /// Get cryptographic parameters from the last finalized block.
    /// A failure must stop startup.
    async fn cryptographic_parameters(&self) -> anyhow::Result<CryptographicParameters>;
    /// Get the last finalized block time in UTC.
    /// Return `None` if the health query has no usable RPC result.
    async fn last_final_time(&self) -> Result<Option<DateTime<Utc>>, NodeFailure>;
}

/// This type supplies node operations through the SDK.
/// Client copies share the existing transport connection.
#[derive(Clone)]
pub struct SdkNode {
    /// This client supplies copies for independent requests.
    client: Client,
    /// New connection attempts use these settings after a transport failure.
    endpoint: Endpoint,
    /// This count permits additional connection attempts after the first failure.
    retries: u32,
    /// This duration limits each RPC call.
    timeout: Duration,
}

impl SdkNode {
    /// Connect to the node with the configured retry count.
    /// HTTPS uses system trust roots and checks the endpoint host name.
    /// Return an error if the connection fails or a target header is set.
    pub async fn connect(config: &NodeConfig) -> anyhow::Result<Self> {
        if config.target.is_some() {
            anyhow::bail!("SDK does not support the configured target header");
        }
        let timeout = Duration::from_secs(config.timeout_seconds);
        let mut endpoint = Endpoint::from_shared(config.endpoint.clone())?.connect_timeout(timeout);
        if config.endpoint.starts_with("https://") {
            endpoint = endpoint.tls_config(ClientTlsConfig::new().with_native_roots())?;
        }
        let client = connect_with_retries(&endpoint, config.connect_retries).await?;
        Ok(Self {
            client,
            endpoint,
            retries: config.connect_retries,
            timeout,
        })
    }

    async fn query_time(&self, mut client: Client) -> Result<Option<DateTime<Utc>>, NodeFailure> {
        match tokio::time::timeout(
            self.timeout,
            client.get_block_info(BlockIdentifier::LastFinal),
        )
        .await
        {
            // Haskell's health wrapper suppresses non-OK RPC status, including deadlines.
            Err(_) => Ok(None),
            Ok(Ok(response)) => Ok(Some(response.response.block_slot_time)),
            Ok(Err(error)) => {
                tracing::warn!(%error, "Last-final block lookup failed");
                match error {
                    QueryError::RPCError(
                        RPCError::ParseError(_) | RPCError::InvalidMetadata(_),
                    ) => Err(NodeFailure::InvalidResponse),
                    QueryError::RPCError(RPCError::CallError(status))
                        if status.source().is_some() =>
                    {
                        Err(NodeFailure::Transport)
                    }
                    _ => Ok(None),
                }
            }
        }
    }
}

async fn connect_with_retries(endpoint: &Endpoint, retries: u32) -> anyhow::Result<Client> {
    for attempt in 0..=retries {
        match Client::new(endpoint.clone()).await {
            Ok(client) => return Ok(client),
            Err(error) => {
                tracing::warn!(attempt, %error, "Node connection failed");
                // The Haskell connector waits one second after each failed attempt.
                tokio::time::sleep(Duration::from_secs(1)).await;
                if attempt == retries {
                    return Err(error).context("Cannot connect to node");
                }
            }
        }
    }
    unreachable!("At least one connection attempt is made")
}

#[async_trait]
impl Node for SdkNode {
    async fn cryptographic_parameters(&self) -> anyhow::Result<CryptographicParameters> {
        let mut client = self.client.clone();
        let response = tokio::time::timeout(
            self.timeout,
            client.get_cryptographic_parameters(BlockIdentifier::LastFinal),
        )
        .await
        .context("Cryptographic-parameter query timed out")?
        .context("Cannot obtain cryptographic parameters")?;
        Ok(response.response)
    }

    async fn last_final_time(&self) -> Result<Option<DateTime<Utc>>, NodeFailure> {
        match self.query_time(self.client.clone()).await {
            Err(NodeFailure::Transport) => {
                let client = connect_with_retries(&self.endpoint, self.retries)
                    .await
                    .map_err(|_| NodeFailure::Transport)?;
                self.query_time(client).await
            }
            result => result,
        }
    }
}
