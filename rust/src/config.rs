//! This module reads command-line arguments and environment variables for startup.
//! It checks settings and loads the identity-provider catalogue files.

use anyhow::{bail, Context};
use clap::{Args, Parser};
use std::{
    net::SocketAddr,
    num::NonZeroU32,
    path::{Path, PathBuf},
};

/// This type contains the service settings.
/// Relative file paths start at the process working directory.
#[derive(Clone, Parser)]
#[command(version, about = "Concordium wallet proxy server")]
pub struct Config {
    /// These settings control the HTTP server and its health checks.
    #[command(flatten, next_help_heading = "Server")]
    pub server: ServerConfig,
    /// These settings identify the transaction-history database.
    #[command(flatten, next_help_heading = "Database")]
    pub database: DatabaseConfig,
    /// These settings control the connection to the Concordium node.
    #[command(flatten, next_help_heading = "Node")]
    pub node: NodeConfig,
    /// These paths identify the three identity-provider catalogues.
    #[command(flatten, next_help_heading = "Catalogues")]
    pub catalogs: CatalogConfig,
}

/// This type contains the HTTP server settings.
#[derive(Clone, Args)]
#[group(skip)]
pub struct ServerConfig {
    /// The server listens at this address. The default is `0.0.0.0:3000`.
    #[arg(long, env = "WALLET_PROXY_LISTEN", default_value_t = Self::default().listen)]
    pub listen: SocketAddr,
    /// This value is the block age limit, in seconds. A block at this age is stale.
    #[arg(long, env = "WALLET_PROXY_HEALTH_TOLERANCE_SECONDS", default_value_t = Self::default().health_tolerance_seconds, allow_negative_numbers = true)]
    pub health_tolerance_seconds: i64,
    /// This flag permits browser requests from any origin on public routes.
    #[arg(long, env = "WALLET_PROXY_ENABLE_PUBLIC_CORS", action = clap::ArgAction::Set, num_args = 0..=1, default_missing_value = "true", require_equals = true, default_value_t = false)]
    pub enable_public_cors: bool,
    /// This filter selects log messages. The default value, `off`, disables logging.
    #[arg(long, env = "WALLET_PROXY_LOG_FILTER", default_value_t = Self::default().log_filter)]
    pub log_filter: String,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            listen: ([0, 0, 0, 0], 3000).into(),
            health_tolerance_seconds: 300,
            enable_public_cors: false,
            log_filter: "off".into(),
        }
    }
}

/// This type contains the PostgreSQL connection settings.
#[derive(Clone, Args)]
#[group(skip)]
pub struct DatabaseConfig {
    /// This required URL supplies the database connection settings.
    /// Use the environment variable for credentials.
    #[arg(
        long = "database-url",
        env = "WALLET_PROXY_DATABASE_URL",
        hide_env_values = true
    )]
    pub url: String,
    /// This positive count limits connections in the PostgreSQL pool. The default is 10.
    #[arg(
        long = "database-max-connections",
        env = "WALLET_PROXY_DATABASE_MAX_CONNECTIONS",
        default_value = "10"
    )]
    pub max_connections: NonZeroU32,
}

/// This type controls node connections and gRPC requests.
#[derive(Clone, Args)]
#[group(skip)]
pub struct NodeConfig {
    /// This required HTTP or HTTPS URL identifies the node. It must not contain a path or query.
    #[arg(long = "node-endpoint", env = "WALLET_PROXY_NODE_ENDPOINT")]
    pub endpoint: String,
    /// This header selects a node behind a gRPC proxy. Validation rejects it because the SDK does not support it.
    #[arg(long = "node-target", env = "WALLET_PROXY_NODE_TARGET")]
    pub target: Option<String>,
    /// This count permits additional connection attempts after the first failure.
    #[arg(
        long = "node-connect-retries",
        env = "WALLET_PROXY_NODE_CONNECT_RETRIES",
        default_value_t = 0
    )]
    pub connect_retries: u32,
    /// This positive value limits each connection attempt and RPC call, in seconds.
    #[arg(
        long = "node-timeout-seconds",
        env = "WALLET_PROXY_NODE_TIMEOUT_SECONDS",
        default_value_t = 15
    )]
    pub timeout_seconds: u64,
}

impl NodeConfig {
    /// Create node settings for an explicit endpoint with the standard retry count and timeout.
    pub fn new(endpoint: impl Into<String>) -> Self {
        Self {
            endpoint: endpoint.into(),
            target: None,
            connect_retries: 0,
            timeout_seconds: 15,
        }
    }
}

/// This type identifies the JSON files for each identity-provider catalogue version.
#[derive(Clone, Args)]
#[group(skip)]
pub struct CatalogConfig {
    /// This file supplies the version 0 catalogue.
    #[arg(long = "ip-data", env = "WALLET_PROXY_IP_DATA")]
    pub v0: PathBuf,
    /// This file supplies the version 1 catalogue.
    #[arg(long = "ip-data-v1", env = "WALLET_PROXY_IP_DATA_V1")]
    pub v1: PathBuf,
    /// This file supplies the version 2 catalogue.
    #[arg(long = "ip-data-v2", env = "WALLET_PROXY_IP_DATA_V2")]
    pub v2: PathBuf,
}

impl Config {
    /// Check settings before a network connection starts.
    /// Return an error if a setting is invalid or the SDK does not support it.
    pub fn validate(&self) -> anyhow::Result<()> {
        self.database.validate()?;
        if self.node.timeout_seconds == 0 {
            bail!("Node timeout must be positive");
        }
        if self.node.target.is_some() {
            bail!("The node target header is not supported by SDK 10; see rust/README.md compatibility issues");
        }
        let uri: http::Uri = self
            .node
            .endpoint
            .parse()
            .context("Invalid node endpoint URI")?;
        if !matches!(uri.scheme_str(), Some("http" | "https"))
            || uri.authority().is_none()
            || uri.path() != "/"
            || uri.query().is_some()
        {
            bail!("Node endpoint must be an http(s) origin without a path or query");
        }
        if self
            .server
            .health_tolerance_seconds
            .checked_mul(1000)
            .is_none()
        {
            bail!("Health tolerance is outside the supported duration range");
        }
        tracing_subscriber::EnvFilter::try_new(&self.server.log_filter)
            .context("Invalid log filter")?;
        Ok(())
    }
}

impl DatabaseConfig {
    /// Check the database URL without including it in error messages.
    fn validate(&self) -> anyhow::Result<()> {
        use std::str::FromStr;
        sqlx::postgres::PgConnectOptions::from_str(&self.url)
            .map_err(|_| anyhow::anyhow!("Invalid PostgreSQL connection URL"))?;
        Ok(())
    }
}

/// Read the catalogue JSON files in version order: 0, 1, then 2.
/// Return an error if a file is missing or contains invalid JSON.
pub fn load_catalogs(config: &CatalogConfig) -> anyhow::Result<[serde_json::Value; 3]> {
    let load = |path: &Path| -> anyhow::Result<serde_json::Value> {
        let bytes = std::fs::read(path)
            .with_context(|| format!("Cannot read catalogue {}", path.display()))?;
        serde_json::from_slice(&bytes)
            .with_context(|| format!("Invalid catalogue JSON {}", path.display()))
    };
    Ok([load(&config.v0)?, load(&config.v1)?, load(&config.v2)?])
}
