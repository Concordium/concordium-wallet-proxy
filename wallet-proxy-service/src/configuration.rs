use clap::Parser;
use concordium_rust_sdk::v2;
use std::net::SocketAddr;

#[derive(Parser)]
pub struct Cli {
    /// The URL used for the database, something of the form:
    /// "postgres://postgres:example@localhost/walletproxy".
    /// Use an environment variable when the connection contains a password, as
    /// command line arguments are visible across OS processes.
    #[arg(long, env = "WALLET_PROXY_DATABASE_URL")]
    pub database_url: String,
    /// gRPC interface of the node. Several can be provided.
    #[arg(long, env = "WALLET_PROXY_NODE_GRPC_ENDPOINT")]
    pub node: v2::Endpoint,
    /// Minimum number of connections in the pool.
    #[arg(
        long,
        env = "WALLET_PROXY_DATABASE_MIN_CONNECTIONS",
        default_value_t = 5
    )]
    pub min_connections: u32,
    /// Maximum number of connections in the pool.
    #[arg(
        long,
        env = "WALLET_PROXY_DATABASE_MAX_CONNECTIONS",
        default_value_t = 10
    )]
    pub max_connections: u32,
    /// Database statement timeout. Abort any statement that takes more than the
    /// specified amount of time. Set to 0 to disable.
    #[arg(
        long,
        env = "WALLET_PROXY_DATABASE_STATEMENT_TIMEOUT_SECS",
        default_value_t = 30
    )]
    pub statement_timeout_secs: u64,
    /// Address to listen to for API requests.
    #[arg(long, env = "WALLET_PROXY_ADDRESS", default_value = "127.0.0.1:8000")]
    pub listen: SocketAddr,
    /// Address to listen for monitoring related requests
    #[arg(
        long,
        env = "WALLET_PROXY_MONITORING_ADDRESS",
        default_value = "127.0.0.1:8003"
    )]
    pub monitoring_listen: SocketAddr,
    /// The maximum log level. Possible values are: `trace`, `debug`, `info`,
    /// `warn`, and `error`.
    #[arg(long, default_value = "info", env = "LOG_LEVEL")]
    pub log_level: tracing_subscriber::filter::LevelFilter,
}
