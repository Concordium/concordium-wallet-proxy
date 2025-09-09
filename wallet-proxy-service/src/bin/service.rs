use anyhow::Context;
use axum::{Json, http::StatusCode};
use clap::Parser;

use prometheus_client::{
    metrics::{family::Family, gauge::Gauge},
    registry::Registry,
};

use serde_json::json;

use concordium_rust_sdk::v2;
use std::net::SocketAddr;
use tokio::net::TcpListener;
use tokio_util::sync::CancellationToken;
use tracing::{error, info};
use tracing_subscriber::{layer::SubscriberExt as _, util::SubscriberInitExt as _};
use wallet_proxy::monitoring_api;

#[derive(Parser)]
struct Cli {
    /// The URL used for the database, something of the form:
    /// "postgres://postgres:example@localhost/walletproxy".
    /// Use an environment variable when the connection contains a password, as
    /// command line arguments are visible across OS processes.
    #[arg(long, env = "WALLET_PROXY_DATABASE_URL")]
    database_url: String,
    /// gRPC interface of the node. Several can be provided.
    #[arg(long, env = "WALLET_PROXY_NODE_GRPC_ENDPOINT")]
    node: v2::Endpoint,
    /// Minimum number of connections in the pool.
    #[arg(
        long,
        env = "WALLET_PROXY_DATABASE_MIN_CONNECTIONS",
        default_value_t = 5
    )]
    min_connections: u32,
    /// Maximum number of connections in the pool.
    #[arg(
        long,
        env = "WALLET_PROXY_DATABASE_MAX_CONNECTIONS",
        default_value_t = 10
    )]
    max_connections: u32,
    /// Database statement timeout. Abort any statement that takes more than the
    /// specified amount of time. Set to 0 to disable.
    #[arg(
        long,
        env = "WALLET_PROXY_DATABASE_STATEMENT_TIMEOUT_SECS",
        default_value_t = 30
    )]
    statement_timeout_secs: u64,
    /// Address to listen to for API requests.
    #[arg(long, env = "WALLET_PROXY_ADDRESS", default_value = "127.0.0.1:8000")]
    listen: SocketAddr,
    /// Address to listen for monitoring related requests
    #[arg(
        long,
        env = "WALLET_PROXY_MONITORING_ADDRESS",
        default_value = "127.0.0.1:8003"
    )]
    monitoring_listen: SocketAddr,
    /// The maximum log level. Possible values are: `trace`, `debug`, `info`,
    /// `warn`, and `error`.
    #[arg(long, default_value = "info", env = "LOG_LEVEL")]
    log_level: tracing_subscriber::filter::LevelFilter,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let filter = if std::env::var("RUST_LOG").is_ok() {
        // If RUST_LOG env is defined we fallback to the default behavior of the env
        // filter.
        tracing_subscriber::EnvFilter::builder().from_env_lossy()
    } else {
        // If RUST_LOG env is not defined, set the --log-level only for this project and
        // leave dependencies filter to info level.
        let pkg_name = env!("CARGO_PKG_NAME").replace('-', "_");
        let crate_name = env!("CARGO_CRATE_NAME");
        format!("info,{pkg_name}={0},{crate_name}={0}", cli.log_level).parse()?
    };
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(filter)
        .init();

    let cancel_token = CancellationToken::new();
    let service_info_family = Family::<Vec<(&str, String)>, Gauge>::default();
    let gauge =
        service_info_family.get_or_create(&vec![("version", clap::crate_version!().to_string())]);
    gauge.set(1);
    let mut registry = Registry::with_prefix("api");
    registry.register(
        "service_info",
        "Information about the software",
        service_info_family.clone(),
    );
    registry.register(
        "service_startup_timestamp_millis",
        "Timestamp of starting up the API service (Unix time in milliseconds)",
        prometheus_client::metrics::gauge::ConstGauge::new(chrono::Utc::now().timestamp_millis()),
    );

    let mut queries_task = {
        let tcp_listener = TcpListener::bind(cli.listen)
            .await
            .context("Parsing TCP listener address failed")?;
        let stop_signal = cancel_token.child_token();
        info!("Server is running at {:?}", cli.listen);
        tokio::spawn(async move {
            axum::serve(
                tcp_listener,
                axum::Router::new(), // TODO implement as part of COR-1810
            )
            .with_graceful_shutdown(stop_signal.cancelled_owned())
            .await
        })
    };

    let mut monitoring_task = {
        let tcp_listener = TcpListener::bind(cli.monitoring_listen)
            .await
            .context("Parsing TCP listener address failed")?;
        let stop_signal = cancel_token.child_token();
        info!(
            "Monitoring server is running at {:?}",
            cli.monitoring_listen
        );
        tokio::spawn(monitoring_api::serve(
            registry,
            tcp_listener,
            stop_signal,
        ))
    };

    // Await for signal to shutdown or any of the tasks to stop.
    tokio::select! {
        _ = tokio::signal::ctrl_c() => {
            info!("Received signal to shutdown");
            cancel_token.cancel();
        },
        result = &mut queries_task => {
            error!("Queries task stopped.");
            if let Err(err) = result? {
                error!("Queries error: {}", err);
            }
            cancel_token.cancel();
        }
        result = &mut monitoring_task => {
            error!("Monitoring task stopped.");
            if let Err(err) = result? {
                error!("Monitoring error: {}", err);
            }
            cancel_token.cancel();
        }
    }
    info!("Shutting down");
    // Ensure all tasks have stopped
    let _ = tokio::join!(monitoring_task, queries_task,);
    Ok(())
}

