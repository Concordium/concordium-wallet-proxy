use clap::Parser;

use tracing_subscriber::{layer::SubscriberExt as _, util::SubscriberInitExt as _};
use wallet_proxy::configuration::Cli;
use wallet_proxy::service;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let filter = if std::env::var("RUST_LOG").is_ok() {
        // If RUST_LOG env is defined we fall back to the default behavior of the env
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

    service::run_service(cli).await
}
