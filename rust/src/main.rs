//! This executable starts the HTTP service after all required startup checks pass.
//! It stops the service when it receives an interrupt or termination signal.

use clap::Parser;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    use concordium_wallet_proxy::{config::Config, http::router_with_network, startup::initialize};
    let config = Config::parse();
    config.validate()?;
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::try_new(
            &config.server.log_filter,
        )?)
        .json()
        .with_writer(std::io::stderr)
        .init();
    let application = initialize(&config).await?;
    let listener = tokio::net::TcpListener::bind(config.server.listen).await?;
    tracing::info!(address = %listener.local_addr()?, "Wallet Proxy listening");
    axum::serve(
        listener,
        router_with_network(
            application.health.clone(),
            application.catalogs,
            application.wallet,
            config.server.enable_public_cors,
            concordium_wallet_proxy::blocks::routes(application.health.node.clone().into())
                .merge(concordium_wallet_proxy::progress::routes(
                    application.health.node.clone().into(),
                ))
                .merge(concordium_wallet_proxy::submission::routes(
                    application.health.node.clone().into(),
                ))
                .merge(concordium_wallet_proxy::fees::routes(
                    application.health.node.clone().into(),
                ))
                .merge(concordium_wallet_proxy::staking::routes(
                    application.health.node.clone().into(),
                ))
                .merge(concordium_wallet_proxy::discovery::routes(
                    application.health.history.clone(),
                ))
                .merge(concordium_wallet_proxy::accounts::routes(
                    application.health.node.clone().into(),
                ))
                .merge(concordium_wallet_proxy::network::routes(
                    application.network,
                ))
                .merge(concordium_wallet_proxy::balances::routes(
                    application.balances,
                )),
        ),
    )
    .with_graceful_shutdown(shutdown())
    .await?;
    Ok(())
}

async fn shutdown() {
    let interrupt = async {
        tokio::signal::ctrl_c()
            .await
            .expect("Cannot install interrupt handler")
    };
    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("Cannot install termination handler")
            .recv()
            .await;
    };
    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();
    tokio::select! { () = interrupt => {}, () = terminate => {} }
}
