use crate::configuration::Cli;
use crate::{monitoring_api};
use anyhow::Context;
use prometheus_client::metrics;
use prometheus_client::registry::Registry;
use tokio::net::TcpListener;
use tokio_util::sync::CancellationToken;
use tracing::{error, info};

pub async fn run_service(cli: Cli) -> anyhow::Result<()> {
    let service_info = metrics::info::Info::new([("version", clap::crate_version!().to_string())]);
    let mut metrics_registry= Registry::default();
    metrics_registry.register(
        "service",
        "Information about the software",
        service_info,
    );
    metrics_registry.register(
        "service_startup_timestamp_millis",
        "Timestamp of starting up the API service (Unix time in milliseconds)",
        metrics::gauge::ConstGauge::new(chrono::Utc::now().timestamp_millis()),
    );

    let cancel_token = CancellationToken::new();
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
        let monitoring_router = monitoring_api::monitoring_router(metrics_registry)?;
        tokio::spawn(async move {
            axum::serve(tcp_listener, monitoring_router)
                .with_graceful_shutdown(stop_signal.cancelled_owned())
                .await
        })
    };

    // Await for signal to shut down or any of the tasks to stop.
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
