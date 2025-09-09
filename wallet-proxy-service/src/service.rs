use crate::configuration::Cli;
use crate::monitoring_api;
use anyhow::Context;
use futures_util::TryFutureExt;
use prometheus_client::metrics;
use prometheus_client::registry::Registry;
use tokio::net::TcpListener;
use tokio_util::sync::CancellationToken;
use tokio_util::task::TaskTracker;
use tracing::{error, info};

pub async fn run_service(cli: Cli) -> anyhow::Result<()> {
    let service_info = metrics::info::Info::new([("version", clap::crate_version!().to_string())]);
    let mut metrics_registry = Registry::default();
    metrics_registry.register("service", "Information about the software", service_info);
    metrics_registry.register(
        "service_startup_timestamp_millis",
        "Timestamp of starting up the API service (Unix time in milliseconds)",
        metrics::gauge::ConstGauge::new(chrono::Utc::now().timestamp_millis()),
    );

    let cancel_token = CancellationToken::new();
    let queries_task = {
        let tcp_listener = TcpListener::bind(cli.listen)
            .await
            .context("Parsing TCP listener address failed")?;
        let stop_signal = cancel_token.child_token();
        info!("Server is running at {:?}", cli.listen);

        axum::serve(
            tcp_listener,
            axum::Router::new(), // TODO implement as part of COR-1810
        )
        .with_graceful_shutdown(stop_signal.cancelled_owned())
        .into_future()
    };

    let monitoring_task = {
        let tcp_listener = TcpListener::bind(cli.monitoring_listen)
            .await
            .context("Parsing TCP listener address failed")?;
        let stop_signal = cancel_token.child_token();
        info!(
            "Monitoring server is running at {:?}",
            cli.monitoring_listen
        );
        let monitoring_router = monitoring_api::monitoring_router(metrics_registry)?;
        axum::serve(tcp_listener, monitoring_router)
            .with_graceful_shutdown(stop_signal.cancelled_owned())
            .into_future()
    };

    let cancel_token_clone = cancel_token.clone();
    tokio::spawn({
        async move {
            tokio::signal::ctrl_c().await.ok();
            info!("Received signal to shutdown");
            cancel_token_clone.cancel();
        }
    });

    let task_tracker = TaskTracker::new();
    let cancel_token_clone = cancel_token.clone();
    task_tracker.spawn(queries_task.inspect_err(move |err| {
        error!("REST API server error: {}", err);
        cancel_token_clone.cancel();
    }));

    let cancel_token_clone = cancel_token.clone();
    task_tracker.spawn(monitoring_task.inspect_err(move |err| {
        error!("Monitoring server error: {}", err);
        cancel_token_clone.cancel();
    }));

    task_tracker.close();
    task_tracker.wait().await;

    info!("Service is shut down");

    Ok(())
}
