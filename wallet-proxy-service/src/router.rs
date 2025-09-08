use axum::{extract::State, routing::get, Router};
use prometheus_client::registry::Registry;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio_util::sync::CancellationToken;

/// Run server exposing the Prometheus metrics and health endpoint.
pub async fn serve(
    registry: Registry,
    tcp_listener: TcpListener,
    stop_signal: CancellationToken,
    health_routes: Router,
) -> anyhow::Result<()> {
    let metric_routes = Router::new()
        .route("/", get(metrics))
        .with_state(Arc::new(registry));
    let app = Router::new()
        .nest("/metrics", metric_routes)
        .nest("/health", health_routes);
    axum::serve(tcp_listener, app)
        .with_graceful_shutdown(stop_signal.cancelled_owned())
        .await?;
    Ok(())
}

/// GET Handler for route `/metrics`.
/// Exposes the metrics in the registry in the Prometheus format.
async fn metrics(State(registry): State<Arc<Registry>>) -> Result<String, String> {
    let mut buffer = String::new();
    prometheus_client::encoding::text::encode(&mut buffer, &registry)
        .map_err(|err| err.to_string())?;
    Ok(buffer)
}
