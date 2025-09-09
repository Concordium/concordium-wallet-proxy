use crate::metrics_registry;
use axum::http::StatusCode;
use axum::{Json, Router, extract::State, routing};
use serde_json::json;

/// Router exposing the Prometheus metrics and health endpoint.
pub fn monitoring_router() -> anyhow::Result<Router> {
    let metric_routes = Router::new().route("/", routing::get(metrics));
    let health_state = HealthState {};
    let health_routes = Router::new()
        .route("/", routing::get(health))
        .with_state(health_state);
    Ok(Router::new()
        .nest("/metrics", metric_routes)
        .nest("/health", health_routes))
}

/// GET Handler for route `/metrics`.
/// Exposes the metrics in the registry in the Prometheus format.
async fn metrics() -> Result<String, String> {
    let mut buffer = String::new();
    prometheus_client::encoding::text::encode(&mut buffer, &metrics_registry::REGISTRY.lock())
        .map_err(|err| err.to_string())?;
    Ok(buffer)
}

/// Represents the state required by the health endpoint router.
///
/// This struct provides access to essential resources needed to determine
/// system health and readiness.
#[derive(Clone)]
struct HealthState {}

/// GET Handler for route `/health`.
/// Verifying the API service state is as expected.
async fn health(State(_state): State<HealthState>) -> (StatusCode, Json<serde_json::Value>) {
    // TODO: database check as part of COR-1809
    // TODO: node check as part of COR-1810

    let is_healthy = true;

    let status_code = if is_healthy {
        StatusCode::OK
    } else {
        StatusCode::INTERNAL_SERVER_ERROR
    };
    (status_code, Json(json!({})))
}
