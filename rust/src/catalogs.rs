//! This module serves the configured identity-provider catalogues.
//! Each API version returns its JSON value without changes to provider data.

use axum::{extract::State, routing::get, Json, Router};
use serde_json::Value;
use std::sync::Arc;

/// Build the three catalogue routes from the JSON values loaded at startup.
/// Array entries are in version order: 0, 1, then 2.
pub(crate) fn routes(catalogs: [Value; 3]) -> Router {
    Router::new()
        .route("/v0/ip_info", get(version_zero))
        .route("/v1/ip_info", get(version_one))
        .route("/v2/ip_info", get(version_two))
        .with_state(Arc::new(catalogs))
}

async fn version_zero(State(catalogs): State<Arc<[Value; 3]>>) -> Json<Value> {
    Json(catalogs[0].clone())
}

async fn version_one(State(catalogs): State<Arc<[Value; 3]>>) -> Json<Value> {
    Json(catalogs[1].clone())
}

async fn version_two(State(catalogs): State<Arc<[Value; 3]>>) -> Json<Value> {
    Json(catalogs[2].clone())
}
