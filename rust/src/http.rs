//! This module serves health and catalogue routes and formats HTTP errors.
//! It applies path redirects, response headers, and optional public CORS handling.

use crate::{
    catalogs,
    health::{Health, HealthError},
    node::NodeFailure,
};
use axum::{
    body::Body,
    extract::{Request, State},
    http::{header, HeaderValue, Method, StatusCode},
    middleware::{self, Next},
    response::{IntoResponse, Response},
    routing::get,
    Json, Router,
};
use serde_json::{json, Value};
use tower_http::trace::TraceLayer;

/// Build the health and catalogue routes with the existing error and header formats.
/// Enable public CORS handling only when `public_cors` is true.
pub fn router(health: Health, catalogs: [Value; 3], public_cors: bool) -> Router {
    Router::new()
        .route("/v0/health", get(check_health))
        .with_state(health)
        .merge(catalogs::routes(catalogs))
        .fallback(|| async { error(StatusCode::NOT_FOUND, 1, "Not found") })
        .method_not_allowed_fallback(|| async {
            error(StatusCode::METHOD_NOT_ALLOWED, 1, "Bad method")
        })
        .layer(middleware::from_fn_with_state(public_cors, compatibility))
        .layer(TraceLayer::new_for_http())
}

async fn check_health(State(health): State<Health>) -> Response {
    match health.check().await {
        Ok(value) => Json(value).into_response(),
        Err(err) => {
            tracing::error!(error = %err, "Health query failed");
            match err {
                HealthError::Node(NodeFailure::Transport) => error(
                    StatusCode::BAD_GATEWAY,
                    0,
                    "Cannot communicate with node via GRPC endpoint.",
                ),
                _ => error(
                    StatusCode::INTERNAL_SERVER_ERROR,
                    0,
                    "Internal server error",
                ),
            }
        }
    }
}

/// Format an error in English and set the response language header.
/// English is the only supported language, regardless of the requested language.
fn error(status: StatusCode, code: u8, message: &str) -> Response {
    let mut response = (
        status,
        Json(json!({"error": code, "errorMessage": message})),
    )
        .into_response();
    response
        .headers_mut()
        .insert(header::CONTENT_LANGUAGE, HeaderValue::from_static("en"));
    response
}

async fn compatibility(State(public_cors): State<bool>, request: Request, next: Next) -> Response {
    let cors = public_cors
        && request.uri().path() != "/v0/transakOnRamp"
        && request.headers().contains_key(header::ORIGIN);
    let preflight = cors
        && request.method() == Method::OPTIONS
        && request
            .headers()
            .contains_key(header::ACCESS_CONTROL_REQUEST_METHOD);
    let allowed = request
        .headers()
        .get(header::ACCESS_CONTROL_REQUEST_METHOD)
        .is_some_and(|method| matches!(method.as_bytes(), b"GET" | b"POST" | b"PUT" | b"OPTIONS"));
    let requested_headers = request
        .headers()
        .get(header::ACCESS_CONTROL_REQUEST_HEADERS)
        .cloned();
    let mut response = if preflight && allowed {
        Response::new(Body::empty())
    } else {
        let path = request.uri().path();
        let clean = format!(
            "/{}",
            path.split('/')
                .filter(|part| !part.is_empty())
                .collect::<Vec<_>>()
                .join("/")
        );
        if clean != path {
            let location = match request.uri().query() {
                Some(query) => format!("{clean}?{query}"),
                None => clean,
            };
            match HeaderValue::from_str(&location) {
                Ok(location) => (
                    StatusCode::MOVED_PERMANENTLY,
                    [(header::LOCATION, location)],
                )
                    .into_response(),
                Err(_) => error(StatusCode::BAD_REQUEST, 1, "Invalid arguments"),
            }
        } else {
            next.run(request).await
        }
    };
    if cors && (!preflight || allowed) {
        response.headers_mut().insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static("*"),
        );
        if preflight {
            response.headers_mut().insert(
                header::ACCESS_CONTROL_ALLOW_METHODS,
                HeaderValue::from_static("GET, POST, PUT, OPTIONS"),
            );
            if let Some(headers) = requested_headers {
                response
                    .headers_mut()
                    .insert(header::ACCESS_CONTROL_ALLOW_HEADERS, headers);
            }
        }
    }
    response.headers_mut().insert(
        header::SERVER,
        HeaderValue::from_static("Concordium-wallet-proxy"),
    );
    if response
        .headers()
        .get(header::CONTENT_TYPE)
        .is_some_and(|v| v == "application/json")
    {
        response.headers_mut().insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/json; charset=utf-8"),
        );
    }
    response
}
