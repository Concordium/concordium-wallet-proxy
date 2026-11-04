//! This module validates purchase-session requests and selects the end-user IP.
//! The account is a query parameter; request bodies are not used.

use super::{SessionFailure, Transak, config::UserIpSource};
use crate::{health::Clock, http::error};
use axum::{
    Json, Router,
    extract::{ConnectInfo, Query, Request, State},
    http::StatusCode,
    middleware,
    response::{IntoResponse, Response},
    routing::post,
};
use std::{
    net::{IpAddr, SocketAddr},
    sync::Arc,
};

/// Build the optional POST/OPTIONS route with its independent origin policy.
pub fn routes<C: Clock + 'static>(service: Option<Arc<Transak<C>>>) -> Router {
    let origins = Arc::new(
        service
            .as_ref()
            .map(|service| service.settings.allowed_origins.clone())
            .unwrap_or_default(),
    );
    Router::new()
        .route("/v0/transakOnRamp", post(create::<C>).options(options::<C>))
        .with_state(service)
        .layer(middleware::from_fn_with_state(origins, super::cors::apply))
}
async fn options<C: Clock>(State(service): State<Option<Arc<Transak<C>>>>) -> Response {
    if service.is_some() {
        "".into_response()
    } else {
        disabled()
    }
}
async fn create<C: Clock>(
    State(service): State<Option<Arc<Transak<C>>>>,
    request: Request,
) -> Response {
    let Some(service) = service else {
        return disabled();
    };
    let Ok(Query(query)) = Query::<Vec<(String, String)>>::try_from_uri(request.uri()) else {
        return error(StatusCode::BAD_REQUEST, 1, "Invalid arguments");
    };
    let Some((_, address)) = query.iter().find(|(key, _)| key == "address") else {
        return error(
            StatusCode::BAD_REQUEST,
            1,
            "Parsing input failed: 'address' parameter is required to be an account address",
        );
    };
    let Ok(address) = crate::compatibility::input::account(address) else {
        return error(
            StatusCode::BAD_REQUEST,
            1,
            "Parsing input failed: 'address' is not a valid account address",
        );
    };
    let Some(ip) = user_ip(service.settings.user_ip_source, &request) else {
        tracing::warn!("Cannot determine Transak end-user IP");
        return error(
            StatusCode::INTERNAL_SERVER_ERROR,
            0,
            "Internal server error",
        );
    };
    match service.create(&ip, address).await {
        Ok(url) => Json(serde_json::json!({"widgetUrl":url})).into_response(),
        Err(SessionFailure::BadGateway) => {
            tracing::warn!("Transak purchase-session request failed");
            error(
                StatusCode::BAD_GATEWAY,
                0,
                "Invalid response from upstream server.",
            )
        }
        Err(SessionFailure::Timeout) => {
            tracing::warn!("Transak purchase-session request timed out");
            error(
                StatusCode::GATEWAY_TIMEOUT,
                0,
                "Timeout occurred waiting for response from upstream server.",
            )
        }
    }
}
fn disabled() -> Response {
    error(StatusCode::NOT_FOUND, 2, "Not found")
}
fn user_ip(source: UserIpSource, request: &Request) -> Option<String> {
    match source {
        UserIpSource::XForwardedFor => {
            let first = std::str::from_utf8(request.headers().get("x-forwarded-for")?.as_bytes())
                .ok()?
                .split(',')
                .next()?
                .trim();
            (!first.is_empty()).then(|| first.to_owned())
        }
        UserIpSource::RemoteAddress => {
            let ConnectInfo(address) = request.extensions().get::<ConnectInfo<SocketAddr>>()?;
            Some(match address.ip() {
                IpAddr::V4(ip) => ip.to_string(),
                // Haskell renders all eight groups without zero compression or leading zeroes.
                IpAddr::V6(ip) => ip
                    .segments()
                    .iter()
                    .map(|group| format!("{group:x}"))
                    .collect::<Vec<_>>()
                    .join(":"),
            })
        }
    }
}
