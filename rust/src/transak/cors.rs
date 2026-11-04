//! This module applies the Transak-only browser-origin policy.
//! Failed CORS checks do not block requests; they omit permission headers, as in the legacy middleware.

use axum::{
    body::Body,
    extract::{Request, State},
    http::{HeaderValue, Method, header},
    middleware::Next,
    response::Response,
};
use std::sync::Arc;

pub(super) async fn apply(
    State(origins): State<Arc<Vec<String>>>,
    request: Request,
    next: Next,
) -> Response {
    let origin = request
        .headers()
        .get(header::ORIGIN)
        .filter(|origin| {
            origins
                .iter()
                .any(|allowed| origin.as_bytes() == allowed.as_bytes())
        })
        .cloned();
    let preflight = request.method() == Method::OPTIONS
        && request
            .headers()
            .contains_key(header::ACCESS_CONTROL_REQUEST_METHOD);
    let allowed_method = request
        .headers()
        .get(header::ACCESS_CONTROL_REQUEST_METHOD)
        .is_some_and(|method| matches!(method.as_bytes(), b"POST" | b"OPTIONS"));
    let allowed_headers = request
        .headers()
        .get(header::ACCESS_CONTROL_REQUEST_HEADERS)
        .is_none_or(|headers| {
            std::str::from_utf8(headers.as_bytes()).is_ok_and(|headers| {
                headers
                    .split(',')
                    .all(|name| name.trim().eq_ignore_ascii_case("content-type"))
            })
        });
    let allowed = origin.is_some() && (!preflight || (allowed_method && allowed_headers));
    let mut response = if preflight && allowed {
        Response::new(Body::empty())
    } else {
        next.run(request).await
    };
    if allowed {
        response
            .headers_mut()
            .insert(header::ACCESS_CONTROL_ALLOW_ORIGIN, origin.unwrap());
        response
            .headers_mut()
            .append(header::VARY, HeaderValue::from_static("Origin"));
        if preflight {
            response.headers_mut().insert(
                header::ACCESS_CONTROL_ALLOW_METHODS,
                HeaderValue::from_static("POST, OPTIONS"),
            );
            response.headers_mut().insert(
                header::ACCESS_CONTROL_ALLOW_HEADERS,
                HeaderValue::from_static("Content-Type"),
            );
        }
    }
    response
}
