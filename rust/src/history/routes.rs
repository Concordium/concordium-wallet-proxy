//! This module serves all pre-token versions of account transaction history.
//! Canonical account resolution precedes filter validation and database selection.

use super::{AccountHistory, HistoryNode, HistoryOrder, HistoryVersion, request::Request, view};
use crate::http::error;
use axum::{
    Json, Router,
    extract::{Path, Query, State},
    http::{StatusCode, header},
    response::{IntoResponse, Response},
    routing::get,
};
use std::sync::Arc;

/// Build the v0, v1, and v2 account-history routes with shared adapters.
pub fn routes<N: HistoryNode + 'static, H: AccountHistory + 'static>(
    node: Arc<N>,
    history: Arc<H>,
) -> Router {
    Router::new()
        .route(
            "/v0/accTransactions/{address}",
            get(history_response::<N, H, 0>),
        )
        .route(
            "/v1/accTransactions/{address}",
            get(history_response::<N, H, 1>),
        )
        .route(
            "/v2/accTransactions/{address}",
            get(history_response::<N, H, 2>),
        )
        .with_state((node, history))
}

async fn history_response<N: HistoryNode, H: AccountHistory, const VERSION: u8>(
    State((node, history)): State<(Arc<N>, Arc<H>)>,
    Path(address): Path<String>,
    Query(values): Query<Vec<(String, String)>>,
) -> Response {
    let Ok(address) = address.parse() else {
        return error(StatusCode::BAD_REQUEST, 1, "Malformed account address");
    };
    let address = match node.canonical_address(address).await {
        Ok(address) => address,
        Err(failure) if failure.is_not_found() => address,
        Err(failure) => return failure.response(),
    };
    let version = match VERSION {
        0 => HistoryVersion::V0,
        1 => HistoryVersion::V1,
        _ => HistoryVersion::V2,
    };
    let request = match Request::parse(version, &values) {
        Ok(request) => request,
        Err(message) => {
            return error(
                StatusCode::BAD_REQUEST,
                1,
                &format!("Parsing input failed: {message}"),
            );
        }
    };
    let entries = match history.account_history(address, &request.query).await {
        Ok(entries) => entries,
        Err(failure) => {
            tracing::warn!(%failure, "Account-history query failed");
            return error(
                StatusCode::INTERNAL_SERVER_ERROR,
                0,
                "Internal server error",
            );
        }
    };
    let transactions = match view::page(entries, address, version, request.raw_rejection) {
        Ok(transactions) => transactions,
        Err(failure) => {
            tracing::warn!(%failure, "Cannot decode legacy account history");
            return error(StatusCode::BAD_GATEWAY, 0, "Database error");
        }
    };
    let mut body = serde_json::json!({"limit":request.query.limit,"order":match request.query.order { HistoryOrder::Ascending => "ascending", HistoryOrder::Descending => "descending" },"count":transactions.len(),"transactions":transactions});
    if let Some(from) = request.query.from {
        body["from"] = serde_json::json!(from);
    }
    ([(header::CONTENT_LANGUAGE, "en")], Json(body)).into_response()
}
