//! Module containing the implementation of a service providing the public
//! facing REST API for `ccdscan-api`.

use crate::graphql_api::{AccountStatementEntryType, ApiServiceConfig};
use axum::{
    extract::{Query, State},
    http::HeaderName,
    response::{AppendHeaders, IntoResponse},
    routing::get,
    Router,
};
use chrono::{DateTime, TimeDelta, Utc};
use concordium_rust_sdk::{common::types::Amount, id::types::AccountAddress};
use futures::TryStreamExt as _;
use prometheus_client::registry::Registry;
use reqwest::StatusCode;
use sqlx::PgPool;
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};
use tracing::error;

/// Service providing the router for the REST API.
#[derive(Debug)]
pub struct Service {
    /// Layer adding monitoring for the routes.
    monitor_layer: monitor::MonitorLayer,
    /// State shared between handlers.
    state: RouterState,
}

/// State shared between handlers.
#[derive(Debug, Clone)]
struct RouterState {
    /// Database connection pool.
    pool: PgPool,
    /// Configurations for the API.
    config: Arc<ApiServiceConfig>,
}

impl Service {
    pub fn new(pool: PgPool, config: Arc<ApiServiceConfig>, registry: &mut Registry) -> Self {
        Self {
            state: RouterState { pool, config },
            monitor_layer: monitor::MonitorLayer::new(registry.sub_registry_with_prefix("rest")),
        }
    }

    pub fn as_router(self) -> Router {
        let cors_layer = CorsLayer::new()
            .allow_origin(Any) // Open access to selected route
            .allow_methods(Any)
            .allow_headers(Any);
        Router::new()
            .route(
                "/rest/balance-statistics/latest",
                get(Self::latest_balance_statistics),
            )
            .route(
                "/rest/export/statement",
                get(Self::export_account_statements),
            )
            .layer(cors_layer)
            .layer(self.monitor_layer)
            .with_state(self.state)
    }

    async fn latest_balance_statistics(
        Query(params): Query<LatestBalanceStatistics>,
        State(state): State<RouterState>,
    ) -> ApiResult<String> {
        let amount = match params.field {
            // deprecated: please use 'totalAmountUnlocked' going forward over 'totalAmounUnlocked'
            // as 'totalAmounUnlocked' is now deprecated and will be removed in a future release
            Balance::TotalAmount | Balance::TotalAmountUnlocked | Balance::TotalAmounUnlocked => {
                sqlx::query_scalar!("SELECT total_amount FROM blocks ORDER BY height DESC LIMIT 1")
                    .fetch_optional(&state.pool)
                    .await?
            }
            Balance::TotalAmountCirculating => {
                let non_circulating_accounts = state
                    .config
                    .non_circulating_account
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>();
                sqlx::query_scalar!(
                    "WITH non_circulating_accounts AS (
                         SELECT
                             COALESCE(SUM(amount), 0)::BIGINT AS total_amount
                         FROM accounts
                         WHERE address = ANY($1)
                     )
                     SELECT
                         (blocks.total_amount
                             - non_circulating_accounts.total_amount)::BIGINT
                     FROM blocks, non_circulating_accounts
                     ORDER BY height DESC
                     LIMIT 1",
                    &non_circulating_accounts
                )
                .fetch_one(&state.pool)
                .await?
            }
        };
        let amount = u64::try_from(amount.ok_or(ApiError::NotFound)?)?;
        let amount = match params.unit {
            Unit::Ccd => amount / 1_000_000,
            Unit::MicroCcd => amount,
        };
        Ok(amount.to_string())
    }

    async fn export_account_statements(
        Query(params): Query<ExportAccountStatement>,
        State(state): State<RouterState>,
    ) -> ApiResult<(AppendHeaders<[(HeaderName, String); 2]>, String)> {
        let to = params.to_time.unwrap_or_else(Utc::now);
        let account_statements_export_max_days =
            i64::try_from(state.config.export_statement_max_days).unwrap_or(32);
        let from = params
            .from_time
            .unwrap_or_else(|| to - TimeDelta::days(account_statements_export_max_days));
        if to - from > TimeDelta::days(account_statements_export_max_days) {
            return Err(ApiError::ExceedsMaxAllowedDaysForAccountStatementExport(
                account_statements_export_max_days,
            ));
        }

        let mut rows = sqlx::query_as!(
            ExportAccountStatementEntry,
            r#"SELECT
                slot_time as timestamp,
                amount,
                account_balance,
                "entry_type" as "entry_type: AccountStatementEntryType"
            FROM account_statements
            WHERE
                account_index = (SELECT index FROM accounts WHERE address = $1)
                AND slot_time between $2 and $3
            ORDER BY slot_time DESC"#,
            params.account_address.to_string(),
            from,
            to
        )
        .fetch(&state.pool);
        let mut csv = String::from("Time,Amount (CCD),Balance (CCD),Label\n");
        while let Some(row) = rows.try_next().await? {
            let account_balance = Amount::from_micro_ccd(row.account_balance.try_into()?);
            let amount_sign = if row.amount.is_negative() { "-" } else { "" };
            let amount = Amount::from_micro_ccd(row.amount.abs().try_into()?);
            csv.push_str(
                format!(
                    "{},{}{},{},{}\n",
                    row.timestamp
                        .to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                    amount_sign,
                    amount,
                    account_balance,
                    row.entry_type
                )
                .as_str(),
            )
        }
        let filename = format!(
            "statement-{}_{}-{}.csv",
            params.account_address,
            from.to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
            to.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
        );
        let headers = AppendHeaders([
            (
                axum::http::header::CONTENT_TYPE,
                "text/csv; charset=utf-8".to_string(),
            ),
            (
                axum::http::header::CONTENT_DISPOSITION,
                format!("attachment; filename=\"{}\"", filename),
            ),
        ]);
        Ok((headers, csv))
    }
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExportAccountStatement {
    account_address: AccountAddress,
    from_time: Option<DateTime<Utc>>,
    to_time: Option<DateTime<Utc>>,
}

struct ExportAccountStatementEntry {
    timestamp: DateTime<Utc>,
    amount: i64,
    account_balance: i64,
    entry_type: AccountStatementEntryType,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
struct LatestBalanceStatistics {
    field: Balance,
    #[serde(default = "default_balance_statistics_unit_microccd")]
    unit: Unit,
}

/// default unit for balance statistics to support backwards compatibility for
/// callers
fn default_balance_statistics_unit_microccd() -> Unit {
    Unit::MicroCcd
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
#[allow(clippy::enum_variant_names)]
enum Balance {
    /// deprecated alias to support backwards compatibility for old callers to
    /// .NET backend - total amount can be provided as 'TotalAmount'
    #[serde(alias = "TotalAmount")]
    TotalAmount,
    /// deprecated alias to support backwards compatibility for old callers to
    /// .NET backend - TotalAmountReleased is equivalent to
    /// TotalAmountCirculating
    #[serde(alias = "TotalAmountReleased")]
    TotalAmountCirculating,
    TotalAmountUnlocked,
    TotalAmounUnlocked, /* deprecated: please use 'totalAmountUnlocked' going forward over
                         * 'totalAmounUnlocked' as 'totalAmounUnlocked' is now deprecated and
                         * should be removed when no more activity is registered for this field */
}
#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
enum Unit {
    Ccd,
    MicroCcd,
}

#[derive(Debug, thiserror::Error, Clone)]
enum ApiError {
    #[error("Information was not found.")]
    NotFound,
    #[error("Chosen time span exceeds max allowed days for account statement export: {0}")]
    ExceedsMaxAllowedDaysForAccountStatementExport(i64),
    #[error("Internal error (FailedDatabaseQuery): {0}")]
    FailedDatabaseQuery(Arc<sqlx::Error>),
    #[error("Invalid integer: {0}")]
    InvalidInt(#[from] std::num::TryFromIntError),
}
impl From<sqlx::Error> for ApiError {
    fn from(value: sqlx::Error) -> Self {
        ApiError::FailedDatabaseQuery(Arc::new(value))
    }
}

type ApiResult<A> = Result<A, ApiError>;

impl IntoResponse for ApiError {
    fn into_response(self) -> axum::response::Response {
        let status = match self {
            ApiError::ExceedsMaxAllowedDaysForAccountStatementExport(_) => StatusCode::BAD_REQUEST,
            ApiError::FailedDatabaseQuery(_) => StatusCode::INTERNAL_SERVER_ERROR,
            ApiError::InvalidInt(_) => StatusCode::INTERNAL_SERVER_ERROR,
            ApiError::NotFound => StatusCode::NOT_FOUND,
        };
        (status, self.to_string()).into_response()
    }
}

mod monitor {
    use axum::http::{Request, Response};
    use prometheus_client::{
        encoding::EncodeLabelSet,
        metrics::{family::Family, histogram},
        registry::Registry,
    };
    use std::{future::Future, pin::Pin, task};

    /// tower layer adding monitoring to a service.
    #[derive(Debug, Clone)]
    pub struct MonitorLayer {
        /// Metric tracking the response status code and response duration.
        requests: Family<QueryLabels, histogram::Histogram>,
    }
    impl MonitorLayer {
        pub fn new(registry: &mut Registry) -> Self {
            let requests: Family<QueryLabels, _> = Family::new_with_constructor(|| {
                histogram::Histogram::new(histogram::exponential_buckets(0.010, 2.0, 10))
            });
            registry.register(
                "request_duration_seconds",
                "Duration of seconds for responding to requests for the separate REST API",
                requests.clone(),
            );
            Self { requests }
        }
    }

    impl<S> tower::Layer<S> for MonitorLayer {
        type Service = MonitorService<S>;

        fn layer(&self, inner: S) -> Self::Service {
            MonitorService {
                inner,
                metrics: self.clone(),
            }
        }
    }

    /// Service middleware tracking metrics.
    #[derive(Debug, Clone)]
    pub struct MonitorService<S> {
        /// The inner service.
        inner: S,
        /// The metrics being tracked.
        metrics: MonitorLayer,
    }

    /// Type representing the Prometheus labels used for metrics related to
    /// queries to the REST API.
    #[derive(Debug, Clone, EncodeLabelSet, PartialEq, Eq, Hash)]
    struct QueryLabels {
        /// Path in the request.
        path: String,
        /// Query parameters after the path in the request.
        params: Option<String>,
        /// The response status code.
        status: Option<u16>,
    }

    impl<S, R, ResBody, F> tower::Service<Request<R>> for MonitorService<S>
    where
        S: tower::Service<Request<R>, Response = Response<ResBody>, Future = F>,
        F: Future<Output = Result<S::Response, S::Error>> + 'static + Send,
    {
        type Error = S::Error;
        type Future = Pin<Box<dyn Future<Output = Result<S::Response, S::Error>> + Send>>;
        type Response = S::Response;

        fn poll_ready(
            &mut self,
            cx: &mut task::Context<'_>,
        ) -> task::Poll<Result<(), Self::Error>> {
            self.inner.poll_ready(cx)
        }

        fn call(&mut self, req: Request<R>) -> Self::Future {
            let start = tokio::time::Instant::now();
            let uri = req.uri();
            let endpoint = String::from(uri.path());
            let params = uri.query().map(|q| q.to_owned());
            let inner_fut = self.inner.call(req);
            let requests = self.metrics.requests.clone();
            Box::pin(async move {
                let res = inner_fut.await;
                let duration = start.elapsed();
                let status = if let Ok(response) = &res {
                    Some(u16::from(response.status()))
                } else {
                    None
                };
                requests
                    .get_or_create(&QueryLabels {
                        path: endpoint,
                        params,
                        status,
                    })
                    .observe(duration.as_secs_f64());
                res
            })
        }
    }
}
