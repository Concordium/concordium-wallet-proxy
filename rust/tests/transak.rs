//! This module checks Transak requests against a controlled local HTTP service.
//! Credentials and target addresses are synthetic; no real purchase session is created.

mod config;
mod http;
mod refresh;

use axum::{
    Router,
    body::{Body, to_bytes},
    extract::{Request, State},
    http::{HeaderMap, StatusCode},
    response::IntoResponse,
    routing::post,
};
use chrono::{DateTime, Utc};
use concordium_rust_sdk::id::types::AccountAddress;
use concordium_wallet_proxy::{
    health::Clock,
    transak::{
        self, Transak,
        config::{Endpoints, Settings, UserIpSource},
    },
};
use serde_json::{Value, json};
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
    time::Duration,
};
use tower::ServiceExt;

/// This address is used only in synthetic session requests.
const ADDRESS: AccountAddress = AccountAddress([7; 32]);
/// This fixed URI intentionally retains letter case and percent escapes.
const WIDGET: &str = "https://EXAMPLE.invalid/buy?session=a%2fb#Step";

/// This source controls wall-clock token-expiry comparisons.
#[derive(Clone)]
struct TestClock(
    /// This shared time can change independently from Tokio's timeout clock.
    Arc<Mutex<DateTime<Utc>>>,
);
impl Default for TestClock {
    fn default() -> Self {
        Self(Arc::new(Mutex::new(
            DateTime::from_timestamp(1000, 0).unwrap(),
        )))
    }
}
impl Clock for TestClock {
    fn now(&self) -> DateTime<Utc> {
        *self.0.lock().unwrap()
    }
}
impl TestClock {
    fn set(&self, seconds: i64, nanos: u32) {
        *self.0.lock().unwrap() = DateTime::from_timestamp(seconds, nanos).unwrap();
    }
}

/// This response controls an upstream status, JSON body, and optional delay.
struct Reply {
    /// This status is returned unchanged by the local server.
    status: u16,
    /// These raw bytes permit malformed and duplicate JSON tests.
    body: String,
    /// This delay exercises refresh locking and cancellation.
    delay: Duration,
}
impl Reply {
    fn json(value: Value) -> Self {
        Self {
            status: 200,
            body: value.to_string(),
            delay: Duration::ZERO,
        }
    }
    fn status(status: u16) -> Self {
        Self {
            status,
            body: "upstream-secret-error".into(),
            delay: Duration::ZERO,
        }
    }
}
/// This record retains the exact outbound request contract.
struct Call {
    /// This path selects refresh or session creation.
    path: String,
    /// These headers include partner authentication and user-IP fields.
    headers: HeaderMap,
    /// This value retains the complete JSON request body.
    body: Value,
}
/// This state supplies queued HTTP responses and records every operation.
#[derive(Default)]
struct Upstream {
    /// These responses replace refresh successes in queue order.
    refresh: Mutex<VecDeque<Reply>>,
    /// These responses replace session successes in queue order.
    sessions: Mutex<VecDeque<Reply>>,
    /// These calls expose ordering and accidental retries.
    calls: Mutex<Vec<Call>>,
}
impl Upstream {
    fn count(&self, path: &str) -> usize {
        self.calls
            .lock()
            .unwrap()
            .iter()
            .filter(|call| call.path == path)
            .count()
    }
    async fn wait(&self, path: &str, count: usize) {
        tokio::time::timeout(Duration::from_secs(2), async {
            while self.count(path) < count {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
    }
}
async fn handle(State(upstream): State<Arc<Upstream>>, request: Request) -> impl IntoResponse {
    let path = request.uri().path().to_owned();
    let headers = request.headers().clone();
    let body =
        serde_json::from_slice(&to_bytes(request.into_body(), usize::MAX).await.unwrap()).unwrap();
    let reply = if path == "/refresh" {
        upstream
            .refresh
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or_else(|| {
                Reply::json(json!({"data":{"accessToken":"synthetic-token","expiresAt":2000}}))
            })
    } else {
        upstream
            .sessions
            .lock()
            .unwrap()
            .pop_front()
            .unwrap_or_else(|| Reply::json(json!({"data":{"widgetUrl":WIDGET}})))
    };
    upstream.calls.lock().unwrap().push(Call {
        path,
        headers,
        body,
    });
    tokio::time::sleep(reply.delay).await;
    (StatusCode::from_u16(reply.status).unwrap(), reply.body)
}
fn settings(source: UserIpSource) -> Settings {
    Settings {
        use_staging: false,
        api_key: "synthetic-key".into(),
        api_secret: "synthetic-secret".into(),
        referrer_domain: "wallet.example".into(),
        user_ip_source: source,
        allowed_origins: vec![
            "https://wallet.example".into(),
            "http://localhost:8080".into(),
        ],
    }
}
async fn service(
    upstream: Arc<Upstream>,
    clock: TestClock,
    source: UserIpSource,
) -> (crate::support::Server, Arc<Transak<TestClock>>) {
    let server = crate::support::serve_router(
        Router::new()
            .route("/refresh", post(handle))
            .route("/session", post(handle))
            .with_state(upstream),
    )
    .await;
    let endpoints = Endpoints {
        refresh: format!("{}/refresh", server.endpoint),
        session: format!("{}/session", server.endpoint),
    };
    let service = Arc::new(Transak::new(settings(source), endpoints, clock).unwrap());
    (server, service)
}
async fn request(
    router: Router,
    query: &str,
    headers: &[(&str, &str)],
) -> axum::response::Response {
    let mut request = Request::builder()
        .method("POST")
        .uri(format!("/v0/transakOnRamp{query}"));
    for (name, value) in headers {
        request = request.header(*name, *value);
    }
    router
        .oneshot(request.body(Body::from("not-json-and-ignored")).unwrap())
        .await
        .unwrap()
}
async fn value(response: axum::response::Response) -> (u16, Value) {
    let status = response.status().as_u16();
    (
        status,
        serde_json::from_slice(&to_bytes(response.into_body(), usize::MAX).await.unwrap()).unwrap(),
    )
}

#[tokio::test]
async fn outbound_contracts_and_original_widget_uri_are_preserved() {
    let upstream = Arc::new(Upstream::default());
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    assert!(
        upstream.calls.lock().unwrap().is_empty(),
        "Startup must not contact Transak"
    );
    assert_eq!(
        service.create("203.0.113.4", ADDRESS).await.unwrap(),
        WIDGET
    );
    let calls = upstream.calls.lock().unwrap();
    assert_eq!(calls.len(), 2);
    assert_eq!(calls[0].path, "/refresh");
    assert_eq!(calls[0].body, json!({"apiKey":"synthetic-key"}));
    assert_eq!(calls[0].headers["api-secret"], "synthetic-secret");
    assert_eq!(calls[1].path, "/session");
    assert_eq!(calls[1].headers["access-token"], "synthetic-token");
    assert_eq!(calls[1].headers["x-user-ip"], "203.0.113.4");
    assert!(!calls[1].headers.contains_key("api-secret"));
    assert_eq!(
        calls[1].body,
        json!({"widgetParams":{"apiKey":"synthetic-key","cryptoCurrencyCode":"CCD","referrerDomain":"wallet.example","walletAddress":ADDRESS,"disableWalletAddressForm":true}})
    );
    for call in calls.iter() {
        assert_eq!(call.headers["accept"], "application/json");
        assert_eq!(call.headers["content-type"], "application/json");
        assert_eq!(call.headers["x-api-key"], "synthetic-key");
    }
}
