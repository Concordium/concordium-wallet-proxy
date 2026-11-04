//! This module checks refresh coordination, one-time retries, and cancellation safety.
//! Time changes are explicit and tests never wait ten real seconds.

use super::*;
use concordium_wallet_proxy::transak::SessionFailure;

#[tokio::test]
async fn concurrent_requests_share_one_refresh_without_serializing_sessions() {
    let upstream = Arc::new(Upstream::default());
    let mut reply = Reply::json(json!({"data":{"accessToken":"shared","expiresAt":2000}}));
    reply.delay = Duration::from_millis(20);
    upstream.refresh.lock().unwrap().push_back(reply);
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    let mut tasks = Vec::new();
    for _ in 0..12 {
        let service = service.clone();
        tasks.push(tokio::spawn(async move {
            service.create("203.0.113.1", ADDRESS).await.unwrap()
        }));
    }
    for task in tasks {
        assert_eq!(task.await.unwrap(), WIDGET);
    }
    assert_eq!(upstream.count("/refresh"), 1);
    assert_eq!(upstream.count("/session"), 12);
}

#[tokio::test]
async fn refresh_margin_is_strict_and_preserves_fractional_expiry() {
    let upstream = Arc::new(Upstream::default());
    upstream.refresh.lock().unwrap().push_back(Reply {
        status: 200,
        body: "{\"data\":{\"accessToken\":\"fractional\",\"expiresAt\":1060.000000001}}".into(),
        delay: Duration::ZERO,
    });
    let clock = TestClock::default();
    let (_server, service) =
        service(upstream.clone(), clock.clone(), UserIpSource::XForwardedFor).await;
    service.create("ip", ADDRESS).await.unwrap();
    clock.set(1000, 1);
    service.create("ip", ADDRESS).await.unwrap();
    assert_eq!(
        upstream.count("/refresh"),
        1,
        "Equality at now + 60 is still valid"
    );
    clock.set(1000, 2);
    service.create("ip", ADDRESS).await.unwrap();
    assert_eq!(upstream.count("/refresh"), 2);
    let calls = upstream.calls.lock().unwrap();
    let tokens: Vec<_> = calls
        .iter()
        .filter(|call| call.path == "/session")
        .map(|call| call.headers["access-token"].to_str().unwrap())
        .collect();
    assert_eq!(tokens, ["fractional", "fractional", "synthetic-token"]);
}

#[tokio::test]
async fn unauthorized_session_retries_once_without_forcing_refresh() {
    for second in [200, 401, 503] {
        let upstream = Arc::new(Upstream::default());
        upstream
            .sessions
            .lock()
            .unwrap()
            .push_back(Reply::status(401));
        if second != 200 {
            upstream
                .sessions
                .lock()
                .unwrap()
                .push_back(Reply::status(second));
        }
        let (_server, service) = service(
            upstream.clone(),
            TestClock::default(),
            UserIpSource::XForwardedFor,
        )
        .await;
        let result = service.create("ip", ADDRESS).await;
        assert_eq!(result.is_ok(), second == 200);
        assert_eq!(upstream.count("/refresh"), 1);
        assert_eq!(upstream.count("/session"), 2);
        let calls = upstream.calls.lock().unwrap();
        assert_eq!(
            calls[1].headers["access-token"],
            calls[2].headers["access-token"]
        );
        assert_eq!(calls[1].body, calls[2].body);
    }
}

#[tokio::test]
async fn non_unauthorized_failures_do_not_retry_and_refresh_failures_restore_the_lock() {
    for status in [400, 403, 429, 500, 503] {
        let upstream = Arc::new(Upstream::default());
        upstream
            .sessions
            .lock()
            .unwrap()
            .push_back(Reply::status(status));
        let (_server, service) = service(
            upstream.clone(),
            TestClock::default(),
            UserIpSource::XForwardedFor,
        )
        .await;
        assert!(matches!(
            service.create("ip", ADDRESS).await,
            Err(SessionFailure::BadGateway)
        ));
        assert_eq!(upstream.count("/session"), 1);
    }
    let upstream = Arc::new(Upstream::default());
    upstream
        .refresh
        .lock()
        .unwrap()
        .push_back(Reply::status(500));
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    assert!(matches!(
        service.create("ip", ADDRESS).await,
        Err(SessionFailure::BadGateway)
    ));
    assert_eq!(upstream.count("/session"), 0);
    assert_eq!(service.create("ip", ADDRESS).await.unwrap(), WIDGET);
    assert_eq!(upstream.count("/refresh"), 2);
}

#[tokio::test]
async fn failed_refresh_restores_the_previous_cached_value() {
    let upstream = Arc::new(Upstream::default());
    let clock = TestClock::default();
    let (_server, service) =
        service(upstream.clone(), clock.clone(), UserIpSource::XForwardedFor).await;
    service.create("ip", ADDRESS).await.unwrap();
    upstream
        .refresh
        .lock()
        .unwrap()
        .push_back(Reply::status(500));
    clock.set(1941, 0);
    assert!(service.create("ip", ADDRESS).await.is_err());
    clock.set(1000, 0);
    assert_eq!(service.create("ip", ADDRESS).await.unwrap(), WIDGET);
    assert_eq!(
        upstream.count("/refresh"),
        2,
        "A failed refresh must not destroy the old token"
    );
}

#[tokio::test]
async fn unauthorized_retry_can_observe_a_token_refreshed_by_another_request() {
    let upstream = Arc::new(Upstream::default());
    upstream.refresh.lock().unwrap().push_back(Reply::json(
        json!({"data":{"accessToken":"old","expiresAt":1060}}),
    ));
    let mut unauthorized = Reply::status(401);
    unauthorized.delay = Duration::from_millis(100);
    upstream.sessions.lock().unwrap().push_back(unauthorized);
    let clock = TestClock::default();
    let (_server, service) =
        service(upstream.clone(), clock.clone(), UserIpSource::XForwardedFor).await;
    let first = service.clone();
    let task = tokio::spawn(async move { first.create("ip", ADDRESS).await });
    upstream.wait("/session", 1).await;
    clock.set(1001, 0);
    assert_eq!(service.create("ip", ADDRESS).await.unwrap(), WIDGET);
    assert_eq!(task.await.unwrap().unwrap(), WIDGET);
    assert_eq!(upstream.count("/refresh"), 2);
    let calls = upstream.calls.lock().unwrap();
    let tokens: Vec<_> = calls
        .iter()
        .filter(|call| call.path == "/session")
        .map(|call| call.headers["access-token"].to_str().unwrap())
        .collect();
    assert_eq!(tokens, ["old", "synthetic-token", "synthetic-token"]);
}

#[tokio::test]
async fn cancelling_a_refresh_releases_the_lock_and_retains_the_old_token() {
    let upstream = Arc::new(Upstream::default());
    let mut reply = Reply::json(json!({"data":{"accessToken":"cancelled","expiresAt":2000}}));
    reply.delay = Duration::from_secs(60);
    upstream.refresh.lock().unwrap().push_back(reply);
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    let first = service.clone();
    let task = tokio::spawn(async move { first.create("ip", ADDRESS).await });
    upstream.wait("/refresh", 1).await;
    task.abort();
    assert!(task.await.unwrap_err().is_cancelled());
    assert_eq!(
        tokio::time::timeout(Duration::from_secs(2), service.create("ip", ADDRESS))
            .await
            .unwrap()
            .unwrap(),
        WIDGET
    );
    assert_eq!(upstream.count("/refresh"), 2);
}

#[tokio::test]
async fn timeout_covers_refresh_lock_waits_and_session_io_and_allows_recovery() {
    for during_refresh in [true, false] {
        let upstream = Arc::new(Upstream::default());
        let mut reply = if during_refresh {
            Reply::json(json!({"data":{"accessToken":"late","expiresAt":2000}}))
        } else {
            Reply::json(json!({"data":{"widgetUrl":WIDGET}}))
        };
        reply.delay = Duration::from_secs(60);
        if during_refresh {
            upstream.refresh.lock().unwrap().push_back(reply);
        } else {
            upstream.sessions.lock().unwrap().push_back(reply);
        }
        let (_server, service) = service(
            upstream.clone(),
            TestClock::default(),
            UserIpSource::XForwardedFor,
        )
        .await;
        let first = service.clone();
        let task = tokio::spawn(async move { first.create("ip", ADDRESS).await });
        upstream
            .wait(
                if during_refresh {
                    "/refresh"
                } else {
                    "/session"
                },
                1,
            )
            .await;
        let second = if during_refresh {
            let service = service.clone();
            let task = tokio::spawn(async move { service.create("ip", ADDRESS).await });
            for _ in 0..10 {
                tokio::task::yield_now().await;
            }
            Some(task)
        } else {
            None
        };
        tokio::time::pause();
        tokio::time::advance(Duration::from_secs(11)).await;
        assert!(matches!(task.await.unwrap(), Err(SessionFailure::Timeout)));
        if let Some(task) = second {
            assert!(matches!(task.await.unwrap(), Err(SessionFailure::Timeout)));
        }
        tokio::time::resume();
        assert_eq!(
            tokio::time::timeout(Duration::from_secs(2), service.create("ip", ADDRESS))
                .await
                .unwrap()
                .unwrap(),
            WIDGET
        );
        assert_eq!(
            upstream.count("/refresh"),
            if during_refresh { 2 } else { 1 }
        );
    }
}
