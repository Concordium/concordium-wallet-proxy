//! This module checks request validation, IP selection, upstream failures, and browser CORS.
//! Expected errors follow Proxy.hs and the English error catalogue.

use super::*;
use axum::{extract::ConnectInfo, http::header};

#[tokio::test]
async fn disabled_and_invalid_requests_do_not_contact_transak() {
    for method in ["POST", "OPTIONS"] {
        let response = transak::routes::<TestClock>(None)
            .oneshot(
                Request::builder()
                    .method(method)
                    .uri("/v0/transakOnRamp")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(
            value(response).await,
            (404, json!({"error":2,"errorMessage":"Not found"}))
        );
    }
    let upstream = Arc::new(Upstream::default());
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    let app = transak::routes(Some(service));
    for (query, message) in [
        (
            "",
            "'address' parameter is required to be an account address",
        ),
        (
            "?address=invalid",
            "'address' is not a valid account address",
        ),
        ("?address=", "'address' is not a valid account address"),
    ] {
        assert_eq!(
            value(request(app.clone(), query, &[]).await).await,
            (
                400,
                json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})
            )
        );
    }
    for headers in [
        vec![],
        vec![("x-forwarded-for", "")],
        vec![("x-forwarded-for", " ,203.0.113.1")],
    ] {
        assert_eq!(
            value(request(app.clone(), &format!("?address={ADDRESS}"), &headers).await).await,
            (
                500,
                json!({"error":0,"errorMessage":"Internal server error"})
            )
        );
    }
    let request = Request::builder()
        .method("POST")
        .uri(format!("/v0/transakOnRamp?address={ADDRESS}"))
        .header(
            "x-forwarded-for",
            axum::http::HeaderValue::from_bytes(&[0xff]).unwrap(),
        )
        .body(Body::empty())
        .unwrap();
    assert_eq!(value(app.oneshot(request).await.unwrap()).await.0, 500);
    assert!(upstream.calls.lock().unwrap().is_empty());
}

#[tokio::test]
async fn first_query_value_and_first_forwarded_ip_are_used_without_ip_validation() {
    let upstream = Arc::new(Upstream::default());
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    let response = request(
        transak::routes(Some(service)),
        &format!("?address={ADDRESS}&address=invalid"),
        &[("x-forwarded-for", "  not-an-ip  , 203.0.113.9")],
    )
    .await;
    assert_eq!(value(response).await, (200, json!({"widgetUrl":WIDGET})));
    assert_eq!(
        upstream.calls.lock().unwrap()[1].headers["x-user-ip"],
        "not-an-ip"
    );
}

#[tokio::test]
async fn peer_ip_selection_ignores_forwarding_headers_and_preserves_ipv6_format() {
    for (peer, expected) in [
        ("192.0.2.3:1234", "192.0.2.3"),
        ("[2001:db8::1]:1234", "2001:db8:0:0:0:0:0:1"),
        ("[::ffff:192.0.2.3]:1234", "0:0:0:0:0:ffff:c000:203"),
    ] {
        let upstream = Arc::new(Upstream::default());
        let (_server, service) = service(
            upstream.clone(),
            TestClock::default(),
            UserIpSource::RemoteAddress,
        )
        .await;
        let app = transak::routes(Some(service));
        let mut request = Request::builder()
            .method("POST")
            .uri(format!("/v0/transakOnRamp?address={ADDRESS}"))
            .header("x-forwarded-for", "spoofed")
            .body(Body::empty())
            .unwrap();
        request
            .extensions_mut()
            .insert(ConnectInfo(peer.parse::<std::net::SocketAddr>().unwrap()));
        assert_eq!(
            value(app.clone().oneshot(request).await.unwrap()).await.0,
            200
        );
        assert_eq!(
            upstream.calls.lock().unwrap()[1].headers["x-user-ip"],
            expected
        );
        assert_eq!(
            value(
                super::request(
                    app,
                    &format!("?address={ADDRESS}"),
                    &[("x-forwarded-for", "spoofed")]
                )
                .await
            )
            .await
            .0,
            500
        );
    }
}

#[tokio::test]
async fn malformed_refresh_and_session_responses_never_leak_upstream_details() {
    for refresh in [true, false] {
        for body in [
            "upstream-secret-error",
            "{}",
            "{\"data\":{}}",
            "null",
            "{\"data\":{\"accessToken\":123,\"expiresAt\":2000,\"widgetUrl\":123}}",
            "{\"data\":{\"accessToken\":\"secret\",\"expiresAt\":2000,\"widgetUrl\":\"https://valid.invalid\"}} trailing",
        ] {
            let upstream = Arc::new(Upstream::default());
            let reply = Reply {
                status: 200,
                body: body.into(),
                delay: Duration::ZERO,
            };
            if refresh {
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
            let response = request(
                transak::routes(Some(service)),
                &format!("?address={ADDRESS}"),
                &[("x-forwarded-for", "ip")],
            )
            .await;
            assert_eq!(
                value(response).await,
                (
                    502,
                    json!({"error":0,"errorMessage":"Invalid response from upstream server."})
                )
            );
            assert_eq!(upstream.count("/session"), usize::from(!refresh));
        }
    }
}

#[tokio::test]
async fn widget_uri_validation_preserves_absolute_opaque_and_fragment_forms() {
    for uri in [
        "https://example.invalid:443/a%2Fb?x=%20#frag",
        "urn:example:item",
        "https://[::1]/",
        "https://user:pass@example.invalid/path",
        "https://example.invalid/?x=a/b?c",
    ] {
        let upstream = Arc::new(Upstream::default());
        upstream
            .sessions
            .lock()
            .unwrap()
            .push_back(Reply::json(json!({"data":{"widgetUrl":uri}})));
        let (_server, service) =
            service(upstream, TestClock::default(), UserIpSource::XForwardedFor).await;
        assert_eq!(service.create("ip", ADDRESS).await.unwrap(), uri);
    }
    for uri in [
        "not a URI",
        "/relative",
        "",
        "https://example.invalid/%zz",
        "https://example.invalid/#one#two",
        "https://[invalid]/",
        "https://example.invalid:bad/",
        "https://example.invalid/ümlaut",
    ] {
        let upstream = Arc::new(Upstream::default());
        upstream
            .sessions
            .lock()
            .unwrap()
            .push_back(Reply::json(json!({"data":{"widgetUrl":uri}})));
        let (_server, service) =
            service(upstream, TestClock::default(), UserIpSource::XForwardedFor).await;
        assert!(
            service.create("ip", ADDRESS).await.is_err(),
            "Invalid URI: {uri}"
        );
    }
}

#[tokio::test]
async fn transak_cors_is_independent_of_public_cors_and_failed_checks_do_not_block() {
    for public in [false, true] {
        let upstream = Arc::new(Upstream::default());
        let (_server, service) = service(
            upstream.clone(),
            TestClock::default(),
            UserIpSource::XForwardedFor,
        )
        .await;
        let (app, _) = crate::health::app_with_routes(
            "missing",
            false,
            public,
            transak::routes(Some(service)),
        );
        for origin in [
            None,
            Some("https://wallet.example"),
            Some("https://other.example"),
        ] {
            let mut headers = vec![("x-forwarded-for", "ip")];
            if let Some(origin) = origin {
                headers.push(("origin", origin));
            }
            let response = request(app.clone(), &format!("?address={ADDRESS}"), &headers).await;
            assert_eq!(response.status(), 200);
            assert_eq!(
                response
                    .headers()
                    .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
                    .map(|value| value.to_str().unwrap()),
                origin.filter(|origin| *origin == "https://wallet.example")
            );
            assert!(
                !response
                    .headers()
                    .contains_key(header::ACCESS_CONTROL_ALLOW_CREDENTIALS)
            );
            assert_eq!(
                response.headers().contains_key(header::VARY),
                origin == Some("https://wallet.example")
            );
            assert_eq!(
                response.headers()[header::SERVER],
                "Concordium-wallet-proxy"
            );
        }
        for (origin, method, headers, allowed) in [
            (
                Some("https://wallet.example"),
                "POST",
                Some("content-type"),
                true,
            ),
            (Some("http://localhost:8080"), "OPTIONS", None, true),
            (Some("https://wallet.example"), "PUT", None, false),
            (
                Some("https://wallet.example"),
                "POST",
                Some("x-unapproved"),
                false,
            ),
            (Some("https://wallet.example/"), "POST", None, false),
            (None, "POST", None, false),
        ] {
            let mut request = Request::builder()
                .method("OPTIONS")
                .uri("/v0/transakOnRamp")
                .header(header::ACCESS_CONTROL_REQUEST_METHOD, method);
            if let Some(origin) = origin {
                request = request.header(header::ORIGIN, origin);
            }
            if let Some(headers) = headers {
                request = request.header(header::ACCESS_CONTROL_REQUEST_HEADERS, headers);
            }
            let response = app
                .clone()
                .oneshot(request.body(Body::empty()).unwrap())
                .await
                .unwrap();
            assert_eq!(response.status(), 200);
            assert_eq!(
                response
                    .headers()
                    .contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN),
                allowed
            );
            assert_eq!(
                response
                    .headers()
                    .contains_key(header::ACCESS_CONTROL_ALLOW_METHODS),
                allowed
            );
            if allowed {
                assert_eq!(
                    response.headers()[header::ACCESS_CONTROL_ALLOW_METHODS],
                    "POST, OPTIONS"
                );
            }
        }
        assert_eq!(
            upstream.count("/session"),
            3,
            "Preflight must not create sessions"
        );
        let (disabled, _) = crate::health::app_with_routes(
            "missing",
            false,
            public,
            transak::routes::<TestClock>(None),
        );
        let preflight = disabled
            .clone()
            .oneshot(
                Request::builder()
                    .method("OPTIONS")
                    .uri("/v0/transakOnRamp")
                    .header(header::ORIGIN, "https://wallet.example")
                    .header(header::ACCESS_CONTROL_REQUEST_METHOD, "POST")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(preflight.status(), 404);
        assert!(
            !preflight
                .headers()
                .contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );
        let response = request(disabled, "", &[("origin", "https://wallet.example")]).await;
        assert_eq!(response.status(), 404);
        assert!(
            !response
                .headers()
                .contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );
    }
}

#[tokio::test]
async fn whole_operation_timeout_has_the_legacy_public_504_response() {
    let upstream = Arc::new(Upstream::default());
    upstream.refresh.lock().unwrap().push_back(Reply {
        status: 200,
        body: "{}".into(),
        delay: Duration::from_secs(60),
    });
    let (_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::XForwardedFor,
    )
    .await;
    let app = transak::routes(Some(service));
    let task = tokio::spawn(async move {
        request(
            app,
            &format!("?address={ADDRESS}"),
            &[("x-forwarded-for", "ip")],
        )
        .await
    });
    upstream.wait("/refresh", 1).await;
    tokio::time::pause();
    tokio::time::advance(Duration::from_secs(11)).await;
    let response = task.await.unwrap();
    tokio::time::resume();
    assert_eq!(
        value(response).await,
        (
            504,
            json!({"error":0,"errorMessage":"Timeout occurred waiting for response from upstream server."})
        )
    );
}

#[tokio::test]
async fn upstream_connection_failure_is_a_bad_gateway_not_a_timeout() {
    let upstream = Arc::new(Upstream::default());
    let (server, service) =
        service(upstream, TestClock::default(), UserIpSource::XForwardedFor).await;
    drop(server);
    tokio::task::yield_now().await;
    assert!(matches!(
        service.create("ip", ADDRESS).await,
        Err(transak::SessionFailure::BadGateway)
    ));
}

#[tokio::test]
async fn actual_http_peer_address_and_shared_response_metadata_are_available() {
    let upstream = Arc::new(Upstream::default());
    let (_upstream_server, service) = service(
        upstream.clone(),
        TestClock::default(),
        UserIpSource::RemoteAddress,
    )
    .await;
    let (app, _) =
        crate::health::app_with_routes("missing", false, true, transak::routes(Some(service)));
    let server = crate::support::serve_router(app).await;
    let response = reqwest::Client::new()
        .post(format!(
            "{}/v0/transakOnRamp?address={ADDRESS}",
            server.endpoint
        ))
        .header("origin", "https://wallet.example")
        .header("x-forwarded-for", "spoofed")
        .send()
        .await
        .unwrap();
    assert_eq!(response.status(), 200);
    assert_eq!(
        response.headers()[header::CONTENT_TYPE],
        "application/json; charset=utf-8"
    );
    assert_eq!(
        response.headers()[header::ACCESS_CONTROL_ALLOW_ORIGIN],
        "https://wallet.example"
    );
    assert_eq!(
        response.json::<Value>().await.unwrap(),
        json!({"widgetUrl":WIDGET})
    );
    assert_eq!(
        upstream.calls.lock().unwrap()[1].headers["x-user-ip"],
        "127.0.0.1"
    );
}
