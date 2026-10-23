//! This module checks submission routes inside the shared HTTP compatibility layer.
//! A real local listener verifies metadata, CORS, redirects, methods, and body handling.

use super::*;
use crate::{health, support};

#[tokio::test]
async fn extended_and_raw_http_metadata_methods_cors_and_redirects_keep_shared_contracts() {
    let node = Arc::new(TestNode::default());
    let (app, database_calls) =
        health::app_with_routes("missing", false, true, submission::routes(node.clone()));
    let server = support::serve_router(app).await;
    let client = reqwest::Client::builder()
        .no_proxy()
        .redirect(reqwest::redirect::Policy::none())
        .build()
        .unwrap();
    for (route, bytes) in [
        (
            "submitExtended",
            fixtures::extended(true).to_string().into_bytes(),
        ),
        ("submitRawTransaction", fixtures::extended_bytes(true)),
    ] {
        let url = format!("{}/v0/{route}", server.endpoint);
        let response = client
            .put(&url)
            .header("origin", "https://wallet.example")
            .header("content-type", "application/octet-stream")
            .body(bytes)
            .send()
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        assert_eq!(
            response.headers()["content-type"],
            "application/json; charset=utf-8"
        );
        assert_eq!(response.headers()["server"], "Concordium-wallet-proxy");
        assert_eq!(response.headers()["access-control-allow-origin"], "*");
        assert_eq!(
            response.json::<Value>().await.unwrap(),
            json!({"submissionId":"f5de8424f8e8629aef626a77cdfc87902c104a74b84301bb8c9e06fcd6bbbb85"})
        );
        let response = client.get(&url).send().await.unwrap();
        assert_eq!(response.status(), 405);
        assert_eq!(response.headers()["content-language"], "en");
        let response = client
            .request(reqwest::Method::OPTIONS, &url)
            .header("origin", "https://wallet.example")
            .header("access-control-request-method", "PUT")
            .send()
            .await
            .unwrap();
        assert_eq!(response.status(), 200);
        assert_eq!(response.headers()["access-control-allow-origin"], "*");
        assert!(
            response.headers()["access-control-allow-methods"]
                .to_str()
                .unwrap()
                .contains("PUT")
        );
        let response = client
            .put(format!("{url}/?x=y"))
            .body("ignored")
            .send()
            .await
            .unwrap();
        assert_eq!(response.status(), 301);
        assert_eq!(response.headers()["location"], format!("/v0/{route}?x=y"));
        let response = client.put(&url).body("{}").send().await.unwrap();
        assert_eq!(response.status(), 400);
        assert_eq!(response.headers()["content-language"], "en");
        assert!(
            response
                .json::<Value>()
                .await
                .unwrap()
                .get("submissionId")
                .is_none()
        );
    }
    assert_eq!(
        *node.items.lock().unwrap(),
        vec![fixtures::extended_bytes(true); 2]
    );
    assert_eq!(database_calls.load(std::sync::atomic::Ordering::SeqCst), 0);
}
