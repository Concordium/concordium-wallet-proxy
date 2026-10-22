//! These tests check update boundaries, request errors, and independent terms fields.
//! Expected responses follow Main.hs and Proxy.hs without a running Haskell server.

use axum::{body::Body, http::Request};
use concordium_wallet_proxy::{
    config::WalletConfig, health::Health, http::router, wallet::WalletSettings,
};
use http_body_util::BodyExt;
use serde_json::{json, Value};
use std::sync::Arc;
use tower::ServiceExt;

async fn request(config: &WalletConfig, uri: &str) -> (u16, Value) {
    let dependencies = Arc::new(super::catalogs::UnusedDependencies);
    let health = Health {
        node: dependencies.clone(),
        history: dependencies.clone(),
        clock: dependencies,
        tolerance_seconds: 300,
    };
    let app = router(
        health,
        std::array::from_fn(|_| json!([])),
        WalletSettings::load(config).unwrap(),
        false,
    );
    let response = app
        .oneshot(Request::builder().uri(uri).body(Body::empty()).unwrap())
        .await
        .unwrap();
    let status = response.status().as_u16();
    let body =
        serde_json::from_slice(&response.into_body().collect().await.unwrap().to_bytes()).unwrap();
    (status, body)
}

#[tokio::test]
async fn generations_platforms_boundaries_and_precedence() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("updates.json");
    std::fs::write(&path, r#"{"ios":{"forceUpdateVersions":"0,5,10-17,64-","suggestUpdateVersions":"-20","url":"force","suggestUrl":"suggest"},"android":{"suggestUpdateVersions":"1-3","url":"fallback"}}"#).unwrap();
    for generation in 0..=1 {
        let mut config = WalletConfig::default();
        if generation == 0 {
            config.v0 = Some(path.clone());
        } else {
            config.v1 = Some(path.clone());
        }
        for (platform, version, expected) in [
            ("ios", 0, json!({"status":"needsUpdate","url":"force"})),
            ("ios", 4, json!({"status":"warning","url":"suggest"})),
            ("ios", 5, json!({"status":"needsUpdate","url":"force"})),
            ("ios", 9, json!({"status":"warning","url":"suggest"})),
            ("ios", 10, json!({"status":"needsUpdate","url":"force"})),
            ("ios", 17, json!({"status":"needsUpdate","url":"force"})),
            ("ios", 18, json!({"status":"warning","url":"suggest"})),
            ("ios", 20, json!({"status":"warning","url":"suggest"})),
            ("ios", 21, json!({"status":"ok"})),
            ("ios", 63, json!({"status":"ok"})),
            ("ios", 64, json!({"status":"needsUpdate","url":"force"})),
            (
                "ios",
                u64::MAX,
                json!({"status":"needsUpdate","url":"force"}),
            ),
            ("android", 0, json!({"status":"ok"})),
            ("android", 1, json!({"status":"warning","url":"fallback"})),
            ("android", 3, json!({"status":"warning","url":"fallback"})),
            ("android", 4, json!({"status":"ok"})),
        ] {
            assert_eq!(
                request(
                    &config,
                    &format!("/v{generation}/appSettings?platform={platform}&appVersion={version}")
                )
                .await,
                (200, expected)
            );
        }
        assert_eq!(
            request(
                &config,
                &format!("/v{}/appSettings?platform=ios&appVersion=0", 1 - generation)
            )
            .await,
            (200, json!({"status":"ok"}))
        );
    }
}

#[tokio::test]
async fn exact_legacy_errors_and_disabled_defaults() {
    let config = WalletConfig::default();
    for generation in 0..=1 {
        for (query, message) in [
            ("", "'platform' field is not present and 'version' field is either not present or not readable."),
            ("appVersion=1", "'platform' field is not present."),
            ("platform=ios", "'version' parameter is not present or readable. It must be a non-negative integer."),
            ("platform=ios&appVersion=1.2", "'version' parameter is not present or readable. It must be a non-negative integer."),
            ("platform=other&appVersion=1", "'param' should either be 'android' or 'ios'"),
        ] {
            assert_eq!(request(&config,&format!("/v{generation}/appSettings?{query}")).await,(400,json!({"error":1,"errorMessage":format!("Parsing input failed: {message}")})));
        }
        for platform in ["ios", "android"] {
            assert_eq!(
                request(
                    &config,
                    &format!("/v{generation}/appSettings?platform={platform}&appVersion=0")
                )
                .await,
                (200, json!({"status":"ok"}))
            );
        }
    }
}

#[tokio::test]
async fn all_terms_presence_combinations() {
    for version in [None, Some(""), Some("v1")] {
        for url in [None, Some(""), Some("https://terms")] {
            let config = WalletConfig {
                tc_version: version.map(str::to_owned),
                tc_url: url.map(str::to_owned),
                ..Default::default()
            };
            let expected = match (version, url) {
                (Some(v), Some(u)) => (200, json!({"version":v,"url":u})),
                _ => (404, json!({"error":1,"errorMessage":"Not found"})),
            };
            assert_eq!(
                request(&config, "/v0/termsAndConditionsVersion").await,
                expected
            );
        }
    }
}

#[test]
fn invalid_files_and_missing_required_links_fail_loading() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("updates.json");
    for value in [
        json!({"ios":{"forceUpdateVersions":"1.2","url":"x"}}),
        json!({"android":{"suggestUpdateVersions":"1--2","url":"x"}}),
        json!({"ios":{"forceUpdateVersions":"1","suggestUrl":"x"}}),
        json!({"android":{"suggestUpdateVersions":"2"}}),
        json!({"ios":{"suggestUpdateVersions":"*"}}),
    ] {
        std::fs::write(&path, value.to_string()).unwrap();
        for generation in 0..=1 {
            let mut config = WalletConfig::default();
            if generation == 0 {
                config.v0 = Some(path.clone());
            } else {
                config.v1 = Some(path.clone());
            }
            assert!(WalletSettings::load(&config).is_err());
        }
    }
    for value in [
        json!({}),
        json!({"ios":null}),
        json!({"ios":{}}),
        json!({"android":{"suggestUrl":"unused"}}),
    ] {
        std::fs::write(&path, value.to_string()).unwrap();
        assert!(WalletSettings::load(&WalletConfig {
            v0: Some(path.clone()),
            ..Default::default()
        })
        .is_ok());
    }
}

#[test]
fn strict_ranges_reject_malformed_input_in_both_fields() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("updates.json");
    for range in [
        " ",
        " 5",
        "5 ",
        "1, 2",
        "1 - 3",
        ",",
        "1,",
        ",1",
        "1,,2",
        "-",
        "17-10",
        "+5",
        "0x10",
        "1.2",
        "5abc",
        "abc",
        "1--2",
        "**",
        "18446744073709551616",
    ] {
        for field in ["forceUpdateVersions", "suggestUpdateVersions"] {
            let mut update = json!({"url":"destination"});
            update[field] = json!(range);
            std::fs::write(&path, json!({"ios":update}).to_string()).unwrap();
            assert!(
                WalletSettings::load(&WalletConfig {
                    v0: Some(path.clone()),
                    ..Default::default()
                })
                .is_err(),
                "{field}: {range:?}"
            );
        }
    }
}

#[tokio::test]
async fn deployment_fixtures_preserve_guidance_and_destinations() {
    // These cutoffs are read independently from the infrastructure values files.
    for (
        v0,
        v1,
        ios_cutoff,
        android_cutoff,
        new_ios_cutoff,
        new_android_cutoff,
        suggested_cutoff,
    ) in [
        (
            include_str!("fixtures/wallet/mainnet-v0.json"),
            include_str!("fixtures/wallet/mainnet-v1.json"),
            63,
            107,
            56,
            35,
            38,
        ),
        (
            include_str!("fixtures/wallet/testnet-v0.json"),
            include_str!("fixtures/wallet/testnet-v1.json"),
            63,
            107,
            57,
            35,
            38,
        ),
        (
            include_str!("fixtures/wallet/stagenet-v0.json"),
            include_str!("fixtures/wallet/stagenet-v1.json"),
            63,
            104,
            47,
            15,
            26,
        ),
    ] {
        let dir = tempfile::tempdir().unwrap();
        let paths = [dir.path().join("v0.json"), dir.path().join("v1.json")];
        std::fs::write(&paths[0], v0).unwrap();
        std::fs::write(&paths[1], v1).unwrap();
        let config = WalletConfig {
            v0: Some(paths[0].clone()),
            v1: Some(paths[1].clone()),
            ..Default::default()
        };
        let values: [Value; 2] = [
            serde_json::from_str(v0).unwrap(),
            serde_json::from_str(v1).unwrap(),
        ];
        for (generation, platform, cutoff, warning_end) in [
            (0, "ios", ios_cutoff, u64::MAX),
            (0, "android", android_cutoff, u64::MAX),
            (1, "ios", new_ios_cutoff, new_ios_cutoff),
            (1, "android", new_android_cutoff, suggested_cutoff),
        ] {
            for version in [
                0,
                cutoff,
                cutoff + 1,
                warning_end,
                warning_end.saturating_add(1),
                u64::MAX,
            ] {
                let update = &values[generation][platform];
                let expected = if version <= cutoff {
                    json!({"status":"needsUpdate","url":update["url"]})
                } else if version <= warning_end {
                    json!({"status":"warning","url":update.get("suggestUrl").unwrap_or(&update["url"])})
                } else {
                    json!({"status":"ok"})
                };
                assert_eq!(
                    request(
                        &config,
                        &format!(
                            "/v{generation}/appSettings?platform={platform}&appVersion={version}"
                        )
                    )
                    .await,
                    (200, expected)
                );
            }
        }
    }
}
