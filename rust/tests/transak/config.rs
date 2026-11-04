//! This module checks typed Transak settings and secret-safe startup validation.
//! No test contacts production or staging endpoints.

use super::*;
use clap::{CommandFactory, FromArgMatches};
use concordium_wallet_proxy::config::Config;

fn parse(extra: &[&str]) -> Result<Config, clap::Error> {
    let mut arguments = vec![
        "proxy",
        "--database-url",
        "postgresql://localhost/wallet",
        "--node-endpoint",
        "http://node.invalid:20000",
        "--ip-data",
        "v0.json",
        "--ip-data-v1",
        "v1.json",
        "--ip-data-v2",
        "v2.json",
    ];
    arguments.extend_from_slice(extra);
    let matches = Config::command()
        .mut_args(|arg| arg.env(None::<&str>))
        .try_get_matches_from(arguments)?;
    Config::from_arg_matches(&matches)
}
fn arguments() -> Vec<&'static str> {
    vec![
        "--enable-transak",
        "--transak-api-key",
        "test-key",
        "--transak-api-secret",
        "test-secret",
        "--transak-referrer-domain",
        "wallet.example",
        "--transak-user-ip-source",
        "x-forwarded-for",
        "--transak-allowed-origin",
        "https://wallet.example",
        "--transak-allowed-origin",
        "http://localhost:8080",
    ]
}

#[test]
fn transak_is_disabled_by_default_and_enabled_fields_are_required() {
    assert!(parse(&[]).unwrap().transak.settings().unwrap().is_none());
    assert!(parse(&["--enable-transak"]).unwrap().validate().is_err());
    let config = parse(&arguments()).unwrap();
    config.validate().unwrap();
    let settings = config.transak.settings().unwrap().unwrap();
    assert!(!settings.use_staging);
    assert!(matches!(
        settings.user_ip_source,
        UserIpSource::XForwardedFor
    ));
    assert_eq!(
        settings.allowed_origins,
        ["https://wallet.example", "http://localhost:8080"]
    );
    for remove in [
        "--transak-api-key",
        "--transak-api-secret",
        "--transak-referrer-domain",
        "--transak-user-ip-source",
    ] {
        let mut args = arguments();
        let index = args.iter().position(|arg| *arg == remove).unwrap();
        args.drain(index..index + 2);
        assert!(
            parse(&args).unwrap().validate().is_err(),
            "Missing {remove}"
        );
    }
    let mut config = config;
    config.transak.transak_allowed_origins.clear();
    assert!(config.validate().is_err());
}

#[test]
fn typed_ip_source_and_staging_hosts_match_legacy_configuration() {
    assert!(parse(&["--transak-user-ip-source", "auto"]).is_err());
    let mut args = arguments();
    args.push("--transak-use-staging");
    let config = parse(&args).unwrap();
    assert!(config.transak.settings().unwrap().unwrap().use_staging);
    let staging = Endpoints::for_staging(true);
    let production = Endpoints::for_staging(false);
    assert_eq!(
        production.refresh,
        "https://api.transak.com/partners/api/v2/refresh-token"
    );
    assert_eq!(
        production.session,
        "https://api-gateway.transak.com/api/v2/auth/session"
    );
    assert_eq!(
        staging.refresh,
        "https://api-stg.transak.com/partners/api/v2/refresh-token"
    );
    assert_eq!(
        staging.session,
        "https://api-gateway-stg.transak.com/api/v2/auth/session"
    );
}

#[test]
fn validation_and_help_do_not_disclose_credentials() {
    let mut config = parse(&arguments()).unwrap();
    config.transak.transak_api_secret = Some("do-not-disclose\nsecret".into());
    let error = config.validate().unwrap_err().to_string();
    assert_eq!(error, "Invalid Transak API secret header");
    assert!(!error.contains("do-not-disclose"));
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_concordium-wallet-proxy"))
        .env_clear()
        .env("WALLET_PROXY_TRANSAK_API_KEY", "do-not-disclose-key")
        .env("WALLET_PROXY_TRANSAK_API_SECRET", "do-not-disclose-secret")
        .arg("--help")
        .output()
        .unwrap();
    assert!(output.status.success());
    assert!(!String::from_utf8_lossy(&output.stdout).contains("do-not-disclose"));
    assert!(!String::from_utf8_lossy(&output.stderr).contains("do-not-disclose"));
}

#[test]
fn transak_enable_flag_overrides_environment_before_startup_validation() {
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_concordium-wallet-proxy"))
        .env_clear()
        .env("WALLET_PROXY_ENABLE_TRANSAK", "true")
        .args([
            "--enable-transak=false",
            "--database-url",
            "postgresql://localhost/wallet",
            "--node-endpoint",
            "http://node.invalid:20000",
            "--ip-data",
            "/does-not-exist/v0.json",
            "--ip-data-v1",
            "/does-not-exist/v1.json",
            "--ip-data-v2",
            "/does-not-exist/v2.json",
        ])
        .output()
        .unwrap();
    assert!(!output.status.success());
    let error = String::from_utf8_lossy(&output.stderr);
    assert!(error.contains("Cannot read catalogue"), "{error}");
    assert!(!error.contains("Transak requires"));
}
