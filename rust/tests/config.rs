//! This module checks command-line settings, environment variables, and secret files.
//! It also checks that invalid settings and catalogue files are rejected.

use clap::{CommandFactory, FromArgMatches};
use concordium_wallet_proxy::config::{load_catalogs, Config};
use std::{ffi::OsString, path::PathBuf, process::Command};

/// Create test settings and three empty catalogue files in a temporary directory.
/// Keep the returned directory value alive while the files are in use.
pub fn config() -> (tempfile::TempDir, Config) {
    let dir = tempfile::tempdir().unwrap();
    for name in ["v0.json", "v1.json", "v2.json"] {
        std::fs::write(dir.path().join(name), "[]").unwrap();
    }
    let mut config = parse(&[]).unwrap();
    config.catalogs.v0 = dir.path().join("v0.json");
    config.catalogs.v1 = dir.path().join("v1.json");
    config.catalogs.v2 = dir.path().join("v2.json");
    (dir, config)
}

fn arguments() -> Vec<OsString> {
    [
        "proxy",
        "--database-url",
        "postgresql://localhost/wallet",
        "--node-endpoint",
        "http://node.example:20000",
        "--ip-data",
        "v0.json",
        "--ip-data-v1",
        "v1.json",
        "--ip-data-v2",
        "v2.json",
    ]
    .into_iter()
    .map(OsString::from)
    .collect()
}

fn parse(extra: &[&str]) -> Result<Config, clap::Error> {
    let mut args = arguments();
    if extra.contains(&"--node-endpoint") {
        let index = args
            .iter()
            .position(|arg| arg == "--node-endpoint")
            .unwrap();
        args.drain(index..index + 2);
    }
    args.extend(extra.iter().map(OsString::from));
    // Disable process environment input for deterministic in-process tests.
    let matches = Config::command()
        .mut_args(|arg| arg.env(None::<&str>))
        .try_get_matches_from(args)?;
    Config::from_arg_matches(&matches)
}

fn command() -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_concordium-wallet-proxy"));
    command
        .env_clear()
        .env("WALLET_PROXY_DATABASE_URL", "postgresql://localhost/wallet")
        .env("WALLET_PROXY_NODE_ENDPOINT", "http://node.example:20000")
        .env("WALLET_PROXY_IP_DATA", "v0.json")
        .env("WALLET_PROXY_IP_DATA_V1", "v1.json")
        .env("WALLET_PROXY_IP_DATA_V2", "v2.json");
    command
}

#[test]
fn clap_command_is_valid() {
    Config::command().debug_assert();
}

#[test]
fn defaults_and_catalogues_are_preserved() {
    let (_dir, config) = config();
    config.validate().unwrap();
    assert_eq!(config.server.listen.to_string(), "0.0.0.0:3000");
    assert_eq!(config.server.health_tolerance_seconds, 300);
    assert!(!config.server.enable_public_cors);
    assert_eq!(config.server.log_filter, "off");
    assert_eq!(config.node.endpoint, "http://node.example:20000");
    assert_eq!(config.node.timeout_seconds, 15);
    assert_eq!(config.node.connect_retries, 0);
    assert_eq!(config.database.max_connections.get(), 10);
    assert_eq!(
        load_catalogs(&config.catalogs).unwrap(),
        [
            serde_json::json!([]),
            serde_json::json!([]),
            serde_json::json!([])
        ]
    );
}

#[test]
fn flags_set_typed_values_and_keep_relative_paths() {
    let config = parse(&[
        "--listen",
        "127.0.0.1:4000",
        "--health-tolerance-seconds",
        "-1",
        "--enable-public-cors",
        "--node-endpoint",
        "https://node.example:20000",
        "--node-connect-retries",
        "2",
        "--node-timeout-seconds",
        "30",
    ])
    .unwrap();
    config.validate().unwrap();
    assert_eq!(config.server.listen.to_string(), "127.0.0.1:4000");
    assert_eq!(config.server.health_tolerance_seconds, -1);
    assert!(config.server.enable_public_cors);
    assert_eq!(config.node.endpoint, "https://node.example:20000");
    assert_eq!(config.node.connect_retries, 2);
    assert_eq!(config.node.timeout_seconds, 30);
    assert_eq!(config.catalogs.v0, PathBuf::from("v0.json"));
    assert!(
        !parse(&["--enable-public-cors=false"])
            .unwrap()
            .server
            .enable_public_cors
    );
}

#[test]
fn environment_supplies_required_settings_and_flags_override_environment() {
    // A bad log filter stops startup before file access or network operations.
    let output = command()
        .args(["--log-filter", "[broken"])
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("Invalid log filter"));
    for (env, value, flag, replacement) in [
        (
            "WALLET_PROXY_LISTEN",
            "invalid",
            "--listen",
            "127.0.0.1:4000",
        ),
        (
            "WALLET_PROXY_NODE_TIMEOUT_SECONDS",
            "0",
            "--node-timeout-seconds",
            "1",
        ),
        (
            "WALLET_PROXY_DATABASE_MAX_CONNECTIONS",
            "0",
            "--database-max-connections",
            "7",
        ),
        (
            "WALLET_PROXY_DATABASE_URL",
            "INVALID_SECRET",
            "--database-url",
            "postgresql://localhost/wallet",
        ),
        (
            "WALLET_PROXY_ENABLE_PUBLIC_CORS",
            "invalid",
            "--enable-public-cors=false",
            "",
        ),
    ] {
        let mut process = command();
        process
            .env(env, value)
            .args(["--log-filter", "[broken"])
            .arg(flag);
        if !replacement.is_empty() {
            process.arg(replacement);
        }
        let output = process.output().unwrap();
        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("Invalid log filter"), "{env}: {stderr}");
    }
    let output = command()
        .env("WALLET_PROXY_NODE_TIMEOUT_SECONDS", "0")
        .output()
        .unwrap();
    assert!(String::from_utf8_lossy(&output.stderr).contains("Node timeout must be positive"));
}

#[test]
fn help_hides_database_secret_and_no_longer_accepts_a_config_file() {
    let output = command()
        .env(
            "WALLET_PROXY_DATABASE_URL",
            "postgresql://user:PRIVATE_PASSWORD@localhost/wallet",
        )
        .arg("--help")
        .output()
        .unwrap();
    assert!(output.status.success());
    let help = String::from_utf8_lossy(&output.stdout);
    assert!(help.contains("WALLET_PROXY_DATABASE_URL"));
    assert!(help.contains("--database-url-file"));
    assert!(!help.contains("PRIVATE_PASSWORD"));
    assert!(!help.contains("--config"));
    assert_eq!(
        parse(&["--config", "proxy.toml"]).err().unwrap().kind(),
        clap::error::ErrorKind::UnknownArgument
    );
}

#[test]
fn node_endpoint_has_no_default() {
    let output = command()
        .env_remove("WALLET_PROXY_NODE_ENDPOINT")
        .output()
        .unwrap();
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("--node-endpoint"));
    assert!(stderr.contains("required"));

    let config = parse(&["--node-endpoint", ""]).unwrap();
    assert!(config.validate().is_err());
    let output = command()
        .env("WALLET_PROXY_NODE_ENDPOINT", "")
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("node endpoint"));
}

#[test]
fn connection_source_and_catalogues_are_required() {
    for omitted in [
        "--database-url",
        "--node-endpoint",
        "--ip-data",
        "--ip-data-v1",
        "--ip-data-v2",
    ] {
        let mut args = arguments();
        let index = args.iter().position(|arg| arg == omitted).unwrap();
        args.drain(index..index + 2);
        let result = Config::command()
            .mut_args(|arg| arg.env(None::<&str>))
            .try_get_matches_from(args);
        assert_eq!(
            result.err().unwrap().kind(),
            clap::error::ErrorKind::MissingRequiredArgument
        );
    }
    assert_eq!(
        parse(&["--database-url-file", "secret"])
            .err()
            .unwrap()
            .kind(),
        clap::error::ErrorKind::ArgumentConflict
    );
}

#[test]
fn database_pool_limit_is_positive_and_does_not_select_a_connection_source() {
    let config = parse(&["--database-max-connections", "7"]).unwrap();
    config.validate().unwrap();
    assert_eq!(config.database.max_connections.get(), 7);
    for value in ["0", "-1", "4294967296", "invalid"] {
        assert!(
            parse(&["--database-max-connections", value]).is_err(),
            "Accepted {value}"
        );
    }

    let output = command()
        .env("WALLET_PROXY_DATABASE_MAX_CONNECTIONS", "0")
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("--database-max-connections"));
    let output = command()
        .env("WALLET_PROXY_DATABASE_MAX_CONNECTIONS", "7")
        .args(["--log-filter", "[broken"])
        .output()
        .unwrap();
    assert!(String::from_utf8_lossy(&output.stderr).contains("Invalid log filter"));

    let mut args = arguments();
    let index = args.iter().position(|arg| arg == "--database-url").unwrap();
    args.drain(index..index + 2);
    args.extend([
        OsString::from("--database-max-connections"),
        OsString::from("7"),
    ]);
    let error = Config::command()
        .mut_args(|arg| arg.env(None::<&str>))
        .try_get_matches_from(args)
        .err()
        .unwrap();
    assert_eq!(
        error.kind(),
        clap::error::ErrorKind::MissingRequiredArgument
    );
}

#[test]
fn secret_file_supplies_the_url_and_invalid_secrets_are_not_disclosed() {
    let (dir, mut config) = config();
    let secret = dir.path().join("db-secret");
    config.database.url = None;
    config.database.url_file = Some(secret.clone());
    std::fs::write(&secret, "postgresql://localhost/wallet\r\n").unwrap();
    config.validate().unwrap();
    assert_eq!(
        config.database.connection_string().unwrap(),
        "postgresql://localhost/wallet"
    );
    std::fs::write(&secret, "SUPER_SECRET_INVALID_URL").unwrap();
    let error = config.validate().unwrap_err();
    assert!(!format!("{error:#}").contains("SUPER_SECRET"));
    std::fs::remove_file(secret).unwrap();
    assert!(config.validate().is_err());
}

#[test]
fn environment_can_select_a_secret_file() {
    let dir = tempfile::tempdir().unwrap();
    let secret = dir.path().join("db-url");
    std::fs::write(&secret, "postgresql://localhost/wallet\n").unwrap();
    let output = command()
        .env_remove("WALLET_PROXY_DATABASE_URL")
        .env("WALLET_PROXY_DATABASE_URL_FILE", secret)
        .args(["--log-filter", "[broken"])
        .output()
        .unwrap();
    assert!(String::from_utf8_lossy(&output.stderr).contains("Invalid log filter"));
}

#[test]
fn invalid_arguments_and_settings_fail_before_network_access() {
    for (flag, value) in [
        ("--node-target", "node-0"),
        ("--node-timeout-seconds", "0"),
        ("--node-endpoint", "ftp://node.example"),
        ("--node-endpoint", "http://node.example/query"),
        ("--listen", "not-a-socket"),
        ("--log-filter", "[broken"),
        ("--health-tolerance-seconds", "9223372036854775807"),
        ("--unknown", "true"),
        ("--enable-public-cors=invalid", ""),
    ] {
        let values = if value.is_empty() {
            vec![flag]
        } else {
            vec![flag, value]
        };
        match parse(&values) {
            Err(_) => {}
            Ok(config) => assert!(config.validate().is_err(), "Accepted {flag}"),
        }
    }
}

#[test]
fn missing_and_malformed_catalogues_are_rejected() {
    let (dir, config) = config();
    std::fs::write(dir.path().join("v1.json"), "not JSON").unwrap();
    assert!(load_catalogs(&config.catalogs).is_err());
    std::fs::remove_file(dir.path().join("v1.json")).unwrap();
    assert!(load_catalogs(&config.catalogs).is_err());
}
