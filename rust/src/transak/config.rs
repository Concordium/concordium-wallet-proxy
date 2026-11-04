//! This module reads typed startup options for the optional Transak integration.
//! Secret values are not included in help text, Debug output, or validation errors.

use anyhow::{Context, ensure};
use clap::{Args, ValueEnum};

/// This option selects the source of the end-user IP header.
#[derive(Clone, Copy, ValueEnum)]
pub enum UserIpSource {
    /// Use the peer socket address without trusting forwarding headers.
    RemoteAddress,
    /// Use the first comma-separated x-forwarded-for entry without IP validation.
    XForwardedFor,
}

/// These typed options replace the legacy Transak configuration file.
#[derive(Clone, Default, Args)]
#[group(skip)]
pub struct TransakConfig {
    /// This flag enables the optional purchase-session endpoint.
    #[arg(long, env = "WALLET_PROXY_ENABLE_TRANSAK", action = clap::ArgAction::Set, num_args = 0..=1, default_missing_value = "true", require_equals = true, default_value_t = false)]
    pub enable_transak: bool,
    /// This flag selects Transak's staging hosts instead of production hosts.
    #[arg(long, env = "WALLET_PROXY_TRANSAK_USE_STAGING", action = clap::ArgAction::Set, num_args = 0..=1, default_missing_value = "true", require_equals = true, default_value_t = false)]
    pub transak_use_staging: bool,
    /// This partner API key is required when Transak is enabled.
    #[arg(long, env = "WALLET_PROXY_TRANSAK_API_KEY", hide_env_values = true)]
    pub transak_api_key: Option<String>,
    /// This partner secret is required when Transak is enabled. Prefer the environment variable.
    #[arg(long, env = "WALLET_PROXY_TRANSAK_API_SECRET", hide_env_values = true)]
    pub transak_api_secret: Option<String>,
    /// This domain is passed unchanged in widget parameters.
    #[arg(long, env = "WALLET_PROXY_TRANSAK_REFERRER_DOMAIN")]
    pub transak_referrer_domain: Option<String>,
    /// This explicit source controls trust in forwarded end-user IP headers.
    #[arg(long, env = "WALLET_PROXY_TRANSAK_USER_IP_SOURCE", value_enum)]
    pub transak_user_ip_source: Option<UserIpSource>,
    /// These exact browser origins are allowed. Repeat the flag or separate environment values with commas.
    #[arg(
        long = "transak-allowed-origin",
        env = "WALLET_PROXY_TRANSAK_ALLOWED_ORIGINS",
        value_delimiter = ','
    )]
    pub transak_allowed_origins: Vec<String>,
}

/// These settings contain the validated Transak partner configuration.
#[derive(Clone)]
pub struct Settings {
    /// This flag selects the staging endpoints.
    pub use_staging: bool,
    /// This key is sent in the x-api-key header and JSON bodies.
    pub api_key: String,
    /// This secret is sent only to the refresh endpoint.
    pub api_secret: String,
    /// This domain is passed in widget parameters.
    pub referrer_domain: String,
    /// This selection identifies the end-user IP source.
    pub user_ip_source: UserIpSource,
    /// These byte-exact origins govern browser CORS headers.
    pub allowed_origins: Vec<String>,
}
impl TransakConfig {
    /// Validate enabled settings without contacting Transak or disclosing credentials.
    pub fn settings(&self) -> anyhow::Result<Option<Settings>> {
        if !self.enable_transak {
            return Ok(None);
        }
        let key = self
            .transak_api_key
            .clone()
            .context("Transak requires an API key")?;
        let secret = self
            .transak_api_secret
            .clone()
            .context("Transak requires an API secret")?;
        http::HeaderValue::from_str(&key).context("Invalid Transak API key header")?;
        http::HeaderValue::from_str(&secret).context("Invalid Transak API secret header")?;
        ensure!(
            !self.transak_allowed_origins.is_empty(),
            "Transak requires at least one allowed origin"
        );
        for origin in &self.transak_allowed_origins {
            http::HeaderValue::from_str(origin).context("Invalid Transak allowed origin header")?;
        }
        Ok(Some(Settings {
            use_staging: self.transak_use_staging,
            api_key: key,
            api_secret: secret,
            referrer_domain: self
                .transak_referrer_domain
                .clone()
                .context("Transak requires a referrer domain")?,
            user_ip_source: self
                .transak_user_ip_source
                .context("Transak requires an explicit user IP source")?,
            allowed_origins: self.transak_allowed_origins.clone(),
        }))
    }
}

/// These URLs select the refresh and widget-session HTTP endpoints.
#[derive(Clone)]
pub struct Endpoints {
    /// This URL accepts refresh-token POST requests.
    pub refresh: String,
    /// This URL accepts widget-session POST requests.
    pub session: String,
}
impl Endpoints {
    /// Select the production or staging hosts used by the legacy service.
    pub fn for_staging(staging: bool) -> Self {
        if staging {
            Self {
                refresh: "https://api-stg.transak.com/partners/api/v2/refresh-token".into(),
                session: "https://api-gateway-stg.transak.com/api/v2/auth/session".into(),
            }
        } else {
            Self {
                refresh: "https://api.transak.com/partners/api/v2/refresh-token".into(),
                session: "https://api-gateway.transak.com/api/v2/auth/session".into(),
            }
        }
    }
}
