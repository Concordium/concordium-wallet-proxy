//! This module sends refresh and widget-session requests to Transak.
//! Upstream bodies, partner credentials, and access tokens are not logged or returned in errors.

use super::config::{Endpoints, Settings};
use anyhow::{Context, ensure};
use concordium_rust_sdk::id::types::AccountAddress;
use num::{BigInt, BigRational};
use serde_json::{Value, json};

/// This token retains an exact numeric POSIX expiry and an opaque header value.
pub(super) struct AccessToken {
    /// This value is sent in the access-token header.
    pub value: String,
    /// This rational number is the POSIX expiry in seconds.
    pub expiry: BigRational,
}
impl Default for AccessToken {
    fn default() -> Self {
        Self {
            value: String::new(),
            expiry: BigRational::from_integer(0.into()),
        }
    }
}

/// This client supplies the two Transak HTTP operations.
pub(super) struct Client {
    /// This reusable HTTP client has no per-operation timeout; the service bounds the whole flow.
    http: reqwest::Client,
    /// These endpoints are fixed by configuration or supplied by controlled tests.
    endpoints: Endpoints,
}
impl Client {
    pub fn new(endpoints: Endpoints) -> anyhow::Result<Self> {
        Ok(Self {
            http: reqwest::Client::builder()
                .retry(reqwest::retry::never())
                .build()?,
            endpoints,
        })
    }
    pub async fn refresh(&self, settings: &Settings) -> anyhow::Result<AccessToken> {
        let response = self
            .http
            .post(&self.endpoints.refresh)
            .header("accept", "application/json")
            .header("api-secret", &settings.api_secret)
            .header("x-api-key", &settings.api_key)
            .json(&json!({"apiKey":settings.api_key}))
            .send()
            .await?;
        ensure!(response.status().is_success(), "Transak refresh failed");
        let data = crate::compatibility::json::read_complete(&response.bytes().await?)?;
        let token = data["data"]["accessToken"]
            .as_str()
            .context("Missing Transak access token")?;
        let expiry = data["data"]["expiresAt"]
            .as_number()
            .context("Missing Transak token expiry")?;
        http::HeaderValue::from_str(token).context("Invalid Transak access token header")?;
        Ok(AccessToken {
            value: token.into(),
            expiry: decimal(&expiry.to_string())?,
        })
    }
    pub async fn session(
        &self,
        settings: &Settings,
        token: &str,
        user_ip: &str,
        address: AccountAddress,
    ) -> anyhow::Result<Result<String, reqwest::StatusCode>> {
        let response = self
            .http
            .post(&self.endpoints.session)
            .header("accept", "application/json")
            .header("access-token", token)
            .header("x-api-key", &settings.api_key)
            .header("x-user-ip", user_ip)
            .json(&json!({"widgetParams":{
                "apiKey":settings.api_key,"cryptoCurrencyCode":"CCD",
                "referrerDomain":settings.referrer_domain,"walletAddress":address,
                "disableWalletAddressForm":true
            }}))
            .send()
            .await?;
        if !response.status().is_success() {
            return Ok(Err(response.status()));
        }
        let value: Value = crate::compatibility::json::read_complete(&response.bytes().await?)?;
        let url = value["data"]["widgetUrl"]
            .as_str()
            .context("Missing Transak widget URL")?;
        ensure!(super::uri::valid(url), "Invalid Transak widget URI");
        Ok(Ok(url.into()))
    }
}

/// Read finite decimal/scientific expiry values without floating-point rounding.
fn decimal(text: &str) -> anyhow::Result<BigRational> {
    let (mantissa, exponent) = text.split_once(['e', 'E']).unwrap_or((text, "0"));
    let fraction = mantissa
        .split_once('.')
        .map_or(0, |(_, fraction)| fraction.len());
    let coefficient = BigInt::parse_bytes(mantissa.replace('.', "").as_bytes(), 10)
        .context("Invalid token expiry coefficient")?;
    let power = exponent
        .parse::<i64>()?
        .checked_sub(fraction as i64)
        .context("Invalid token expiry exponent")?;
    let scale = BigInt::from(10).pow(power.unsigned_abs().try_into()?);
    let seconds = if power >= 0 {
        BigRational::from_integer(coefficient * scale)
    } else {
        BigRational::new(coefficient, scale)
    };
    // POSIXTime uses Fixed Pico: fromRational floors to whole picoseconds.
    let resolution = BigInt::from(1_000_000_000_000u64);
    let picoseconds = (seconds * &resolution).floor().to_integer();
    Ok(BigRational::new(picoseconds, resolution))
}
