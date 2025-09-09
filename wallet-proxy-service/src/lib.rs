/// Configuration of the Wallet Proxy service
pub mod configuration;
mod metrics_registry;
/// Monitoring API router (health, metrics, etc.)
mod monitoring_api;
/// The Wallet Proxy service, including all endpoints
pub mod service;
