//! This library supplies startup, health checks, and identity-provider catalogues for the Wallet Proxy.
//! The existing Haskell service remains available during development.

pub mod catalogs;
pub mod config;
pub mod database;
pub mod dependencies;
pub mod health;
pub mod http;
pub mod network;
pub mod node;
pub mod startup;
pub mod wallet;
