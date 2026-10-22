//! This library supplies startup and wallet-facing HTTP features for the Wallet Proxy.
//! The existing Haskell service remains available during development.

pub mod balances;
pub mod catalogs;
mod compatibility;
pub mod config;
pub mod database;
pub mod dependencies;
pub mod health;
pub mod http;
pub mod network;
pub mod node;
pub mod startup;
pub mod wallet;
