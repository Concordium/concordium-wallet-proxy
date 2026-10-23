//! This library supplies startup and wallet-facing HTTP features for the Wallet Proxy.
//! The existing Haskell service remains available during development.

pub mod accounts;
pub mod balances;
pub mod blocks;
pub mod catalogs;
mod compatibility;
pub mod config;
pub mod database;
pub mod dependencies;
pub mod discovery;
pub mod fees;
pub mod health;
pub mod http;
pub mod network;
pub mod node;
pub mod staking;
pub mod startup;
pub mod wallet;
