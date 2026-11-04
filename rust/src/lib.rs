//! This library supplies startup and wallet-facing HTTP features for the Wallet Proxy.
//! The existing Haskell service remains available during development.

pub mod accounts;
pub mod balances;
pub mod blocks;
pub mod catalogs;
pub mod cis2;
mod compatibility;
pub mod config;
pub mod database;
pub mod dependencies;
pub mod discovery;
pub mod faucet;
pub mod fees;
pub mod health;
pub mod history;
pub mod http;
pub mod network;
pub mod node;
pub mod plt;
pub mod progress;
pub mod staking;
pub mod startup;
pub mod submission;
pub mod transak;
pub mod wallet;
