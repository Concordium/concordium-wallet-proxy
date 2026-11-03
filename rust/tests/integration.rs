//! This test target checks configuration, health responses, and dependency access.
//! It uses controlled node responses and disposable databases instead of production services.

mod accounts;
mod accounts_rpc;
mod balances;
mod blocks;
mod catalogs;
mod cis2;
mod config;
mod discovery;
mod fees;
mod health;
mod history;
mod network;
mod postgres;
mod progress;
mod sdk;
mod staking;
mod submission;
mod support;

mod wallet;
