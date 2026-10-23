//! This test target checks configuration, health responses, and dependency access.
//! It uses controlled node responses and disposable databases instead of production services.

mod accounts;
mod accounts_rpc;
mod balances;
mod blocks;
mod catalogs;
mod config;
mod discovery;
mod health;
mod network;
mod postgres;
mod sdk;
mod staking;
mod support;

mod wallet;
