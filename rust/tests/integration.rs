//! This test target checks configuration, health responses, and dependency access.
//! It uses controlled node responses and disposable databases instead of production services.

mod config;
mod health;
mod postgres;
mod sdk;
mod support;
