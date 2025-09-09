use parking_lot::Mutex;
use prometheus_client::registry::Registry;
use std::sync::LazyLock;

/// The prometheus metrics registry for the service
pub static REGISTRY: LazyLock<Mutex<Registry>> = LazyLock::new(|| Mutex::new(Registry::default()));
