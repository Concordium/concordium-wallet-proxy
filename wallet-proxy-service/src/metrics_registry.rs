use parking_lot::Mutex;
use prometheus_client::registry::Registry;
use std::sync::LazyLock;

pub static REGISTRY: LazyLock<Mutex<Registry>> = LazyLock::new(|| Mutex::new(Registry::default()));
