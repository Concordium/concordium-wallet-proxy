//! This module selects the concrete dependency types for the application.
//! Each feature sets the capability bounds that it requires.

use crate::{database::PostgresHistory, health::SystemClock, node::SdkNode};

/// This interface groups dependency types without requiring unused capabilities.
pub trait Dependencies {
    /// This type supplies node operations and shares its connection when cloned.
    type Node: Clone;
    /// This type supplies transaction-history access.
    type History;
    /// This type supplies the current time.
    type Clock;
}

/// This type selects the production dependency implementations.
pub struct Production;

impl Dependencies for Production {
    type Node = SdkNode;
    type History = PostgresHistory;
    type Clock = SystemClock;
}
