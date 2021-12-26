use crate::Number;

/// Comparator for mask matching
#[derive(Debug, Copy, Clone)]
pub enum Comparator {
    AlwaysTrue,
    AlwaysFalse,
    Eq(Number),
    NotEq(Number),
}

impl Default for Comparator {
    fn default() -> Self {
        Self::AlwaysTrue
    }
}
