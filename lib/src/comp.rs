use crate::Number;

pub trait Comparator: Default {}

/// Comparator for mask matching
#[derive(Debug, Copy, Clone)]
pub enum BasicComparator {
    AlwaysTrue,
    AlwaysFalse,
    Eq(Number),
    NotEq(Number),
}

impl Default for BasicComparator {
    fn default() -> Self {
        Self::AlwaysTrue
    }
}

impl Comparator for BasicComparator {}
