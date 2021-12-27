use crate::Number;

pub trait Comparator: Default {
    fn compare(&self, other: &Number) -> bool;
}

/// Comparator for mask matching
#[derive(Debug, Clone)]
pub enum BasicComparator {
    AlwaysTrue,
    AlwaysFalse,
    Eq(Number),
    NotEq(Number),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
}

impl Default for BasicComparator {
    fn default() -> Self {
        Self::AlwaysTrue
    }
}

impl Comparator for BasicComparator {
    fn compare(&self, other: &Number) -> bool {
        match self {
            Self::AlwaysTrue => true,
            Self::AlwaysFalse => false,
            Self::Eq(num) => num == other,
            Self::NotEq(num) => num != other,
            Self::And(c1, c2) => c1.compare(other) && c2.compare(other),
            Self::Or(c1, c2) => c1.compare(other) || c2.compare(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{IntBuilder, IntKind};

    use super::*;

    #[test]
    fn it_should_always_be_true() {
        let c = BasicComparator::AlwaysTrue;
        assert!(c.compare(&Number::default()));
    }

    #[test]
    fn it_should_always_be_false() {
        let c = BasicComparator::AlwaysFalse;
        assert!(!c.compare(&Number::default()));
    }

    #[test]
    fn it_should_compare_eq_numbers() {
        let c = BasicComparator::Eq(Number::default());
        assert!(c.compare(&Number::default()));
    }

    #[test]
    fn it_should_compare_ne_numbers() {
        let c = BasicComparator::NotEq(Number::Int(
            IntBuilder::default().kind(IntKind::U16(0)).build().unwrap(),
        ));
        assert!(c.compare(&Number::default()));
    }

    #[test]
    fn it_should_use_and_success() {
        let c1 = Box::new(BasicComparator::AlwaysTrue);
        let c2 = Box::new(BasicComparator::AlwaysTrue);

        let c = BasicComparator::And(c1, c2);

        assert!(c.compare(&Number::default()));
    }

    #[test]
    fn it_should_use_and_fail() {
        let c1 = Box::new(BasicComparator::AlwaysFalse);
        let c2 = Box::new(BasicComparator::AlwaysTrue);

        let c = BasicComparator::And(c1, c2);

        assert!(!c.compare(&Number::default()));
    }

    #[test]
    fn it_should_use_or_success() {
        let c1 = Box::new(BasicComparator::AlwaysFalse);
        let c2 = Box::new(BasicComparator::AlwaysTrue);

        let c = BasicComparator::Or(c1, c2);

        assert!(c.compare(&Number::default()));
    }

    #[test]
    fn it_should_use_or_fail() {
        let c1 = Box::new(BasicComparator::AlwaysFalse);
        let c2 = Box::new(BasicComparator::AlwaysFalse);

        let c = BasicComparator::Or(c1, c2);

        assert!(!c.compare(&Number::default()));
    }
}
