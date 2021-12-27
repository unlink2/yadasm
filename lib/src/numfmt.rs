use crate::Float;
use crate::Int;

pub trait NumberKind: Default + Copy + Clone + PartialEq {}
pub trait NumberFormat<TNum>: Default + Copy + Clone {}

#[derive(Debug, Copy, Clone)]
pub enum Endianness {
    Big,
    Little,
}

impl Default for Endianness {
    fn default() -> Self {
        // contorversial default choice
        Self::Little
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IntFormat {
    Octal,
    Hex,
    Decimal,
    Binary,
}

impl NumberFormat<Int> for IntFormat {}

impl Default for IntFormat {
    fn default() -> Self {
        Self::Binary
    }
}

#[derive(Debug, Copy, Clone)]
pub enum FloatFormat {
    Full,
    Scientific,
    Precision(usize),
}

impl NumberFormat<Float> for FloatFormat {}

impl Default for FloatFormat {
    fn default() -> Self {
        Self::Full
    }
}
