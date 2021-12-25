use crate::Binary;
use crate::{Float, Integer, Number};

/// Mask size
#[derive(Debug, Copy, Clone)]
pub enum MaskKind {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

impl Default for MaskKind {
    fn default() -> Self {
        Self::U8(0)
    }
}

/// Comparator for mask matching
#[derive(Debug, Copy, Clone)]
pub enum MaskComparator {
    AlwaysTrue,
    AlwaysFalse,
    Eq(Number),
    NotEq(Number),
}

impl Default for MaskComparator {
    fn default() -> Self {
        Self::AlwaysTrue
    }
}

/// Operation to be applied for each mask
/// TODO maybe implement a real math parser as an option? (Do we need it?)
#[derive(Debug, Copy, Clone)]
pub enum MaskOperation {
    And,
    Or,
    Xor,
    Not,
}

impl Default for MaskOperation {
    fn default() -> Self {
        Self::And
    }
}

#[derive(Debug, Clone)]
pub enum MaskResponseKind {
    None,
    Static(String),
    Number(Number),
    Str,
}

impl Default for MaskResponseKind {
    fn default() -> Self {
        Self::None
    }
}

/// Combination of operation, kind and return value
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct MaskParser {
    prefix: String, // prefix for the parser
    op: MaskOperation,
    kind: MaskKind,
    comparator: MaskComparator,
    response: MaskResponseKind,
}

impl MaskParser {
    pub fn parse(&self, binary: &mut Binary) -> String {
        todo!()
    }

    pub fn matches(&self, binary: &mut Binary) -> bool {
        todo!()
    }
}

/// collection of masks, returns a string in the end
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct Mask {
    /// Opcode parser must match the operation before the data
    /// is matched
    opcode: MaskParser,

    /// Data is matched in order
    data: Vec<MaskParser>,
}

impl Mask {
    pub fn parse(&self, binary: &mut Binary) -> String {
        todo!()
    }
}
