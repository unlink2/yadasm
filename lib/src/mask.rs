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
pub enum MaskResponse {
    None,
    Static(String),
    Decimal,
    Hex,
    Binary,
    Octal,
}

impl Default for MaskResponse {
    fn default() -> Self {
        Self::None
    }
}

/// Combination of operation, kind and return value
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct MaskParser {
    op: MaskOperation,
    kind: MaskKind,
    response: MaskResponse,
}

/// collection of masks, returns a string in the end!
pub struct Mask {
    /// Opcode parser must match the operation before the data
    /// is matched
    opcode: MaskParser,

    /// Data is matched in order
    data: Vec<MaskParser>,
}
