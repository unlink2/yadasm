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

impl Default for IntFormat {
    fn default() -> Self {
        Self::Binary
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IntKind {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    I8(i8),
    I16(i16),
    I32(i32),
    I128(i128),
}

impl Default for IntKind {
    fn default() -> Self {
        Self::U8(0)
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Integer {
    format: IntFormat,
    kind: IntKind,
    endianess: Endianness,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32(f32),
    F64(f64),
}

impl Default for FloatKind {
    fn default() -> Self {
        Self::F32(0.0)
    }
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Float {
    kind: FloatKind,
    endianess: Endianness,
}

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Integer(Integer),
    Float(Float),
}

impl Default for Number {
    fn default() -> Self {
        Self::Integer(Integer::default())
    }
}
