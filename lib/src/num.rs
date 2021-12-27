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

impl NumberFormat<IntKind> for IntFormat {}

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

impl NumberKind for IntKind {}

impl Default for IntKind {
    fn default() -> Self {
        Self::U8(0)
    }
}

#[derive(Debug, Builder, Copy, Clone, Default)]
#[builder(setter(into))]
pub struct Num<TKind, TFormat>
where
    TKind: NumberKind,
    TFormat: NumberFormat<TKind>,
{
    #[builder(default)]
    format: TFormat,
    #[builder(default)]
    kind: TKind,
    #[builder(default)]
    endianess: Endianness,
}

impl<TKind, TFormat> PartialEq for Num<TKind, TFormat>
where
    TKind: NumberKind,
    TFormat: NumberFormat<TKind>,
{
    fn eq(&self, other: &Num<TKind, TFormat>) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatKind {
    F32(f32),
    F64(f64),
}

impl NumberKind for FloatKind {}

impl Default for FloatKind {
    fn default() -> Self {
        Self::F32(0.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum FloatFormat {
    Full,
    Scientific,
    Precision(usize),
}

impl NumberFormat<FloatKind> for FloatFormat {}

impl Default for FloatFormat {
    fn default() -> Self {
        Self::Full
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(Num<IntKind, IntFormat>),
    Float(Num<FloatKind, FloatFormat>),
}

impl Default for Number {
    fn default() -> Self {
        Self::Int(Num::<IntKind, IntFormat>::default())
    }
}

pub type Int = Num<IntKind, IntFormat>;
pub type IntBuilder = NumBuilder<IntKind, IntFormat>;
pub type Float = Num<FloatKind, FloatFormat>;
pub type FloatBuilder = Num<FloatKind, FloatFormat>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_compare_ints() {
        let i1 = NumBuilder::<IntKind, IntFormat>::default()
            .kind(IntKind::U16(100))
            .build()
            .unwrap();
        let i2 = NumBuilder::default()
            .kind(IntKind::U16(100))
            .build()
            .unwrap();

        let i3 = NumBuilder::default()
            .kind(IntKind::U32(100))
            .build()
            .unwrap();

        assert_eq!(i1, i2);

        assert_ne!(i1, i3);
    }

    #[test]
    fn it_should_compare_floats() {
        let f1 = NumBuilder::<FloatKind, FloatFormat>::default()
            .kind(FloatKind::F32(100.0))
            .build()
            .unwrap();
        let f2 = NumBuilder::default()
            .kind(FloatKind::F32(100.0))
            .build()
            .unwrap();

        let f3 = NumBuilder::default()
            .kind(FloatKind::F32(101.0))
            .build()
            .unwrap();

        assert_eq!(f1, f2);

        assert_ne!(f1, f3);
    }
}
