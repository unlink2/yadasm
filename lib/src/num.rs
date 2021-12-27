use crate::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Int {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
}

impl Int {}

impl Default for Int {
    fn default() -> Self {
        Self::U8(0)
    }
}

impl std::ops::Add<Int> for Int {
    type Output = Result<Int, Error>;

    fn add(self, rhs: Int) -> Result<Int, Error> {
        match (self, rhs) {
            (Self::U8(l), Self::U8(r)) => Ok(Self::U8(l + r)),
            (Self::U16(l), Self::U16(r)) => Ok(Self::U16(l + r)),
            (Self::U32(l), Self::U32(r)) => Ok(Self::U32(l + r)),
            (Self::U64(l), Self::U64(r)) => Ok(Self::U64(l + r)),
            (Self::U128(l), Self::U128(r)) => Ok(Self::U128(l + r)),
            (Self::I8(l), Self::I8(r)) => Ok(Self::I8(l + r)),
            (Self::I16(l), Self::I16(r)) => Ok(Self::I16(l + r)),
            (Self::I32(l), Self::I32(r)) => Ok(Self::I32(l + r)),
            (Self::I64(l), Self::I64(r)) => Ok(Self::I64(l + r)),
            (Self::I128(l), Self::I128(r)) => Ok(Self::I128(l + r)),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::ops::Sub<Int> for Int {
    type Output = Result<Int, Error>;

    fn sub(self, rhs: Int) -> Result<Int, Error> {
        match (self, rhs) {
            (Self::U8(l), Self::U8(r)) => Ok(Self::U8(l - r)),
            (Self::U16(l), Self::U16(r)) => Ok(Self::U16(l - r)),
            (Self::U32(l), Self::U32(r)) => Ok(Self::U32(l - r)),
            (Self::U64(l), Self::U64(r)) => Ok(Self::U64(l - r)),
            (Self::U128(l), Self::U128(r)) => Ok(Self::U128(l - r)),
            (Self::I8(l), Self::I8(r)) => Ok(Self::I8(l - r)),
            (Self::I16(l), Self::I16(r)) => Ok(Self::I16(l - r)),
            (Self::I32(l), Self::I32(r)) => Ok(Self::I32(l - r)),
            (Self::I64(l), Self::I64(r)) => Ok(Self::I64(l - r)),
            (Self::I128(l), Self::I128(r)) => Ok(Self::I128(l - r)),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::ops::Mul<Int> for Int {
    type Output = Result<Int, Error>;

    fn mul(self, rhs: Int) -> Result<Int, Error> {
        match (self, rhs) {
            (Self::U8(l), Self::U8(r)) => Ok(Self::U8(l * r)),
            (Self::U16(l), Self::U16(r)) => Ok(Self::U16(l * r)),
            (Self::U32(l), Self::U32(r)) => Ok(Self::U32(l * r)),
            (Self::U64(l), Self::U64(r)) => Ok(Self::U64(l * r)),
            (Self::U128(l), Self::U128(r)) => Ok(Self::U128(l * r)),
            (Self::I8(l), Self::I8(r)) => Ok(Self::I8(l * r)),
            (Self::I16(l), Self::I16(r)) => Ok(Self::I16(l * r)),
            (Self::I32(l), Self::I32(r)) => Ok(Self::I32(l * r)),
            (Self::I64(l), Self::I64(r)) => Ok(Self::I64(l * r)),
            (Self::I128(l), Self::I128(r)) => Ok(Self::I128(l * r)),
            _ => Err(Error::TypeError),
        }
    }
}

impl std::ops::Div<Int> for Int {
    type Output = Result<Int, Error>;

    fn div(self, rhs: Int) -> Result<Int, Error> {
        match (self, rhs) {
            (Self::U8(l), Self::U8(r)) => Ok(Self::U8(l / r)),
            (Self::U16(l), Self::U16(r)) => Ok(Self::U16(l / r)),
            (Self::U32(l), Self::U32(r)) => Ok(Self::U32(l / r)),
            (Self::U64(l), Self::U64(r)) => Ok(Self::U64(l / r)),
            (Self::U128(l), Self::U128(r)) => Ok(Self::U128(l / r)),
            (Self::I8(l), Self::I8(r)) => Ok(Self::I8(l / r)),
            (Self::I16(l), Self::I16(r)) => Ok(Self::I16(l / r)),
            (Self::I32(l), Self::I32(r)) => Ok(Self::I32(l / r)),
            (Self::I64(l), Self::I64(r)) => Ok(Self::I64(l / r)),
            (Self::I128(l), Self::I128(r)) => Ok(Self::I128(l / r)),
            _ => Err(Error::TypeError),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Float {
    F32(f32),
    F64(f64),
}

impl Default for Float {
    fn default() -> Self {
        Self::F32(0.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(Int),
    Float(Float),
}

impl Default for Number {
    fn default() -> Self {
        Self::Int(Int::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_compare_ints() {
        let i1 = Int::U16(100);
        let i2 = Int::U16(100);

        let i3 = Int::U32(100);

        assert_eq!(i1, i2);

        assert_ne!(i1, i3);
    }

    #[test]
    fn it_should_compare_floats() {
        let f1 = Float::F32(100.0);
        let f2 = Float::F32(100.0);

        let f3 = Float::F32(101.0);

        assert_eq!(f1, f2);

        assert_ne!(f1, f3);
    }

    #[test]
    fn it_should_add_int() {
        assert_eq!(Int::U16(100) + Int::U16(50), Ok(Int::U16(150)));
        assert_eq!(Int::U32(100) + Int::U16(50), Err(Error::TypeError));
    }

    #[test]
    fn it_should_sub_int() {
        assert_eq!(Int::U16(100) - Int::U16(50), Ok(Int::U16(50)));
        assert_eq!(Int::U32(100) - Int::U16(50), Err(Error::TypeError));
    }

    #[test]
    fn it_should_mul_int() {
        assert_eq!(Int::U16(100) * Int::U16(50), Ok(Int::U16(100 * 50)));
        assert_eq!(Int::U32(100) * Int::U16(50), Err(Error::TypeError));
    }

    #[test]
    fn it_should_div_int() {
        assert_eq!(Int::U16(100) / Int::U16(50), Ok(Int::U16(100 / 50)));
        assert_eq!(Int::U32(100) / Int::U16(50), Err(Error::TypeError));
        assert_eq!(Int::U16(100) / Int::U16(0), Err(Error::MathError));
    }
}
