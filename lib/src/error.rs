#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    TypeError,
    MathError,
    ErrorAt(usize),
}
