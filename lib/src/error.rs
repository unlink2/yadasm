#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ErrorKind {
    ParserFailed,
    OutOfData,
    NoMatch,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind }
    }
}
