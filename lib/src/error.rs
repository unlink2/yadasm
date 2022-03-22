pub enum ErrorKind {
    ParserError,
    OutOfDataError,
}

pub struct Error {
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind }
    }
}
