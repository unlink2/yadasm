use crate::dasm::Word;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ErrorKind {
    ParserFailed,
    OutOfData,
    NoMatch,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub address: Option<Word>,
    pub value: Option<u8>
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind, address: None, value: None }
    }

    pub fn set_address(mut self, addr: Word) -> Self {
        self.address = Some(addr);
        self
    }

    pub fn set_value(mut self, value: u8) -> Self {
        self.value = Some(value);
        self
    }
}
