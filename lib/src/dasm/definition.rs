use serde::{Serialize, Deserialize};

use crate::dasm::Word;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Definition {
    pub text: String,
    pub value: Word,
    pub size: usize,
}

impl Definition {
    pub fn new(text: &str, value: Word, size: usize) -> Self {
        Self {
            text: text.into(),
            value,
            size,
        }
    }
}
