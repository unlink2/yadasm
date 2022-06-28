use serde::{Deserialize, Serialize};

use crate::dasm::Word;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq)]
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
