use crate::{Integer, Number};

/// Operation to be applied for each mask
#[derive(Debug, Copy, Clone)]
pub enum Operation {
    And(Integer),
    Or(Integer),
    Xor(Integer),
    Add(Number),
    Sub(Number),
    Div(Number),
    Mul(Number),
    Mod(Number),
}

impl Default for Operation {
    fn default() -> Self {
        Self::And(Integer::default())
    }
}
