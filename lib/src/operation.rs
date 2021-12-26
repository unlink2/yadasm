use crate::{Int, Num, Number};

pub trait Operation: Default {
    fn apply(&self, other: &Number) -> Number;
}

/// Operation to be applied for each mask
#[derive(Debug, Copy, Clone)]
pub enum BasicOperation {
    And(Int),
    Or(Int),
    Xor(Int),
    Add(Number),
    Sub(Number),
    Div(Number),
    Mul(Number),
    Mod(Number),
}

impl Default for BasicOperation {
    fn default() -> Self {
        Self::And(Int::default())
    }
}

impl Operation for BasicOperation {
    fn apply(&self, other: &Number) -> Number {
        todo!()
    }
}
