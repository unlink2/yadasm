use crate::{Integer, Number};

pub trait Operation: Default {}

/// Operation to be applied for each mask
#[derive(Debug, Copy, Clone)]
pub enum BasicOperation {
    And(Integer),
    Or(Integer),
    Xor(Integer),
    Add(Number),
    Sub(Number),
    Div(Number),
    Mul(Number),
    Mod(Number),
}

impl Default for BasicOperation {
    fn default() -> Self {
        Self::And(Integer::default())
    }
}

impl Operation for BasicOperation {}
