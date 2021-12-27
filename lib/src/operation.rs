use crate::{Error, Int, Number};

pub trait Operation: Default {
    fn apply(&self, other: &Number) -> Result<Number, Error>;
}

/// Operation to be applied for each mask
#[derive(Debug, Clone)]
pub enum BasicOperation {
    And(Int),
    Or(Int),
    Xor(Int),
    Add(Number),
    Sub(Number),
    Div(Number),
    Mul(Number),
    Mod(Number),
    Collection(Vec<BasicOperation>),
}

impl Default for BasicOperation {
    fn default() -> Self {
        Self::And(Int::default())
    }
}

impl Operation for BasicOperation {
    fn apply(&self, other: &Number) -> Result<Number, Error> {
        todo!()
    }
}
