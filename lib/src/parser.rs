use crate::{Binary, Error};
use crate::{Comparator, Context, Float, Integer, Node, Number, Operation, Response};

/// collection of masks, returns a string in the end
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct Parser {
    /// Opcode parser must match the operation before the data
    /// is matched
    opcode: Node,

    /// Data is matched in order
    data: Vec<Node>,
}

impl Parser {
    pub fn parse(&self, ctx: &Context, binary: &mut Binary) -> String {
        todo!()
    }
}
