use crate::{Binary, Error};
use crate::{Comparator, Context, Integer, Operation, Response};

/// Combination of operation, kind and return value
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct Node {
    prefix: String, // prefix for the parser
    // postfix operations that are applied to the read data e.g.
    // data mask & = result1 (masked bits)
    // result1 2 << = result2 (shifted bitwise)
    op: Vec<Operation>,
    // kind reads from the stream and returns the apropriate number datatype
    mask: Integer,
    // after every operation was applied the comparator needs to pass for the match
    // to succeed
    comparator: Comparator,
    response: Response,
}

impl Node {
    pub fn parse(&self, ctx: &Context, binary: &mut Binary) -> Result<String, Error> {
        todo!()
    }

    pub fn matches(&self, binary: &mut Binary) -> bool {
        todo!()
    }
}
