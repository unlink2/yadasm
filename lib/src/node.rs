use crate::{BasicContext, Comparator, Integer, Operation, Response};
use crate::{Binary, Error, Parser};

/// Combination of operation, kind and return value
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct Node<TResponse, TComparator, TOperation>
where
    TResponse: Response,
    TComparator: Comparator,
    TOperation: Operation,
{
    prefix: String, // prefix for the parser
    // postfix operations that are applied to the read data e.g.
    // data mask & = result1 (masked bits)
    // result1 2 << = result2 (shifted bitwise)
    op: Vec<TOperation>,
    // kind reads from the stream and returns the apropriate number datatype
    mask: Integer,
    // after every operation was applied the comparator needs to pass for the match
    // to succeed
    comparator: TComparator,
    response: TResponse,
}

impl<TResponse, TComparator, TOperation> Node<TResponse, TComparator, TOperation>
where
    TResponse: Response,
    TComparator: Comparator,
    TOperation: Operation,
{
    pub fn matches(&self, binary: &mut dyn Binary) -> bool {
        todo!()
    }
}

impl<TResponse, TComparator, TOperation> Parser for Node<TResponse, TComparator, TOperation>
where
    TResponse: Response,
    TComparator: Comparator,
    TOperation: Operation,
{
    type Response = Result<String, Error>;
    type Context = BasicContext;

    fn parse(&self, ctx: &BasicContext, binary: &mut dyn Binary) -> Result<String, Error> {
        todo!()
    }
}
