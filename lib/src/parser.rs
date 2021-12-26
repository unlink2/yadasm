use crate::{BasicContext, Comparator, Float, Int, Node, Number, Operation, Response};
use crate::{Binary, Error};

pub trait Parser {
    type Response;
    type Context;

    fn parse(&self, ctx: &Self::Context, binary: &mut dyn Binary) -> Self::Response;
}

/// collection of masks, returns a string in the end
#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct BasicParser<TResponse, TComparator, TOperation>
where
    TResponse: Response,
    TComparator: Comparator,
    TOperation: Operation,
{
    /// Opcode parser must match the operation before the data
    /// is matched
    opcode: Node<TResponse, TComparator, TOperation>,

    /// Data is matched in order
    data: Vec<Node<TResponse, TComparator, TOperation>>,
}

impl<TResponse, TComparator, TOperation> Parser for BasicParser<TResponse, TComparator, TOperation>
where
    TResponse: Response,
    TComparator: Comparator,
    TOperation: Operation,
{
    type Response = String;
    type Context = BasicContext;

    fn parse(&self, ctx: &BasicContext, binary: &mut dyn Binary) -> String {
        todo!()
    }
}
