use crate::{Context, Symbol, Word};

#[derive(Clone, Debug, PartialEq, Eq)]
struct Node {
    pub tokens: Vec<Token>,
    pub symbols: Vec<Symbol>,
}

impl Node {
    pub fn output(&self, ctx: &Context, connector: &str, end: &str) -> String {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenAttributes {
    Std,
    NewLine,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub text: String,
    pub size: usize,
    pub raw: Word,
    pub ptr: Option<Word>,
    pub attr: TokenAttributes,
    pub connector: u16,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl Token {}

pub trait Line {
    fn size(&self) -> usize;
}

impl Line for [Token] {
    /// Calculates the total length of a list of tokens
    fn size(&self) -> usize {
        self.iter().fold(0, |prev, x| prev + x.size)
    }
}
