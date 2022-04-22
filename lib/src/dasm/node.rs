use std::{cell::RefCell, rc::Rc};

use crate::dasm::{Comparator, Context, Error, ErrorKind, Parsed, ReadOp, Word};

type Converter = dyn Fn(&mut Context, Word, usize) -> Parsed;

#[derive(Clone)]
pub struct Node {
    pub children: Vec<Node>,
    pub reader: ReadOp,
    pub size: usize,
    pub read: usize,
    pub converter: Rc<RefCell<Converter>>,
    pub comparator: Rc<RefCell<Comparator<Word>>>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.children == other.children && self.size == other.size && self.read == other.read
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("children", &self.children)
            .field("size", &self.size)
            .field("read", &self.read)
            .finish()
    }
}

impl Node {
    pub fn new(
        size: usize,
        read: usize,
        reader: ReadOp,
        converter: impl Fn(&mut Context, Word, usize) -> Parsed + 'static,
        comparator: impl Fn(Word) -> bool + 'static,
    ) -> Self {
        Self::with_children(size, read, reader, converter, comparator, &[])
    }

    pub fn with_children(
        size: usize,
        read: usize,
        reader: ReadOp,
        converter: impl Fn(&mut Context, Word, usize) -> Parsed + 'static,
        comparator: impl Fn(Word) -> bool + 'static,
        children: &[Node],
    ) -> Self {
        Self {
            children: children.to_vec(),
            size,
            reader,
            read,
            converter: Rc::new(RefCell::new(converter)),
            comparator: Rc::new(RefCell::new(comparator)),
        }
    }

    pub fn push(&mut self, child: Node) {
        self.children.push(child)
    }

    fn parse_with(&self, ctx: &mut Context, bin: &[u8], parsed: &mut Parsed) -> Result<(), Error> {
        let read = match (self.reader)(self.read, bin) {
            Some(read) => read,
            None => return Err(Error::new(ErrorKind::OutOfData)),
        };
        if (self.comparator.borrow())(read) {
            parsed.append((self.converter.borrow())(ctx, read, self.size));

            for c in &self.children {
                match c.parse(ctx, &bin[parsed.size()..]) {
                    Err(err) => return Err(err),
                    Ok(p) => parsed.append(p),
                }
            }
            Ok(())
        } else {
            Err(Error::new(ErrorKind::NoMatch))
        }
    }

    pub fn parse(&self, ctx: &mut Context, bin: &[u8]) -> Result<Parsed, Error> {
        let mut parsed = Parsed::new(vec![]);

        match self.parse_with(ctx, bin, &mut parsed) {
            Err(err) => Err(err),
            Ok(_) => Ok(parsed),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::dasm::{readnle, Symbol, Token, TokenAttributes};

    use super::*;

    fn test_context() -> Context {
        let ctx = Context::new(0x100, 0x110);

        ctx
    }

    fn test_comparator1(dat: Word) -> bool {
        dat == 1
    }

    fn test_comparator2(dat: Word) -> bool {
        dat == 2
    }

    fn test_comparator3(dat: Word) -> bool {
        dat == 3
    }

    fn test_converter(ctx: &mut Context, dat: Word, size: usize) -> Parsed {
        match dat {
            1 => Parsed::new(vec![Token::new(
                "test1",
                size,
                dat,
                TokenAttributes::Std,
                None,
            )]),
            2 => Parsed::new(vec![Token::new(
                "test2",
                size,
                dat,
                TokenAttributes::Std,
                None,
            )]),
            _ => {
                ctx.add_symbol(Symbol::new(
                    "Test3:",
                    ctx.address,
                    0,
                    crate::dasm::SymbolAttributes::NewLine,
                ));
                Parsed::new(vec![Token::new(
                    "test3",
                    size,
                    dat,
                    TokenAttributes::Std,
                    None,
                )])
            }
        }
    }

    fn test_node1() -> Node {
        Node::new(1, 1, readnle, &test_converter, test_comparator1)
    }

    fn test_node2() -> Node {
        let mut n = test_node1();
        n.comparator = Rc::new(RefCell::new(test_comparator2));
        n.push(Node::new(2, 1, readnle, &test_converter, test_comparator3));

        n
    }

    #[test]
    fn it_should_parse_valid_input() {
        let mut ctx = test_context();
        let node = test_node1();
        assert_eq!(
            Ok(Parsed::new(vec![Token::new(
                "test1",
                1,
                1,
                TokenAttributes::Std,
                None
            )])),
            node.parse(&mut ctx, &[1])
        );
    }

    #[test]
    fn it_should_parse_valid_input_with_children() {
        let mut ctx = test_context();
        let node = test_node2();

        assert_eq!(
            Ok(Parsed::new(vec![
                Token::new("test2", 1, 2, TokenAttributes::Std, None),
                Token::new("test3", 2, 3, TokenAttributes::Std, None)
            ])),
            node.parse(&mut ctx, &[2, 3, 2, 3, 2, 3, 2, 3])
        );
        assert_eq!(
            Some(&vec![Symbol::new(
                "Test3:",
                0x100,
                0,
                crate::dasm::SymbolAttributes::NewLine
            )]),
            ctx.symbols.get(&0x100),
            "Expected symbols"
        );
    }

    #[test]
    fn it_should_fail_if_out_of_data() {
        let mut ctx = test_context();
        let node = test_node1();
        assert_eq!(
            Err(Error::new(ErrorKind::OutOfData)),
            node.parse(&mut ctx, &[])
        );
    }

    #[test]
    fn it_should_fail_if_out_of_data_with_chidlren() {
        let mut ctx = test_context();
        let node = test_node2();
        assert_eq!(
            Err(Error::new(ErrorKind::OutOfData)),
            node.parse(&mut ctx, &[2])
        );
    }

    #[test]
    fn it_should_fail_if_not_a_match() {
        let mut ctx = test_context();
        let node = test_node2();
        assert_eq!(
            Err(Error::new(ErrorKind::NoMatch)),
            node.parse(&mut ctx, &[2, 1])
        );
    }
}
