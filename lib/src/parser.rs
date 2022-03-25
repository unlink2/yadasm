use std::collections::HashMap;

use crate::{Context, Error, ErrorKind, Node, Parsed, ReadOp, Word};

pub type NodeLookup = HashMap<Word, Node>;

pub type BuildLookup = fn(nodes: &[Node], read: usize) -> NodeLookup;

pub fn build_lookup(nodes: &[Node], read: usize) -> NodeLookup {
    let mut lookup = NodeLookup::default();

    for i in 0..2_u64.pow(read as u32 * 8) {
        for node in nodes {
            if (node.comparator.borrow())(i as Word) {
                lookup.insert(i, node.clone());
            }
        }
    }

    lookup
}

#[derive(Clone)]
pub struct Arch {
    pub lookup: NodeLookup,
    pub default: Option<Node>,
    pub read: usize,
    pub reader: ReadOp,
}

impl Arch {
    pub fn new(
        nodes: &[Node],
        build_lookup: BuildLookup,
        default: Option<Node>,
        read: usize,
        reader: ReadOp,
    ) -> Self {
        Self {
            lookup: (build_lookup)(nodes, read),
            default,
            read,
            reader,
        }
    }

    pub fn lookup_node(&self, dat: Word) -> Option<&Node> {
        match self.lookup.get(&dat) {
            Some(node) => Some(node),
            None => self.default.as_ref(),
        }
    }
}

impl Parser for Arch {
    fn parse(&self, ctx: &mut Context, bin: &[u8], next: &[impl Parser]) -> Result<Parsed, Error> {
        // read the opcode
        let opcode = match (self.reader)(self.read, bin) {
            Some(opcode) => opcode,
            None => return Err(Error::new(ErrorKind::OutOfData)),
        };

        // find a corresponding node
        let node = match self.lookup_node(opcode) {
            Some(node) => node,
            None => return self.default_parse(ctx, bin, self.tail(next)),
        };

        node.parse(ctx, bin)
    }
}

// Trait for any parser
pub trait Parser {
    /// Default implementation just calls the next parser in line
    /// or errors if no parsers are left
    fn parse(&self, ctx: &mut Context, bin: &[u8], next: &[impl Parser]) -> Result<Parsed, Error>;

    fn default_parse(
        &self,
        ctx: &mut Context,
        bin: &[u8],
        next: &[impl Parser],
    ) -> Result<Parsed, Error> {
        if let Some(frst) = next.get(0) {
            frst.parse(ctx, bin, self.tail(next))
        } else {
            Err(Error::new(ErrorKind::ParserFailed))
        }
    }

    fn tail<'a, T>(&self, next: &'a [T]) -> &'a [T] {
        if !next.is_empty() {
            &next[1..]
        } else {
            &[]
        }
    }
}

fn parse_pass(
    ctx: &mut Context,
    bin: &[u8],
    parsers: &[impl Parser],
) -> Result<Vec<Parsed>, Error> {
    let mut offset = 0;
    let mut parsed = vec![];

    // if there are no parsers... there is nothing to parse
    if parsers.is_empty() {
        Ok(parsed)
    } else {
        while !ctx.is_at_end() && !bin[offset..].is_empty() {
            let next = parsers[0].parse(ctx, &bin[offset..], &parsers[1..])?;
            offset += next.size();
            ctx.advance(next.size() as Word);
            parsed.push(next);
        }

        Ok(parsed)
    }
}

pub fn parse(ctx: &mut Context, bin: &[u8], parsers: &[impl Parser]) -> Result<Vec<Parsed>, Error> {
    // pass 1 -> build symbol database
    parse_pass(ctx, bin, parsers)?;
    ctx.reset();

    // pass 2 -> parse actual results
    parse_no_symbols(ctx, bin, parsers)
}

pub fn parse_no_symbols(
    ctx: &mut Context,
    bin: &[u8],
    parsers: &[impl Parser],
) -> Result<Vec<Parsed>, Error> {
    // pass 2 -> parse actual results
    parse_pass(ctx, bin, parsers)
}

pub fn parse_to_strings(
    ctx: &mut Context,
    bin: &[u8],
    parsers: &[impl Parser],
) -> Result<Vec<String>, Error> {
    let parsed = parse(ctx, bin, parsers)?;

    let mut strs = vec![];
    for p in parsed {
        strs.push(p.output(ctx, "", "", "", ":"));
    }
    Ok(strs)
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{readnle, Symbol, SymbolAttributes, Token, TokenAttributes};

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

    fn test_comparator4(dat: Word) -> bool {
        dat == 4
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
                ctx.add_first_symbol(Symbol::new(
                    "Test3:",
                    ctx.address,
                    0,
                    crate::SymbolAttributes::NewLine,
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
        n.push(Node::new(1, 1, readnle, &test_converter, test_comparator3));

        n
    }

    fn test_node3() -> Node {
        Node::new(1, 1, readnle, &test_converter, test_comparator4)
    }

    fn test_arch() -> Arch {
        Arch::new(
            &[test_node1(), test_node2(), test_node3()],
            build_lookup,
            None,
            1,
            readnle,
        )
    }

    fn test_arch_default() -> Arch {
        Arch::new(
            &[test_node1(), test_node2(), test_node3()],
            build_lookup,
            Some(test_node3()),
            1,
            readnle,
        )
    }

    #[test]
    fn it_should_build_lookup() {
        let arch = test_arch();

        let mut expected = HashMap::default();
        expected.insert(1, test_node1());
        expected.insert(2, test_node2());
        expected.insert(4, test_node3());

        assert_eq!(expected, arch.lookup);
    }

    #[test]
    fn it_should_find_correct_node() {
        let arch = test_arch();

        assert_eq!(Some(&test_node1()), arch.lookup_node(1), "Without defaults");

        let arch_def = test_arch_default();
        assert_eq!(
            Some(&test_node3()),
            arch_def.lookup_node(0xFF),
            "With defaults"
        );
        assert_eq!(
            Some(&test_node1()),
            arch_def.lookup_node(1),
            "With defaults"
        );

        assert_eq!(None, arch.lookup_node(0xFF), "Not found");
    }

    #[test]
    fn it_should_parse_input_nodes() {
        let mut ctx = test_context();
        let arch = test_arch();
        let data = [2, 3, 1, 1, 2, 3, 1];

        let expected = vec![
            Parsed {
                tokens: vec![
                    Token {
                        text: "test2".into(),
                        size: 1,
                        raw: 2,
                        ptr: None,
                        address: None,
                        attr: TokenAttributes::Std,
                        prefix_rep: 1,
                        postfix_rep: 1,
                    },
                    Token {
                        text: "test3".into(),
                        size: 1,
                        raw: 3,
                        ptr: None,
                        address: None,
                        attr: TokenAttributes::Std,
                        prefix_rep: 1,
                        postfix_rep: 1,
                    },
                ],
            },
            Parsed {
                tokens: vec![Token {
                    text: "test1".into(),
                    size: 1,
                    raw: 1,
                    ptr: None,
                    address: None,
                    attr: TokenAttributes::Std,
                    prefix_rep: 1,
                    postfix_rep: 1,
                }],
            },
            Parsed {
                tokens: vec![Token {
                    text: "test1".into(),
                    size: 1,
                    raw: 1,
                    ptr: None,
                    address: None,
                    attr: TokenAttributes::Std,
                    prefix_rep: 1,
                    postfix_rep: 1,
                }],
            },
            Parsed {
                tokens: vec![
                    Token {
                        text: "test2".into(),
                        size: 1,
                        raw: 2,
                        ptr: None,
                        address: None,
                        attr: TokenAttributes::Std,
                        prefix_rep: 1,
                        postfix_rep: 1,
                    },
                    Token {
                        text: "test3".into(),
                        size: 1,
                        raw: 3,
                        ptr: None,
                        address: None,
                        attr: TokenAttributes::Std,
                        prefix_rep: 1,
                        postfix_rep: 1,
                    },
                ],
            },
            Parsed {
                tokens: vec![Token {
                    text: "test1".into(),
                    size: 1,
                    raw: 1,
                    ptr: None,
                    address: None,
                    attr: TokenAttributes::Std,
                    prefix_rep: 1,
                    postfix_rep: 1,
                }],
            },
        ];

        let mut symbols = HashMap::default();
        symbols.insert(
            256,
            vec![Symbol {
                attr: SymbolAttributes::NewLine,
                address: 256,
                name: "Test3:".into(),
                order: 0,
                prefix_rep: 1,
                postfix_rep: 1,
            }],
        );
        symbols.insert(
            260,
            vec![Symbol {
                attr: SymbolAttributes::NewLine,
                address: 260,
                name: "Test3:".into(),
                order: 0,
                prefix_rep: 1,
                postfix_rep: 1,
            }],
        );

        assert_eq!(
            Ok(expected),
            parse(&mut ctx, &data, &[arch]),
            "Parser output"
        );
        assert_eq!(symbols, ctx.symbols, "Symbol table");
    }

    #[test]
    fn it_should_fail_with_error() {
        let mut ctx = test_context();
        let data_out_of_data = [4, 2];
        let data_parser_failed = [5, 2];

        assert_eq!(
            Err(Error::new(ErrorKind::OutOfData)),
            parse(&mut ctx, &data_out_of_data, &[test_arch()])
        );
        ctx.reset();
        assert_eq!(
            Err(Error::new(ErrorKind::ParserFailed)),
            parse(&mut ctx, &data_parser_failed, &[test_arch()])
        );
    }
}
