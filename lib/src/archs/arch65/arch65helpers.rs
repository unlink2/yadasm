use crate::{
    always_true, build_lookup, readnle, Arch, Context, Node, Parsed, Symbol, SymbolAttributes,
    Token, TokenAttributes, Word,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InstModes {
    Immediate,     // dynamically sized
    ImmediateByte, // always 1 byte
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    Accumulator,
    Implied,
    Relative,
    AbsoluteJump,
    IndirectJump,
    ZeroPageIndirect,
    JumpAbsoluteX,
    StackS,
    DirectPageIndirectLong,
    AbsoluteLong,
    StackSY,
    DirectPageIndirectLongY,
    AbsoluteLongX,
    JumpAbsoluteLong,
    Per,
    BranchLong,
    Move,
    Jml,
    Jsrx,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImInfo {
    pub name: String,
    pub im: InstModes,
    pub opcode: Word,
}

impl ImInfo {
    pub fn new(mask: fn(InstModes, Word) -> Word, im: InstModes, name: &str, opcode: Word) -> Self {
        Self {
            name: name.into(),
            im,
            opcode: (mask)(im, opcode),
        }
    }

    pub fn to_node(&self, children: &[Node]) -> Node {
        opcode_node(&self.name, self.opcode, children)
    }
}

pub fn text_converter(text: &str, ctx: &mut Context, dat: Word, size: usize) -> Parsed {
    Parsed::new(vec![Token::new(
        text,
        size,
        dat,
        crate::TokenAttributes::Std,
        Some(ctx.address),
    )])
}

pub fn no_converter(ctx: &mut Context, dat: Word, size: usize) -> Parsed {
    Parsed::new(vec![Token::new(
        "",
        size,
        dat,
        crate::TokenAttributes::Std,
        Some(ctx.address),
    )])
}

pub fn number_converter(
    ftm: fn(dat: Word) -> String,
    prefix: &str,
    postfix: &str,
    attr: TokenAttributes,
    ctx: &mut Context,
    dat: Word,
    size: usize,
) -> Parsed {
    if let Some(def) = ctx.get_def(dat) {
        Parsed::new(vec![Token::new(
            &format!("{}{}{}", prefix, def.text, postfix),
            size,
            dat,
            attr,
            Some(ctx.address),
        )])
    } else {
        Parsed::new(vec![Token::new(
            &format!("{}{}{}", prefix, (ftm)(dat), postfix),
            size,
            dat,
            attr,
            Some(ctx.address),
        )])
    }
}

pub fn rel_addr_converter(ctx: &Context, dat: Word) -> Word {
    if dat & 0x80 > 0 {
        ctx.address - (!dat + 0xFF)
    } else {
        ctx.address + dat + 2
    }
}

pub fn rel_word_addr_converter(ctx: &Context, dat: Word) -> Word {
    if dat & 0x8000 > 0 {
        ctx.address - (!dat + 0xFFFF)
    } else {
        ctx.address + dat + 2
    }
}

pub fn abs_addr_converter(_ctx: &Context, dat: Word) -> Word {
    dat
}

pub fn label_converter(
    addr_converter: fn(&Context, Word) -> Word,
    ctx: &mut Context,
    dat: Word,
    size: usize,
) -> Parsed {
    let addr = addr_converter(ctx, dat);
    let name = if let Some(syms) = ctx.get_symbols(addr) {
        // symbols are guaranteed to have at least one entry if they exist
        syms[0].name.clone()
    } else {
        let name = format!("label_{:X}", addr);
        let sym = Symbol::new(&name, addr, 0xFF, SymbolAttributes::Std);
        ctx.add_symbol(sym);
        name
    };
    Parsed::new(vec![Token::new(
        &name,
        size,
        dat,
        TokenAttributes::Std,
        Some(ctx.address),
    )])
}

pub fn opcode_comparator(expected: Word, dat: Word) -> bool {
    expected == dat
}

pub fn append_string_node(text: &str) -> Node {
    let text = text.to_owned();
    Node::new(
        0,
        0,
        readnle,
        move |ctx, dat, size| text_converter(&text, ctx, dat, size),
        always_true,
    )
}

pub fn opcode_node(name: &str, opcode: Word, children: &[Node]) -> Node {
    let name = name.to_owned();

    Node::with_children(
        1,
        1,
        readnle,
        move |ctx, dat, size| text_converter(&name, ctx, dat, size),
        move |dat| opcode_comparator(opcode, dat),
        children,
    )
}

/// Consumes a byte of a certain value. Does not emit anything
pub fn consume_byte_node(opcode: Word, children: &[Node]) -> Node {
    Node::with_children(
        1,
        1,
        readnle,
        no_converter,
        move |dat| opcode_comparator(opcode, dat),
        children,
    )
}

pub fn read_char_node(
    prefix: &str,
    postfix: &str,
    attr: TokenAttributes,
    children: &[Node],
) -> Node {
    let prefix = prefix.to_owned();
    let postfix = postfix.to_owned();
    Node::with_children(
        1,
        1,
        readnle,
        move |ctx, dat, size| {
            number_converter(
                |dat| format!("{}", dat as u8 as char),
                &prefix,
                &postfix,
                attr,
                ctx,
                dat,
                size,
            )
        },
        always_true,
        children,
    )
}

pub fn read_byte_node(
    prefix: &str,
    postfix: &str,
    attr: TokenAttributes,
    children: &[Node],
) -> Node {
    let prefix = prefix.to_owned();
    let postfix = postfix.to_owned();
    Node::with_children(
        1,
        1,
        readnle,
        move |ctx, dat, size| {
            number_converter(
                |dat| format!("{:02X}", dat as u8),
                &prefix,
                &postfix,
                attr,
                ctx,
                dat,
                size,
            )
        },
        always_true,
        children,
    )
}

pub fn read_immediate_node(size: usize) -> Node {
    match size {
        1 => read_byte_node("#", "", TokenAttributes::Std, &[]),
        _ => read_word_node("#", "", TokenAttributes::Std, &[]),
    }
}

pub fn read_word_node(
    prefix: &str,
    postfix: &str,
    attr: TokenAttributes,
    children: &[Node],
) -> Node {
    let prefix = prefix.to_owned();
    let postfix = postfix.to_owned();
    Node::with_children(
        2,
        2,
        readnle,
        move |ctx, dat, size| {
            number_converter(
                |dat| format!("{:04X}", dat as u16),
                &prefix,
                &postfix,
                attr,
                ctx,
                dat,
                size,
            )
        },
        always_true,
        children,
    )
}

pub fn read_lword_node(
    prefix: &str,
    postfix: &str,
    attr: TokenAttributes,
    children: &[Node],
) -> Node {
    let prefix = prefix.to_owned();
    let postfix = postfix.to_owned();
    Node::with_children(
        3,
        3,
        readnle,
        move |ctx, dat, size| {
            number_converter(
                |dat| format!("{:06X}", dat as u32),
                &prefix,
                &postfix,
                attr,
                ctx,
                dat,
                size,
            )
        },
        always_true,
        children,
    )
}

pub fn read_rel_label_node() -> Node {
    Node::new(
        1,
        1,
        readnle,
        move |ctx, dat, size| label_converter(rel_addr_converter, ctx, dat, size),
        always_true,
    )
}

pub fn read_rel_word_label_node() -> Node {
    Node::new(
        2,
        2,
        readnle,
        move |ctx, dat, size| label_converter(rel_word_addr_converter, ctx, dat, size),
        always_true,
    )
}

pub fn read_abs_label_node() -> Node {
    Node::new(
        2,
        2,
        readnle,
        move |ctx, dat, size| label_converter(abs_addr_converter, ctx, dat, size),
        always_true,
    )
}

pub fn read_abs_long_label_node() -> Node {
    Node::new(
        3,
        3,
        readnle,
        move |ctx, dat, size| label_converter(abs_addr_converter, ctx, dat, size),
        always_true,
    )
}

pub fn extract_cc_bits(opcode: Word) -> Word {
    opcode & 0b00000011
}

pub fn apply_mask(opcode: Word) -> Word {
    opcode & 0b00011100
}

pub fn extract_opcode(opcode: Word) -> Word {
    opcode & 0b11100011
}

pub fn is_cc01(opcode: Word) -> bool {
    extract_cc_bits(opcode) == 0x01
}

pub fn is_cc10(opcode: Word) -> bool {
    extract_cc_bits(opcode) == 0x02
}

pub fn is_cc00(opcode: Word) -> bool {
    extract_cc_bits(opcode) == 0x00
}

pub fn is_cc11(opcode: Word) -> bool {
    extract_cc_bits(opcode) == 0x03
}

pub fn make_opcode(base: Word, opcode: Word) -> Word {
    apply_mask(base) | extract_opcode(opcode)
}

// alternative maks implementation
// for opcodes that break the rules and just
// need to be echoed
pub fn mask_echo(_im: InstModes, opcode: Word) -> Word {
    opcode
}

pub fn make_instruction(i: &ImInfo, immediate_size: usize) -> Node {
    match i.im {
        InstModes::Immediate => i.to_node(&[read_immediate_node(immediate_size)]),
        InstModes::ImmediateByte => i.to_node(&[read_immediate_node(1)]),

        InstModes::Relative => i.to_node(&[read_rel_label_node()]),
        InstModes::Implied => i.to_node(&[]),

        InstModes::ZeroPage => i.to_node(&[read_byte_node("", "", TokenAttributes::Std, &[])]),
        InstModes::ZeroPageX => i.to_node(&[read_byte_node("", ", x", TokenAttributes::Std, &[])]),
        InstModes::ZeroPageY => i.to_node(&[read_byte_node("", ", y", TokenAttributes::Std, &[])]),

        InstModes::Absolute => i.to_node(&[read_word_node("", "", TokenAttributes::Std, &[])]),
        InstModes::AbsoluteX => i.to_node(&[read_word_node("", ", x", TokenAttributes::Std, &[])]),
        InstModes::AbsoluteY => i.to_node(&[read_word_node("", ", y", TokenAttributes::Std, &[])]),

        InstModes::IndirectX => {
            i.to_node(&[read_byte_node("(", ", x)", TokenAttributes::Std, &[])])
        }
        InstModes::IndirectY => {
            i.to_node(&[read_byte_node("(", "), y", TokenAttributes::Std, &[])])
        }

        InstModes::Accumulator => i.to_node(&[append_string_node("A")]),
        InstModes::AbsoluteJump => i.to_node(&[read_abs_label_node()]),
        InstModes::IndirectJump => i.to_node(&[
            append_string_node("("),
            read_abs_label_node(),
            append_string_node(")"),
        ]),
        InstModes::ZeroPageIndirect => {
            i.to_node(&[read_byte_node("(", ")", TokenAttributes::Std, &[])])
        }
        InstModes::JumpAbsoluteX => {
            i.to_node(&[read_byte_node("(", ", x)", TokenAttributes::Std, &[])])
        }
        InstModes::AbsoluteLong => i.to_node(&[read_lword_node("", "", TokenAttributes::Std, &[])]),
        InstModes::AbsoluteLongX => {
            i.to_node(&[read_lword_node("", ", x", TokenAttributes::Std, &[])])
        }
        InstModes::DirectPageIndirectLong => {
            i.to_node(&[read_byte_node("[", "]", TokenAttributes::Std, &[])])
        }
        InstModes::DirectPageIndirectLongY => {
            i.to_node(&[read_byte_node("[", "], y", TokenAttributes::Std, &[])])
        }
        InstModes::StackS => i.to_node(&[read_byte_node("", ", s", TokenAttributes::Std, &[])]),
        InstModes::StackSY => {
            i.to_node(&[read_byte_node("(", ", s), y", TokenAttributes::Std, &[])])
        }
        InstModes::JumpAbsoluteLong => i.to_node(&[read_abs_long_label_node()]),
        InstModes::Per => i.to_node(&[read_abs_label_node()]),
        InstModes::BranchLong => i.to_node(&[read_rel_word_label_node()]),
        InstModes::Move => i.to_node(&[
            read_immediate_node(1),
            append_string_node(", "),
            read_immediate_node(1),
        ]),
        InstModes::Jml => i.to_node(&[
            append_string_node("["),
            read_abs_label_node(),
            append_string_node("]"),
        ]),
        InstModes::Jsrx => i.to_node(&[
            append_string_node("("),
            read_abs_label_node(),
            append_string_node(", x)"),
        ]),
    }
}

pub fn make_instructions(ims: &[ImInfo], immediate_size: usize) -> Vec<Node> {
    let mut res = vec![];

    for im in ims {
        res.push(make_instruction(im, immediate_size));
    }

    res
}

pub fn make_arch(nodes: &[Node], default: Option<Node>) -> Arch {
    Arch::new(nodes, build_lookup, default, 1, readnle)
}
