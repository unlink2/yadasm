use crate::{
    always_false, always_true, readnle, Context, Node, Parsed, Symbol, SymbolAttributes, Token,
    TokenAttributes, Word,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InstructionModes {
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
    ZeropageY,
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
    pub im: InstructionModes,
    pub opcode: Word,
}

impl ImInfo {
    pub fn new(
        mask: fn(InstructionModes, Word) -> Word,
        im: InstructionModes,
        name: &str,
        opcode: Word,
    ) -> Self {
        Self {
            name: name.into(),
            im,
            opcode: (mask)(im, opcode),
        }
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

pub fn opcode_node(name: &str, opcode: Word, children: Vec<Node>) -> Node {
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
pub fn consume_byte_node(opcode: Word, children: Vec<Node>) -> Node {
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
    children: Vec<Node>,
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
    children: Vec<Node>,
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
        1 => read_byte_node("#", "", TokenAttributes::Std, vec![]),
        _ => read_word_node("#", "", TokenAttributes::Std, vec![]),
    }
}

pub fn read_word_node(
    prefix: &str,
    postfix: &str,
    attr: TokenAttributes,
    children: Vec<Node>,
) -> Node {
    let prefix = prefix.to_owned();
    let postfix = postfix.to_owned();
    Node::with_children(
        2,
        2,
        readnle,
        move |ctx, dat, size| {
            number_converter(
                |dat| format!("{:04X}", dat as u8),
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
    children: Vec<Node>,
) -> Node {
    let prefix = prefix.to_owned();
    let postfix = postfix.to_owned();
    Node::with_children(
        3,
        3,
        readnle,
        move |ctx, dat, size| {
            number_converter(
                |dat| format!("{:06X}", dat as u8),
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
pub fn mask_echo(_im: InstructionModes, opcode: Word) -> Word {
    opcode
}

pub fn make_instruction(immediate_size: usize, iminfo: ImInfo) -> Node {
    match iminfo.im {
        _ => Node::new(0, 0, readnle, no_converter, always_false),
    }
}
