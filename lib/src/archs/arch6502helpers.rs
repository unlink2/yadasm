use crate::{
    always_true, readnle, Context, IWord, Node, Parsed, Symbol, SymbolAttributes, Token,
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
    let text_owned = text.to_owned();
    Node::new(
        0,
        0,
        readnle,
        move |ctx, dat, size| text_converter(&text_owned, ctx, dat, size),
        always_true,
    )
}
