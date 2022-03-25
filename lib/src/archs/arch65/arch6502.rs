use crate::{Node, Word};

use super::{is_cc00, is_cc01, is_cc10, make_opcode, mask_echo, ImInfo, InstModes};

fn mask(im: InstModes, opcode: Word) -> Word {
    match im {
        InstModes::Immediate if is_cc00(opcode) => make_opcode(0xE0, opcode),
        InstModes::Immediate if is_cc01(opcode) => make_opcode(0x69, opcode),
        InstModes::Immediate if is_cc10(opcode) => make_opcode(0xA2, opcode),

        InstModes::ZeroPage if is_cc00(opcode) => make_opcode(0xE4, opcode),
        InstModes::ZeroPage if is_cc01(opcode) => make_opcode(0x65, opcode),
        InstModes::ZeroPage if is_cc10(opcode) => make_opcode(0xA6, opcode),

        InstModes::ZeroPageX if is_cc00(opcode) => make_opcode(0xB4, opcode),
        InstModes::ZeroPageX if is_cc01(opcode) => make_opcode(0x75, opcode),
        InstModes::ZeroPageX if is_cc10(opcode) => make_opcode(0x75, opcode),

        InstModes::ZeroPageY if is_cc10(opcode) => make_opcode(0xEC, opcode),

        InstModes::Absolute if is_cc00(opcode) => make_opcode(0xBC, opcode),
        InstModes::Absolute if is_cc01(opcode) => make_opcode(0x7D, opcode),
        InstModes::Absolute if is_cc10(opcode) => make_opcode(0x7D, opcode),

        InstModes::AbsoluteJump if is_cc00(opcode) => make_opcode(0xEC, opcode),

        InstModes::AbsoluteX if is_cc00(opcode) => make_opcode(0xBC, opcode),
        InstModes::AbsoluteX if is_cc01(opcode) => make_opcode(0x7D, opcode),
        InstModes::AbsoluteX if is_cc10(opcode) => make_opcode(0x7D, opcode),

        InstModes::AbsoluteY if is_cc01(opcode) => make_opcode(0x79, opcode),
        InstModes::AbsoluteY if is_cc10(opcode) => make_opcode(0xBE, opcode),

        InstModes::IndirectX if is_cc01(opcode) => make_opcode(0x61, opcode),
        InstModes::IndirectY if is_cc01(opcode) => make_opcode(0x71, opcode),

        InstModes::Implied if is_cc01(opcode) => make_opcode(0x00, opcode),

        InstModes::Relative if is_cc00(opcode) => mask_echo(im, opcode),
        InstModes::Relative if is_cc01(opcode) => mask_echo(im, opcode),

        InstModes::Accumulator if is_cc10(opcode) => make_opcode(0x0A, opcode),
        // indirect jmp breaks rules
        InstModes::IndirectJump if is_cc00(opcode) => mask_echo(im, 0x6C),

        InstModes::Implied => mask_echo(im, opcode),
        _ => mask_echo(im, opcode),
    }
}

fn make_im_info(im: InstModes, name: &str, opcode: Word) -> ImInfo {
    ImInfo::new(mask, im, name, opcode)
}

fn make_im_from(ims: &[InstModes], name: &str, opcode: Word) -> Vec<ImInfo> {
    ims.iter().map(|i| make_im_info(*i, name, opcode)).collect()
}

fn make_store(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::ZeroPage,
            InstModes::ZeroPageX,
            InstModes::Absolute,
            InstModes::AbsoluteX,
            InstModes::AbsoluteY,
            InstModes::IndirectX,
            InstModes::IndirectY,
        ],
        name,
        opcode,
    )
}

fn make_load(name: &str, opcode: Word) -> Vec<ImInfo> {
    let mut res = make_store(name, opcode);
    res.push(make_im_info(InstModes::Immediate, name, opcode));
    res
}

fn make_acc(name: &str, opcode: Word) -> Vec<ImInfo> {
    vec![make_im_info(InstModes::Accumulator, name, opcode)]
}

fn make_logic(name: &str, opcode: Word) -> Vec<ImInfo> {
    let mut res = make_acc(name, opcode);
    res.append(&mut make_im_from(
        &[
            InstModes::ZeroPage,
            InstModes::ZeroPageX,
            InstModes::Absolute,
            InstModes::AbsoluteX,
        ],
        name,
        opcode,
    ));
    res
}

fn make_bit(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::ZeroPage, InstModes::Absolute], name, opcode)
}

fn make_branch(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Relative], name, opcode)
}

fn make_implied(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Implied], name, opcode)
}

fn make_compare_index(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::ZeroPage,
            InstModes::ZeroPageX,
            InstModes::Absolute,
            InstModes::AbsoluteX,
        ],
        name,
        opcode,
    )
}

fn make_dec(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::ZeroPage,
            InstModes::ZeroPageX,
            InstModes::Absolute,
            InstModes::AbsoluteX,
        ],
        name,
        opcode,
    )
}

fn make_jump(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[InstModes::AbsoluteJump, InstModes::IndirectJump],
        name,
        opcode,
    )
}

fn make_jsr(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::AbsoluteJump], name, opcode)
}

fn make_loadx(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::Immediate,
            InstModes::ZeroPage,
            InstModes::ZeroPageY,
            InstModes::Absolute,
            InstModes::AbsoluteY,
        ],
        name,
        opcode,
    )
}

fn make_loady(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::Immediate,
            InstModes::ZeroPage,
            InstModes::ZeroPageX,
            InstModes::Absolute,
            InstModes::AbsoluteX,
        ],
        name,
        opcode,
    )
}

fn make_storex(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::ZeroPage,
            InstModes::ZeroPageY,
            InstModes::Absolute,
        ],
        name,
        opcode,
    )
}

fn make_storey(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::ZeroPage,
            InstModes::ZeroPageX,
            InstModes::Absolute,
        ],
        name,
        opcode,
    )
}

pub fn make_instructions6502(immediate_size: usize) -> Vec<Node> {
    let mut res = vec![];

    [
        make_load("adc", 0x69),
        make_logic("asl", 0x1E),
        make_load("and", 0x21),
        make_bit("bit", 0x24),
        make_branch("bpl", 0x10),
        make_branch("bmi", 0x30),
        make_branch("bvc", 0x50),
        make_branch("bvs", 0x70),
        make_branch("bne", 0xD0),
        make_branch("beq", 0xF0),
        make_implied("brk", 0x00),
        make_load("cmp", 0xC0),
        make_compare_index("cpx", 0xE0),
        make_compare_index("cpy", 0xC0),
        make_dec("dec", 0xC6),
        make_load("eor", 0x49),
        make_implied("clc", 0x18),
        make_implied("sec", 0x38),
        make_implied("cli", 0x58),
        make_implied("sei", 0x78),
        make_implied("clv", 0xB8),
        make_implied("cld", 0xD8),
        make_implied("sed", 0xF8),
        make_dec("inc", 0xE6),
        make_jump("jmp", 0x4C),
        make_jsr("jsr", 0x20),
        make_load("lda", 0xA9),
        make_loadx("ldx", 0xA2),
        make_loady("ldy", 0xA0),
        make_logic("lsr", 0x4A),
        make_implied("tax", 0xAA),
        make_implied("txa", 0x8A),
        make_implied("dex", 0xCA),
        make_implied("inx", 0xE8),
        make_implied("tay", 0xA8),
        make_implied("dey", 0x88),
        make_implied("iny", 0xC8),
        make_logic("rol", 0x2A),
        make_logic("ror", 0x6A),
        make_implied("rti", 0x40),
        make_implied("rts", 0x60),
        make_load("sbc", 0xE9),
        make_store("sta", 0x85),
        make_implied("txs", 0x9A),
        make_implied("tsx", 0xBA),
        make_implied("pha", 0x48),
        make_implied("pla", 0x68),
        make_implied("php", 0x08),
        make_implied("plp", 0x28),
        make_storex("stx", 0x86),
        make_storey("sty", 0x84),
    ]
    .iter()
    .for_each(|i| {
        res.append(&mut super::arch65helpers::make_instructions(
            i,
            immediate_size,
        ))
    });

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{archs::make_arch, parse_to_strings, Arch, Context};

    fn test_context() -> Context {
        let ctx = Context::new(0x600, 0x700);

        ctx
    }

    fn test_arch() -> Arch {
        make_arch(&make_instructions6502(1), None)
    }

    #[test]
    fn it_should_parse_immediate() {
        let mut ctx = test_context();
        let arch = test_arch();

        let result = parse_to_strings(
            &mut ctx,
            &[
                0xA9, 0xAB, 0xA5, 0xAB, 0xB5, 0xAB, 0xAD, 0x00, 0x02, 0xBD, 0x00, 0x02, 0xB9, 0x00,
                0x02, 0xA1, 0xAb, 0xB1, 0xAB,
            ],
            &[arch],
        );
        let expected = vec![
            "lda #$AB",
            "lda $AB",
            "lda $AB, x",
            "lda $0200",
            "lda $0200, x",
            "lda $0200, y",
            "lda ($AB, x)",
            "lda ($AB), y",
        ]
        .iter()
        .map(|s| s.to_string())
        .collect();

        assert_eq!(Ok(expected), result);
    }
}
