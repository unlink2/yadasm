use crate::dasm::{Node, Word};

use super::{
    is_cc11, make_implied, make_instructions65c02, make_opcode, mask_echo, ImInfo, InstModes,
};

pub fn mask(im: InstModes, opcode: Word) -> Word {
    match im {
        InstModes::StackS => make_opcode(0x03, opcode),
        InstModes::DirectPageIndirectLong if is_cc11(opcode) => make_opcode(0x07, opcode),
        InstModes::AbsoluteLong if is_cc11(opcode) => make_opcode(0x0F, opcode),
        InstModes::StackSY if is_cc11(opcode) => make_opcode(0x13, opcode),
        InstModes::DirectPageIndirectLongY if is_cc11(opcode) => make_opcode(0x17, opcode),
        InstModes::AbsoluteLongX if is_cc11(opcode) => make_opcode(0x1F, opcode),
        _ => mask_echo(im, opcode),
    }
}

fn make_im_info(im: InstModes, name: &str, opcode: Word) -> ImInfo {
    ImInfo::new(mask, im, name, opcode)
}

fn make_im_from(ims: &[InstModes], name: &str, opcode: Word) -> Vec<ImInfo> {
    ims.iter().map(|i| make_im_info(*i, name, opcode)).collect()
}

fn make_extended(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(
        &[
            InstModes::AbsoluteLong,
            InstModes::AbsoluteLongX,
            InstModes::DirectPageIndirectLong,
            InstModes::DirectPageIndirectLongY,
            InstModes::StackS,
            InstModes::StackSY,
        ],
        name,
        opcode,
    )
}

fn make_cop(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::ImmediateByte], name, opcode)
}

fn make_jump_long(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::JumpAbsoluteLong], name, opcode)
}

fn make_per(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Per], name, opcode)
}

fn make_brl(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::BranchLong], name, opcode)
}

fn make_move(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Move], name, opcode)
}

fn make_pei(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::ZeroPage], name, opcode)
}

fn make_pea(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Immediate], name, opcode)
}

fn make_jml(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Jml], name, opcode)
}

fn make_jsrx(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Jsrx], name, opcode)
}

pub fn make_instructions65c816(immediate_size: usize) -> Vec<Node> {
    let mut res = make_instructions65c02(immediate_size);

    [
        make_extended("ora", 0x03),
        make_extended("eor", 0x43),
        make_extended("adc", 0x63),
        make_extended("sta", 0x83),
        make_extended("lda", 0xA3),
        make_extended("cmp", 0xC3),
        make_extended("sbc", 0xE3),
        make_implied("xce", 0xFB),
        make_cop("cop", 0x02),
        make_implied("phd", 0x0B),
        make_implied("pld", 0x2B),
        make_implied("rtl", 0x6B),
        make_implied("phb", 0x8B),
        make_implied("plb", 0xAB),
        make_implied("wai", 0xCB),
        make_implied("xba", 0xEB),
        make_implied("tcs", 0x1B),
        make_implied("tsc", 0x3B),
        make_implied("tcd", 0x5B),
        make_implied("tdc", 0x7B),
        make_implied("txy", 0x9B),
        make_implied("tyx", 0xBB),
        make_implied("stp", 0xDB),
        make_implied("wdm", 0x42),
        make_jump_long("jsl", 0x22),
        make_per("per", 0x62),
        make_brl("brl", 0x82),
        make_cop("rep", 0xC2),
        make_cop("sep", 0xE2),
        make_move("mvp", 0x44),
        make_move("mvn", 0x54),
        make_pei("pei", 0xD4),
        make_pea("pea", 0xF4),
        make_jump_long("jmp", 0x5C),
        make_jml("jmp", 0xDC),
        make_jsrx("jsr", 0xFC),
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
    use crate::{
        dasm::archs::{make_arch, IMMEDIATE_SIZE16},
        dasm::{parse_to_strings, Arch, Context, Error},
    };

    fn test_context() -> Context {
        let ctx = Context::new(0x600, 0x700);

        ctx
    }

    fn test_arch() -> Arch {
        make_arch(&make_instructions65c816(IMMEDIATE_SIZE16), None)
    }

    fn parser_helper(data: &[u8], expected: &[&str]) -> (Vec<String>, Result<Vec<String>, Error>) {
        let mut ctx = test_context();
        let arch = test_arch();

        let result = parse_to_strings(&mut ctx, data, &[&arch]);
        let expected = expected.iter().map(|s| s.to_string()).collect();

        (expected, result)
    }

    #[test]
    fn it_should_parse_long_branch_and_immediate() {
        let (expected, result) = parser_helper(
            &[0xA9, 0x12, 0x34, 0x82, 0x6A, 0xFF],
            &["lda #$3412", "brl label_56F"],
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_extended_instructions() {
        let (expected, result) = parser_helper(
            &[
                0x03, 0x11, 0x07, 0x22, 0x0F, 0x33, 0x44, 0x55, 0x13, 0x66, 0x17, 0x77, 0x1F, 0xAA,
                0xBB, 0xCC,
            ],
            &[
                "ora $11, s",
                "ora [$22]",
                "ora $554433",
                "ora ($66, s), y",
                "ora [$77], y",
                "ora $CCBBAA, x",
            ],
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_xce() {
        let (expected, result) = parser_helper(&[0xFB], &["xce"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_cop() {
        let (expected, result) = parser_helper(&[0x02, 0x12], &["cop #$12"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_long_jump() {
        let (expected, result) = parser_helper(
            &[0x22, 0x12, 0x23, 0x45, 0x22, 0x12, 0x23, 0x45],
            &["jsl label_452312", "jsl label_452312"],
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_per() {
        let (expected, result) = parser_helper(&[0x62, 0x12, 0x23], &["per label_2312"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_rep_and_sep() {
        let (expected, result) =
            parser_helper(&[0xC2, 0x12, 0xE2, 0x13], &["rep #$12", "sep #$13"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_move() {
        let (expected, result) = parser_helper(&[0x44, 0x12, 0xE2], &["mvp #$12, #$E2"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_pea() {
        let (expected, result) = parser_helper(&[0xF4, 0x12, 0x11], &["pea #$1112"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_jml() {
        let (expected, result) = parser_helper(&[0xDC, 0x12, 0x11], &["jmp [label_1112]"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_jsrx() {
        let (expected, result) = parser_helper(&[0xFC, 0x12, 0x11], &["jsr (label_1112, x)"]);
        assert_eq!(Ok(expected), result);
    }
}
