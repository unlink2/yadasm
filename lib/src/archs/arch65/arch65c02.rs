use crate::{Node, Word};

use super::{
    is_cc00, is_cc10, make_branch, make_implied, make_instructions6502, make_opcode, mask_echo,
    ImInfo, InstModes,
};

pub fn mask(im: InstModes, opcode: Word) -> Word {
    match im {
        InstModes::ZeroPageIndirect if is_cc10(opcode) => make_opcode(0x12, opcode),
        InstModes::Immediate => make_opcode(0x89, opcode), // bit
        InstModes::ZeroPageX if is_cc00(opcode) => make_opcode(0x04, opcode),
        InstModes::Absolute if is_cc00(opcode) => make_opcode(0x1C, opcode),
        InstModes::JumpAbsoluteX if is_cc00(opcode) => make_opcode(0x7C, opcode),
        InstModes::AbsoluteX if is_cc00(opcode) => make_opcode(0x9E, opcode),
        InstModes::ZeroPage if is_cc00(opcode) => make_opcode(0x14, opcode),
        _ => mask_echo(im, opcode),
    }
}

fn make_im_info(im: InstModes, name: &str, opcode: Word) -> ImInfo {
    ImInfo::new(mask, im, name, opcode)
}

fn make_im_info_echo(im: InstModes, name: &str, opcode: Word) -> ImInfo {
    ImInfo::new(mask_echo, im, name, opcode)
}

fn make_im_from(ims: &[InstModes], name: &str, opcode: Word) -> Vec<ImInfo> {
    ims.iter().map(|i| make_im_info(*i, name, opcode)).collect()
}

fn make_zero_page_indirect(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::ZeroPageIndirect], name, opcode)
}

fn make_jump_indirect(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::JumpAbsoluteX], name, opcode)
}

fn make_bit_ext(name: &str, _opcode: Word) -> Vec<ImInfo> {
    vec![
        make_im_info_echo(InstModes::ZeroPageX, name, 0x34),
        make_im_info_echo(InstModes::AbsoluteX, name, 0x3C),
        make_im_info_echo(InstModes::Immediate, name, 0x89),
    ]
}

fn make_trb(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::ZeroPage, InstModes::Absolute], name, opcode)
}

fn make_tsb(name: &str, _opcode: Word) -> Vec<ImInfo> {
    vec![
        make_im_info_echo(InstModes::ZeroPage, name, 0x04),
        make_im_info_echo(InstModes::Absolute, name, 0x0C),
    ]
}

fn make_stz(name: &str, _opcode: Word) -> Vec<ImInfo> {
    vec![
        make_im_info_echo(InstModes::ZeroPageX, name, 0x74),
        make_im_info_echo(InstModes::AbsoluteX, name, 0x9E),
        make_im_info_echo(InstModes::ZeroPage, name, 0x64),
        make_im_info_echo(InstModes::Absolute, name, 0x9C),
    ]
}

fn make_acc(name: &str, opcode: Word) -> Vec<ImInfo> {
    make_im_from(&[InstModes::Accumulator], name, opcode)
}

pub fn make_instructions65c02(immediate_size: usize) -> Vec<Node> {
    let mut res = make_instructions6502(immediate_size);

    [
        make_zero_page_indirect("ora", 0x12),
        make_zero_page_indirect("and", 0x32),
        make_zero_page_indirect("eor", 0x52),
        make_zero_page_indirect("adc", 0x72),
        make_zero_page_indirect("sta", 0x92),
        make_zero_page_indirect("lda", 0xB2),
        make_zero_page_indirect("cmp", 0xD2),
        make_zero_page_indirect("sbc", 0xF2),
        make_jump_indirect("jmp", 0x7C),
        make_bit_ext("bit", 0x00),
        make_tsb("tsb", 0x00),
        make_trb("trb", 0x1C),
        make_stz("stz", 0x74),
        make_branch("bra", 0x80),
        make_acc("inc", 0x1A),
        make_acc("dec", 0x3A),
        make_implied("phy", 0x5A),
        make_implied("ply", 0x7A),
        make_implied("phx", 0xDA),
        make_implied("plx", 0xFA),
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
        archs::{bytes_read_byte_node, make_arch},
        parse_to_strings, Arch, Context, Error, TokenAttributes,
    };

    fn test_context() -> Context {
        let ctx = Context::new(0x600, 0x700);

        ctx
    }

    fn test_arch() -> Arch {
        make_arch(&make_instructions65c02(1), None)
    }

    fn parser_helper(data: &[u8], expected: &[&str]) -> (Vec<String>, Result<Vec<String>, Error>) {
        let mut ctx = test_context();
        let arch = test_arch();

        let result = parse_to_strings(&mut ctx, data, &[arch]);
        let expected = expected.iter().map(|s| s.to_string()).collect();

        (expected, result)
    }

    #[test]
    fn it_should_parse_zero_page_indirect() {
        let (expected, result) = parser_helper(&[0x12, 0xAA], &["ora ($AA)"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_jump_abs_x() {
        let (expected, result) = parser_helper(&[0x7C, 0xAA, 0xBB], &["jmp (label_BBAA, x)"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_bit_ext() {
        let (expected, result) = parser_helper(
            &[0x89, 0xAA, 0x34, 0xBB, 0x3C, 0xDD, 0xCC],
            &["bit #$AA", "bit $BB, x", "bit $CCDD, x"],
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_tsb() {
        let (expected, result) =
            parser_helper(&[0x04, 0xAA, 0x0C, 0xAA, 0xBB], &["tsb $AA", "tsb $BBAA"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_trb() {
        let (expected, result) =
            parser_helper(&[0x14, 0xAA, 0x1C, 0xAA, 0xBB], &["trb $AA", "trb $BBAA"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_stz() {
        let (expected, result) = parser_helper(
            &[0x74, 0xAA, 0x9E, 0xAA, 0xBB, 0x64, 0x11, 0x9C, 0x11, 0x22],
            &["stz $AA, x", "stz $BBAA, x", "stz $11", "stz $2211"],
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_bra() {
        let (expected, result) = parser_helper(&[0x80, 0xAA], &["bra label_5AC"]);
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_single_byte_instructions() {
        let (expected, result) = parser_helper(
            &[0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA],
            &["inc A", "dec A", "phy", "ply", "phx", "plx"],
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_parse_default() {
        let mut ctx = test_context();
        let mut arch = test_arch();
        arch.default = Some(bytes_read_byte_node(TokenAttributes::NewLine, &[]));

        let data: [u8; 2] = [0xEA, 0xFF];
        let expected = ["nop", "!byte $FF\n"];

        let result = parse_to_strings(&mut ctx, &data, &[arch]);
        let expected = expected.iter().map(|s| s.to_string()).collect();
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_fail_without_default() {
        let mut ctx = test_context();
        let arch = test_arch();

        let data: [u8; 2] = [0xEA, 0xFF];

        let result = parse_to_strings(&mut ctx, &data, &[arch]);
        assert_eq!(
            Err(Error::new(crate::ErrorKind::ParserFailed).set_address(0x601)),
            result
        );
    }
}
