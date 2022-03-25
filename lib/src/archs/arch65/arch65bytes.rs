use std::iter;

use crate::{Node, TokenAttributes, Word};

use super::{append_string_node, arch65helpers::opcode_node};

pub fn bytes_read_byte_node(attr: TokenAttributes, children: &[Node]) -> Node {
    super::arch65helpers::read_byte_node("!byte ", "", attr, children)
}

pub fn bytes_read_word_node(attr: TokenAttributes, children: &[Node]) -> Node {
    super::arch65helpers::read_word_node("!word ", "", attr, children)
}

pub fn bytes_read_lword_node(attr: TokenAttributes, children: &[Node]) -> Node {
    super::arch65helpers::read_lword_node("!le24 ", "", attr, children)
}

pub fn bytes_read_string_node(len: usize, attr: TokenAttributes) -> Node {
    super::arch65helpers::read_char_node(
        "!text \"",
        "",
        attr,
        &iter::repeat(super::arch65helpers::read_char_node(
            "",
            "",
            TokenAttributes::Std,
            &[],
        ))
        .take(len - 1)
        .chain([append_string_node("\"")])
        .collect::<Vec<Node>>(),
    )
}

pub fn bytes_read_macro_node(name: &str, opcode: Word, children: &[Node]) -> Node {
    opcode_node(&format!("+{}", name), opcode, children)
}

#[cfg(test)]
mod tests {
    use crate::{
        archs::{append_string_node, consume_byte_node, make_arch, read_byte_node},
        parse_to_strings, Context, Error, ErrorKind,
    };

    use super::*;

    fn test_context() -> Context {
        let ctx = Context::new(0x100, 0x110);

        ctx
    }

    fn test_node() -> Node {
        bytes_read_lword_node(
            TokenAttributes::NewLine,
            &[bytes_read_word_node(
                TokenAttributes::NewLine,
                &[bytes_read_byte_node(TokenAttributes::NewLine, &[])],
            )],
        )
    }

    fn test_string_node() -> Node {
        bytes_read_string_node(5, TokenAttributes::Std)
    }

    fn test_macro_node() -> Node {
        bytes_read_macro_node(
            "testMacro",
            0xA9,
            &[
                read_byte_node("", "", TokenAttributes::Std, &[]),
                consume_byte_node(0xA9, &[]),
                append_string_node(", "),
                read_byte_node("", "", TokenAttributes::Std, &[]),
            ],
        )
    }

    #[test]
    fn it_should_read_custom_structure() {
        let mut ctx = test_context();
        let arch = make_arch(&[test_node()], None);

        let parsed = parse_to_strings(
            &mut ctx,
            &[
                0x11, 0x11, 0x11, 0x22, 0x22, 0x33, 0x11, 0x11, 0x11, 0x22, 0x22, 0x33,
            ],
            &[arch],
        );

        assert_eq!(
            Ok(vec![
                "!le24 $111111\n!word $2222\n!byte $33\n".to_owned(),
                "!le24 $111111\n!word $2222\n!byte $33\n".to_owned()
            ]),
            parsed
        );
    }

    #[test]
    fn it_should_fail_if_out_of_data() {
        let mut ctx = test_context();
        let arch = make_arch(&[test_node()], None);

        let parsed = parse_to_strings(
            &mut ctx,
            &[
                0x11, 0x11, 0x11, 0x22, 0x22, 0x33, 0x11, 0x11, 0x11, 0x22, 0x22,
            ],
            &[arch],
        );

        assert_eq!(Err(Error::new(ErrorKind::OutOfData)), parsed);
    }

    #[test]
    fn it_should_read_string() {
        let mut ctx = test_context();
        let arch = make_arch(&[test_string_node()], None);

        let parsed = parse_to_strings(&mut ctx, &[72, 101, 108, 108, 111], &[arch]);

        assert_eq!(Ok(vec!["!text \"Hello\"".to_owned(),]), parsed);
    }

    #[test]
    fn it_should_read_macro() {
        let mut ctx = test_context();
        let arch = make_arch(&[test_macro_node()], None);

        let parsed = parse_to_strings(&mut ctx, &[0xA9, 0x11, 0xA9, 0x22], &[arch]);

        assert_eq!(Ok(vec!["+testMacro $11, $22".to_owned(),]), parsed);
    }

    #[test]
    fn it_should_not_parse_invalid_macros() {
        let mut ctx = test_context();
        let arch = make_arch(&[test_macro_node()], None);

        let parsed = parse_to_strings(&mut ctx, &[0xA9, 0x11, 0xA8, 0x22], &[arch]);

        assert_eq!(Err(Error::new(ErrorKind::NoMatch)), parsed);
    }
}
