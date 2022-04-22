use crate::dasm::{always_true, build_lookup, readnle, Arch, Context, Node, Parsed, Token, Word};

fn raw_converter(_ctx: &mut Context, dat: Word, size: usize) -> Parsed {
    Parsed::new(vec![Token::new(
        &((dat as u8) as char).to_string(),
        size,
        dat,
        crate::dasm::TokenAttributes::Std,
        None,
    )])
}

fn arch_raw_nodes() -> Vec<Node> {
    vec![Node::new(1, 1, readnle, &raw_converter, always_true)]
}

pub fn arch_raw() -> Arch {
    Arch::new(
        &arch_raw_nodes(),
        build_lookup,
        Some(arch_raw_nodes()[0].clone()),
        1,
        readnle,
    )
}

#[cfg(test)]
mod tests {
    use crate::dasm::parse_to_strings;

    use super::*;

    fn test_context() -> Context {
        let ctx = Context::new(0x100, 0x110);

        ctx
    }

    #[test]
    fn it_should_parse_raw() {
        let mut ctx = test_context();

        let result = parse_to_strings(
            &mut ctx,
            &[0xA9, 0x44, 0xAD, 0x00, 0xFF, 0x55, 0x44, 0xA9, 0x45],
            &[&arch_raw()],
        );
        let expected = vec!["©", "D", "\u{ad}", "\u{0}", "ÿ", "U", "D", "©", "E"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        assert_eq!(Ok(expected), result);
    }
}
