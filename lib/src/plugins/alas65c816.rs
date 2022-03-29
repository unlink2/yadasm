use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::{
    archs::{
        bytes_read_byte_node, make_arch, make_instructions65c816, IMMEDIATE_SIZE16, IMMEDIATE_SIZE8,
    },
    Arch, Context, Parsed, Parser, Token, TokenAttributes, Word,
};

pub fn default_aas() -> Arch {
    make_arch(
        &make_instructions65c816(IMMEDIATE_SIZE8),
        Some(bytes_read_byte_node(crate::TokenAttributes::NewLine, &[])),
    )
}

pub fn default_aal() -> Arch {
    make_arch(
        &make_instructions65c816(IMMEDIATE_SIZE16),
        Some(bytes_read_byte_node(crate::TokenAttributes::NewLine, &[])),
    )
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct AlAs65c816 {
    at: HashSet<Word>,

    #[serde(skip, default = "default_aal")]
    aal: Arch,
    #[serde(skip, default = "default_aas")]
    aas: Arch,
}

impl Default for AlAs65c816 {
    fn default() -> Self {
        Self {
            aal: default_aal(),
            aas: default_aas(),
            at: HashSet::default(),
        }
    }
}

impl AlAs65c816 {
    pub fn new(at: HashSet<Word>) -> Self {
        Self {
            at,
            ..Default::default()
        }
    }

    fn is_as(ctx: &crate::Context) -> bool {
        ctx.get_flag("as").is_some()
    }

    fn force_as(&self, ctx: &Context) -> bool {
        !Self::is_as(ctx) && self.at.contains(&ctx.address)
    }

    fn force_al(&self, ctx: &Context) -> bool {
        Self::is_as(ctx) && self.at.contains(&ctx.address)
    }

    fn is_al_tokens(parsed: &Parsed) -> bool {
        if parsed.tokens.len() < 2 {
            false
        } else {
            parsed.tokens[0].raw == 0xC2 && parsed.tokens[1].raw == 0x20
        }
    }

    fn is_as_tokens(parsed: &Parsed) -> bool {
        if parsed.tokens.len() < 2 {
            false
        } else {
            parsed.tokens[0].raw == 0xE2 && parsed.tokens[1].raw == 0x20
        }
    }

    fn modify(&self, ctx: &mut crate::Context, parsed: &mut Parsed) {
        if self.force_al(ctx) || Self::is_al_tokens(parsed) {
            ctx.unset_flag("as");
            parsed.push(Token::new("\n!al", 0, 0, TokenAttributes::Std, None))
        } else if self.force_as(ctx) || Self::is_as_tokens(parsed) {
            ctx.set_flag("as", "");
            parsed.push(Token::new("\n!as", 0, 0, TokenAttributes::Std, None))
        }
    }
}

impl Parser for AlAs65c816 {
    fn parse(
        &self,
        ctx: &mut crate::Context,
        bin: &[u8],
        next: &[&dyn Parser],
    ) -> Result<crate::Parsed, crate::Error> {
        let mut parsed = if Self::is_as(ctx) {
            self.aas.parse(ctx, bin, self.tail(next))
        } else {
            self.aal.parse(ctx, bin, self.tail(next))
        };

        if let Ok(parsed) = &mut parsed {
            self.modify(ctx, parsed);
        }

        parsed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse_to_strings, Context, Error};

    fn test_context() -> Context {
        let ctx = Context::new(0x600, 0x700);

        ctx
    }

    fn parser_helper(
        data: &[u8],
        expected: &[&str],
        at: HashSet<Word>,
    ) -> (Vec<String>, Result<Vec<String>, Error>) {
        let mut ctx = test_context();
        let alas = AlAs65c816::new(at);

        let result = parse_to_strings(&mut ctx, data, &[&alas]);
        let expected = expected.iter().map(|s| s.to_string()).collect();

        (expected, result)
    }

    #[test]
    fn it_should_use_al_as() {
        let (expected, result) = parser_helper(
            &[
                0xC2, 0x20, 0xA9, 0xEA, 0xEA, 0xE2, 0x20, 0xA9, 0xEA, 0xC2, 0x20, 0xA9, 0xEA, 0xEA,
                0xE2, 0x20, 0xA9, 0xEA, 0xC2, 0x20,
            ],
            &[
                "rep #$20\n!al",
                "lda #$EAEA",
                "sep #$20\n!as",
                "lda #$EA",
                "rep #$20\n!al",
                "lda #$EAEA",
                "sep #$20\n!as",
                "lda #$EA",
                "rep #$20\n!al",
            ],
            HashSet::default(),
        );
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_force_al_as() {
        let (expected, result) = parser_helper(
            &[
                0xC2, 0x21, 0xA9, 0xEA, 0xEA, 0xE2, 0x21, 0xA9, 0xEA, 0xC2, 0x21, 0xA9, 0xEA, 0xEA,
                0xE2, 0x20, 0xA9, 0xEA, 0xC2, 0x21,
            ],
            &[
                "rep #$21\n!as",
                "lda #$EA",
                "nop",
                "sep #$21",
                "lda #$EA",
                "rep #$21\n!al",
                "lda #$EAEA",
                "sep #$20\n!as",
                "lda #$EA",
                "rep #$21\n!al",
            ],
            HashSet::from([0x600, 0x609, 0x612]),
        );
        assert_eq!(Ok(expected), result);
    }
}
