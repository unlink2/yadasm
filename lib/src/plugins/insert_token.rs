use std::collections::HashMap;

use crate::{Parser, Token, Word};

pub struct InsertToken {
    pub before: HashMap<Word, Token>,
    pub after: HashMap<Word, Token>,
    pub default: bool,
    pub default_prefix: String,
}

impl InsertToken {
    pub fn new(default: bool, before: HashMap<Word, Token>, after: HashMap<Word, Token>) -> Self {
        Self {
            default,
            before,
            after,
            default_prefix: " ; ".into(),
        }
    }
}

impl Parser for InsertToken {
    fn parse(
        &self,
        ctx: &mut crate::Context,
        bin: &[u8],
        next: &[&dyn Parser],
    ) -> Result<crate::Parsed, crate::Error> {
        // call the next parser and modify the result as needed
        let result = self.default_parse(ctx, bin, self.tail(next));

        match result {
            Ok(mut parsed) => {
                if let Some(before) = self.before.get(&ctx.address) {
                    parsed.tokens.insert(0, before.clone());
                }
                if let Some(after) = self.after.get(&ctx.address) {
                    parsed.push(after.clone());
                } else if self.default {
                    parsed.push(Token::new(
                        &format!("{}{:X}", self.default_prefix, ctx.address),
                        0,
                        0,
                        crate::TokenAttributes::Std,
                        None,
                    ))
                }
                Ok(parsed)
            }
            Err(err) => Err(err),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        archs::{make_arch, make_instructions65c816, IMMEDIATE_SIZE16},
        parse_to_strings, Arch, Context, TokenAttributes,
    };

    fn test_context() -> Context {
        let ctx = Context::new(0x600, 0x700);

        ctx
    }

    fn test_arch() -> Arch {
        make_arch(&make_instructions65c816(IMMEDIATE_SIZE16), None)
    }

    #[test]
    fn it_should_use_insert_token_plugin() {
        let mut ctx = test_context();
        let arch = test_arch();
        let insert_line = InsertToken::new(
            false,
            HashMap::from([(
                0x602,
                Token::new("before ", 0, 0, TokenAttributes::Std, None),
            )]),
            HashMap::from([(
                0x602,
                Token::new(" ; insert after", 0, 0, TokenAttributes::Std, None),
            )]),
        );

        let expected = [
            "nop",
            "nop",
            "before lda #$EAEA ; insert after",
            "lda #$EAEA",
        ];
        let data: [u8; 8] = [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA];

        let result = parse_to_strings(&mut ctx, &data, &[&insert_line, &arch]);
        let expected = expected.iter().map(|s| s.to_string()).collect();

        assert_eq!(Ok(expected), result)
    }

    #[test]
    fn it_should_use_insert_default_token_plugin() {
        let mut ctx = test_context();
        let arch = test_arch();
        let insert_line = InsertToken::new(
            true,
            HashMap::from([(
                0x602,
                Token::new("before ", 0, 0, TokenAttributes::Std, None),
            )]),
            HashMap::from([(
                0x602,
                Token::new(" ; insert after", 0, 0, TokenAttributes::Std, None),
            )]),
        );

        let expected = [
            "nop ; 600",
            "nop ; 601",
            "before lda #$EAEA ; insert after",
            "lda #$EAEA ; 605",
            "nop ; 608",
        ];
        let data: [u8; 9] = [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA];

        let result = parse_to_strings(&mut ctx, &data, &[&insert_line, &arch]);
        let expected = expected.iter().map(|s| s.to_string()).collect();

        assert_eq!(Ok(expected), result)
    }
}
