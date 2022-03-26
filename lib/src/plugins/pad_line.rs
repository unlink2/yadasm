use crate::{Parser, Token};

pub struct PadLine {
    pub c: char,
    pub padding: isize,
    token_prefix: String,
    token_postfix: String,
    label_prefix: String,
    label_postfix: String,
}

impl PadLine {
    pub fn new(c: char, padding: isize) -> Self {
        Self {
            c,
            padding,
            token_prefix: "".into(),
            token_postfix: "".into(),
            label_prefix: "".into(),
            label_postfix: ":".into(),
        }
    }
}

impl Parser for PadLine {
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
                let padding = self.padding.abs()
                    - (parsed
                        .output(
                            ctx,
                            &self.token_prefix,
                            &self.token_postfix,
                            &self.label_prefix,
                            &self.label_postfix,
                        )
                        .len() as isize);
                let token = Token::new(
                    &std::iter::repeat(self.c)
                        .take(0.max(padding.abs() as usize))
                        .collect::<String>(),
                    0,
                    0,
                    crate::TokenAttributes::Std,
                    None,
                );
                if self.padding > 0 {
                    parsed.push(token);
                } else if self.padding < 0 {
                    parsed.tokens.insert(0, token);
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
        archs::{make_arch, make_instructions6502, IMMEDIATE_SIZE16},
        parse_to_strings, Arch, Context, Error,
    };

    fn test_context() -> Context {
        let ctx = Context::new(0x600, 0x700);

        ctx
    }

    fn test_arch() -> Arch {
        make_arch(&make_instructions6502(IMMEDIATE_SIZE16), None)
    }

    fn parser_helper(
        data: &[u8],
        expected: &[&str],
        padding: isize,
    ) -> (Vec<String>, Result<Vec<String>, Error>) {
        let mut ctx = test_context();
        let arch = test_arch();
        let pad = PadLine::new('#', padding);

        let result = parse_to_strings(&mut ctx, data, &[&pad, &arch]);
        let expected = expected.iter().map(|s| s.to_string()).collect();

        (expected, result)
    }

    #[test]
    fn it_should_pad_line() {
        let (expected, result) = parser_helper(
            &[0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA],
            &[
                "nop#########",
                "nop#########",
                "lda #$EAEA##",
                "lda #$EAEA##",
                "nop#########",
            ],
            12,
        );

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_pad_line_exactly() {
        let (expected, result) = parser_helper(
            &[0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA],
            &[
                "nop#######",
                "nop#######",
                "lda #$EAEA",
                "lda #$EAEA",
                "nop#######",
            ],
            10,
        );

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_not_pad_line() {
        let (expected, result) = parser_helper(
            &[0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA],
            &["nop", "nop", "lda #$EAEA", "lda #$EAEA", "nop"],
            0,
        );

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_pad_line_negative() {
        let (expected, result) = parser_helper(
            &[0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA],
            &[
                "#########nop",
                "#########nop",
                "##lda #$EAEA",
                "##lda #$EAEA",
                "#########nop",
            ],
            -12,
        );

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn it_should_pad_line_negative_exact() {
        let (expected, result) = parser_helper(
            &[0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA],
            &[
                "#######nop",
                "#######nop",
                "lda #$EAEA",
                "lda #$EAEA",
                "#######nop",
            ],
            -10,
        );

        assert_eq!(Ok(expected), result);
    }
}
