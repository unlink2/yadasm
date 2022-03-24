use crate::{Context, Word};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Parsed {
    pub tokens: Vec<Token>,
}

impl Parsed {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn output(
        &self,
        ctx: &Context,
        token_prefix: &str,
        token_postfix: &str,
        lable_prefix: &str,
        label_postfix: &str,
    ) -> String {
        if self.tokens.is_empty() {
            "".into()
        } else {
            format!(
                "{}{}",
                ctx.get_symbols(ctx.address)
                    .unwrap_or(&vec![])
                    .iter()
                    .fold(String::new(), |prev, s| {
                        format!("{}{}", prev, s.output(lable_prefix, label_postfix))
                    }),
                self.tokens.iter().fold(String::new(), |prev, t| {
                    format!("{}{}", prev, t.output(token_prefix, token_postfix))
                }),
            )
        }
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn append(&mut self, mut other: Self) {
        self.tokens.append(&mut other.tokens);
    }

    pub fn size(&self) -> usize {
        self.tokens.size()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenAttributes {
    Std,
    NewLine,
}

impl ToString for TokenAttributes {
    fn to_string(&self) -> String {
        match self {
            Self::NewLine => "\n".into(),
            _ => "".into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub text: String,
    pub size: usize,
    pub raw: Word,
    pub address: Option<Word>,
    pub ptr: Option<Word>,
    pub attr: TokenAttributes,
    pub prefix_rep: usize,
    pub postfix_rep: usize,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        self.text.to_owned()
    }
}

impl Token {
    pub fn new(
        text: &str,
        size: usize,
        raw: Word,
        attr: TokenAttributes,
        address: Option<Word>,
    ) -> Self {
        Self {
            text: text.into(),
            size,
            raw,
            address,
            attr,
            ptr: None,
            prefix_rep: 1,
            postfix_rep: 1,
        }
    }

    pub fn output(&self, prefix: &str, postfix: &str) -> String {
        format!(
            "{}{}{}{}",
            prefix.repeat(self.prefix_rep),
            self.to_string(),
            postfix.repeat(self.postfix_rep),
            self.attr.to_string()
        )
    }
}

pub trait Line {
    fn size(&self) -> usize;
}

impl Line for [Token] {
    /// Calculates the total length of a list of tokens
    fn size(&self) -> usize {
        self.iter().fold(0, |prev, x| prev + x.size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Symbol;
    use crate::SymbolAttributes;

    fn test_context() -> Context {
        let ctx = Context::new(0x100, 0x110);

        ctx
    }

    #[test]
    fn it_should_calculate_total_size_of_tokens() {
        let cws = [
            Token::new("test", 1, 0, TokenAttributes::Std, None),
            Token::new("test", 3, 0, TokenAttributes::Std, None),
        ];

        assert_eq!(4, cws.size());
    }

    #[test]
    fn it_should_output_formatted_result() {
        let mut ctx = test_context();
        ctx.address = 0x101;

        ctx.add_symbol(Symbol::new( "shadowed",0x101, 0, SymbolAttributes::Shadow));
        ctx.add_symbol(Symbol::new( "correct", 0x101, 0, SymbolAttributes::NewLine));
        ctx.add_symbol(Symbol::new( "wrong", 0x100, 0, SymbolAttributes::Std));

        let node = Parsed::new(vec![
            Token {
                prefix_rep: 0,
                ..Token::new("lda", 1, 0, TokenAttributes::Std, None)
            },
            Token::new("#$10", 1, 0, TokenAttributes::NewLine, None),
        ]);
        assert_eq!(
            ">>correct:\nlda #$10\n",
            node.output(&ctx, " ", "", ">>", ":")
        );
    }

    #[test]
    fn it_should_return_nothing_with_empty_input() {
        let ctx = test_context();

        let node = Parsed::new(vec![]);
        assert_eq!("", node.output(&ctx, "", "", "", ""));
    }

    #[test]
    fn it_should_use_prefix_dup() {
        let mut t = Token::new("test", 1, 0, TokenAttributes::Std, None);
        t.prefix_rep = 2;
        t.postfix_rep = 3;
        assert_eq!(">>test<<<", t.output(">", "<"));
    }
}
