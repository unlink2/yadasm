use crate::{Context, Symbol, Word};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node(Vec<Token>);

impl Node {
    pub fn output(&self, ctx: &Context, connector: &str, end: &str) -> String {
        if self.0.is_empty() {
            return "".into();
        } else {
            format!(
                "{}{}",
                ctx.get_symbols(ctx.address)
                    .unwrap_or(&vec![])
                    .iter()
                    .fold(String::new(), |prev, s| {
                        format!("{}{}", prev, s.output("", ""))
                    }),
                self.0.iter().fold(String::new(), |prev, t| {
                    format!("{}{}", prev, t.output(connector, end))
                }),
            )
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenAttributes {
    Std,
    NewLine,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub text: String,
    pub size: usize,
    pub raw: Word,
    pub ptr: Option<Word>,
    pub attr: TokenAttributes,
    pub rep_connector: usize,
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
        rep_connector: usize,
        ptr: Option<Word>,
    ) -> Self {
        Self {
            text: text.into(),
            size,
            raw,
            ptr,
            attr,
            rep_connector,
        }
    }

    pub fn output(&self, connector: &str, end: &str) -> String {
        match self.attr {
            TokenAttributes::NewLine => format!(
                "{}{}{}",
                connector.repeat(self.rep_connector),
                self.to_string(),
                end
            ),
            _ => format!(
                "{}{}",
                connector.repeat(self.rep_connector),
                self.to_string()
            ),
        }
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
    use crate::SymbolAttributes;

    use super::*;
    fn test_context() -> Context {
        let ctx = Context::new(0x100, 0x110);

        ctx
    }

    #[test]
    fn it_should_calculate_total_size_of_tokens() {
        let cws = [
            Token::new("test", 1, 0, TokenAttributes::Std, 1, None),
            Token::new("test", 3, 0, TokenAttributes::Std, 1, None),
        ];

        assert_eq!(4, cws.size());
    }

    #[test]
    fn it_should_output_formatted_result() {
        let mut ctx = test_context();
        ctx.address = 0x101;

        ctx.add_symbol(Symbol::new(0x101, "shadowed", 0, SymbolAttributes::Shadow));
        ctx.add_symbol(Symbol::new(0x101, "correct", 0, SymbolAttributes::Std));
        ctx.add_symbol(Symbol::new(0x100, "wrong", 0, SymbolAttributes::Std));

        let node = Node(vec![
            Token::new("lda", 1, 0, TokenAttributes::Std, 1, None),
            Token::new("#$10", 1, 0, TokenAttributes::Std, 1, None),
        ]);
        assert_eq!("correct:\nlda #$10", node.output(&ctx, " ", "\n"));
    }
}
