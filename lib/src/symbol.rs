use crate::Word;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolAttributes {
    Std,
    NewLine,
    Shadow,
}

impl ToString for SymbolAttributes {
    fn to_string(&self) -> String {
        match self {
            Self::NewLine => "\n".into(),
            _ => "".into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub attr: SymbolAttributes,
    pub address: Word,
    pub name: String,
    pub order: u16,
    pub prefix_rep: usize,
    pub postfix_rep: usize,
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        match self.attr {
            SymbolAttributes::Shadow => "".into(),
            _ => self.name.clone(),
        }
    }
}

impl Symbol {
    pub fn new(name: &str, address: Word, order: u16, attr: SymbolAttributes) -> Self {
        Self {
            address,
            name: name.into(),
            order,
            attr,
            prefix_rep: 1,
            postfix_rep: 1,
        }
    }

    pub fn output(&self, prefix: &str, postfix: &str) -> String {
        match self.attr {
            SymbolAttributes::Shadow => "".into(),
            _ => format!(
                "{}{}{}{}",
                prefix.repeat(self.prefix_rep),
                self.to_string(),
                postfix.repeat(self.postfix_rep),
                self.attr.to_string()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_use_prefix_dup() {
        let mut t = Symbol::new("test", 0, 0, SymbolAttributes::Std);
        t.prefix_rep = 2;
        t.postfix_rep = 1;
        assert_eq!(">>test:", t.output(">", ":"));
    }
}
