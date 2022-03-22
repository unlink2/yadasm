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
    pub fn new(address: Word, name: &str, order: u16, attr: SymbolAttributes) -> Self {
        Self {
            address,
            name: name.into(),
            order,
            attr,
        }
    }

    pub fn output(&self, prefix: &str, postfix: &str) -> String {
        match self.attr {
            SymbolAttributes::Shadow => "".into(),
            _ => format!(
                "{}{}{}{}",
                prefix,
                self.to_string(),
                postfix,
                self.attr.to_string()
            ),
        }
    }
}
