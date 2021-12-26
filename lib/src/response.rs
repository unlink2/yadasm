use crate::Number;

#[derive(Debug, Clone)]
pub enum Response {
    None,
    Static(String),
    Number(Number),
    String(String),
}

impl Default for Response {
    fn default() -> Self {
        Self::None
    }
}
