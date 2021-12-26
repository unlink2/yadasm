use crate::Number;

pub trait Response: Default {}

#[derive(Debug, Clone)]
pub enum BasicResponse {
    None,
    Static(String),
    Number(Number),
    String(String),
}

impl Default for BasicResponse {
    fn default() -> Self {
        Self::None
    }
}

impl Response for BasicResponse {}
