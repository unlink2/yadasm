use core::fmt::Write;

use crate::error::Error;

use super::env::Env;

pub type Data = [u8];
pub type Parser = fn(env: &mut Env, data: &Data, write: &dyn Write) -> Result<(), Error>;
