use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::dasm::{AlAs65c816, Context, InsertToken, PadLine};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone)]
pub enum Archs {
    A6502,
    A65C02,
    A65C816,
    A6502Bytes,
    ARaw,
    InsertToken(Box<InsertToken>),
    PadLine(Box<PadLine>),
    AlAs65C816(Box<AlAs65c816>),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct Config {
    archs: Vec<Archs>,
    ctx: Context,
    input: PathBuf,
    output: Option<PathBuf>,
}
