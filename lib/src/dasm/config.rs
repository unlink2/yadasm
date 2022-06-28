use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::dasm::{AlAs65c816, Context, InsertToken, PadLine};

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    archs: Vec<Archs>,
    ctx: Context,
    input: PathBuf,
    output: Option<PathBuf>,
}
