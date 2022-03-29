use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::{AlAs65c816, Context, InsertToken, PadLine};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Archs {
    A6502,
    A65C02,
    A65C816,
    A6502Bytes,
    ARaw,
    InsertToken(InsertToken),
    PadLine(PadLine),
    AlAs65C816(AlAs65c816),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    archs: Vec<Archs>,
    ctx: Context,
    input: PathBuf,
    output: Option<PathBuf>,
}
