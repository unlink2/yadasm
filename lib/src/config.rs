use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::{Context, InsertToken, PadLine, AlAs65c816};

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum Archs {
    A6502,
    A65C02,
    A65C816,
    A6502Bytes,
    ARaw,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum Plugins {
    InsertToken(InsertToken),
    PadLine(PadLine),
    AlAs65C816(AlAs65c816),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Config {
    archs: Vec<Archs>,
    plugins: Vec<Plugins>,
    ctx: Context,
    input: PathBuf,
    output: Option<PathBuf>,
}
