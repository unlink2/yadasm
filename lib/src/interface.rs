use std::{
    fs::{self, File},
    io::Read,
    io::{Write, stdout},
    path::PathBuf,
};

use clap::{ArgEnum, Parser};

use crate::{
    archs::{
        arch_raw, bytes_read_byte_node, make_arch, make_instructions6502, make_instructions65c02,
        make_instructions65c816, IMMEDIATE_SIZE16, IMMEDIATE_SIZE8,
    },
     parse_with, Context, TokenAttributes, Word,
};

#[derive(ArgEnum, Debug, Copy, Clone)]
pub enum Archs {
    A6502,
    A65C02,
    A65C816,
    AlAs65C816,
    A6502Bytes,
    ARaw,
    InsertToken,
    PadLine,
}

impl Archs {
    pub fn into_parser(&self) -> Box<dyn crate::Parser + 'static> {
        Box::new(match self {
            Self::A6502 => make_arch(
                &make_instructions6502(IMMEDIATE_SIZE8),
                Some(bytes_read_byte_node(TokenAttributes::NewLine, &[])),
            ),
            Self::A65C02 => make_arch(
                &make_instructions65c02(IMMEDIATE_SIZE8),
                Some(bytes_read_byte_node(TokenAttributes::NewLine, &[])),
            ),
            Self::A65C816 => make_arch(
                &make_instructions65c816(IMMEDIATE_SIZE16),
                Some(bytes_read_byte_node(TokenAttributes::NewLine, &[])),
            ),
            _ => arch_raw(),
        })
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
    #[clap(long)]
    append: bool,

    #[clap(short, long)]
    out: Option<PathBuf>,

    #[clap(short, long, default_value = "0")]
    start: usize,

    #[clap(short, long)]
    read: Option<usize>,

    #[clap(short, long, default_value = "0")]
    base: Word,

    #[clap(short, long, default_value = ":")]
    label_postfix: String,

    #[clap(required = true)]
    input: PathBuf,

    #[clap(arg_enum, long, short, default_value = "a6502")]
    archs: Vec<Archs>,
}

pub fn exec_cli(args: &[String]) {
    let mut args = Cli::parse_from(args);

    let mut archs = vec![];
    args.archs.iter().for_each(|a| archs.push(a.into_parser()));

    let mut arch_refs = vec![];
    archs.iter().for_each(|a| arch_refs.push(a.as_ref()));

    let mut f = File::open(&args.input).expect("No file found");
    let meta = fs::metadata(&args.input).expect("Unable to access file");
    let mut buffer = vec![0; meta.len() as usize];
    f.read_exact(&mut buffer).expect("Read error");

    if args.read.is_none() {
        args.read = Some(buffer.len() - args.start)
    }

    let mut out: Box<dyn Write> = if let Some(out) = args.out {
        Box::new(File::create(out).expect("Unable to create output file"))
    } else {
        Box::new(stdout()) 
    };

    let mut ctx = Context::new(args.base, args.base + args.read.unwrap_or(0) as Word);
    parse_with(
        &mut ctx,
        &buffer[args.start..args.start + args.read.unwrap_or(0)],
        &arch_refs,
        &mut |ctx, parsed| {
            writeln!(out, "{}", parsed.output(ctx, "", "", "", &args.label_postfix)).expect("Write error");
        },
    )
    .expect("Parser error");
}
