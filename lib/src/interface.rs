use std::{
    error::Error,
    fs::{self, File},
    io::Read,
    io::{stdout, Write},
    path::PathBuf,
};

use clap::{ArgEnum, Args, Parser, Subcommand};

use crate::{
    dasm::archs::{
        bytes_read_byte_node, make_arch, make_instructions6502, make_instructions65c02,
        make_instructions65c816, IMMEDIATE_SIZE16, IMMEDIATE_SIZE8,
    },
    dasm::parse_with,
    dasm::Context,
    dasm::Definition,
    dasm::Symbol,
    dasm::TokenAttributes,
    dasm::Word,
};

#[derive(ArgEnum, Debug, Copy, Clone)]
pub enum Archs {
    A6502,
    A65C02,
    A65C816,
}

impl Archs {
    pub fn into_parser(&self, no_default: bool) -> Box<dyn crate::dasm::Parser + 'static> {
        let default = if no_default {
            None
        } else {
            Some(bytes_read_byte_node(TokenAttributes::NewLine, &[]))
        };
        Box::new(match self {
            Self::A6502 => make_arch(&make_instructions6502(IMMEDIATE_SIZE8), default),

            Self::A65C02 => make_arch(&make_instructions65c02(IMMEDIATE_SIZE8), default),

            Self::A65C816 => make_arch(&make_instructions65c816(IMMEDIATE_SIZE16), default),
            // _ => arch_raw(),
        })
    }
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Cli {
    #[clap(subcommand)]
    pub cmds: SubCommands,
}

#[derive(Subcommand, Debug)]
pub enum SubCommands {
    Dasm(Dasm),
    Asm(Asm),
}

#[derive(Args, Debug)]
pub struct Asm {}

#[derive(Args, Debug)]
pub struct Dasm {
    // TODO handle append
    #[clap(long)]
    append: bool,

    #[clap(long)]
    no_default: bool,

    #[clap(short, long)]
    out: Option<PathBuf>,

    #[clap(short, long, default_value = "0")]
    start: usize,

    #[clap(short, long)]
    read: Option<usize>,

    #[clap(short, long, default_value = "0")]
    base: Word,

    #[clap(long, default_value = ":")]
    label_postfix: String,
    #[clap(long, default_value = "")]
    line_postfix: String,

    #[clap(long, default_value = "")]
    label_prefix: String,
    #[clap(long, default_value = "    ")]
    line_prefix: String,

    #[clap(required = true)]
    input: PathBuf,

    #[clap(arg_enum, long, short, default_value = "a6502")]
    archs: Vec<Archs>,

    #[clap(long, parse(try_from_str = parse_def), multiple_occurrences(true))]
    sym: Vec<(String, Word)>,

    #[clap(long, parse(try_from_str = parse_def), multiple_occurrences(true))]
    def: Vec<(String, Word)>,
}

fn parse_def<T, U>(s: &str) -> Result<(T, U), Box<dyn Error + Send + Sync + 'static>>
where
    T: std::str::FromStr,
    T::Err: Error + Send + Sync + 'static,
    U: std::str::FromStr,
    U::Err: Error + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid KEY=value: no `=` found in `{}`", s))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}

pub fn exec_cli(args: &[String]) {
    let args = Cli::parse_from(args);
    match args.cmds {
        SubCommands::Dasm(dasm) => exec_cli_dasm(dasm),
        SubCommands::Asm(asm) => exec_cli_asm(asm),
    }
}

pub fn exec_cli_asm(_args: Asm) {}

pub fn exec_cli_dasm(mut args: Dasm) {
    let mut archs = vec![];
    args.archs
        .iter()
        .for_each(|a| archs.push(a.into_parser(args.no_default)));

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

    args.sym.iter().for_each(|s| {
        ctx.add_symbol(Symbol::new(
            &s.0,
            s.1,
            0,
            crate::dasm::SymbolAttributes::NewLine,
        ))
    });

    args.def
        .iter()
        .for_each(|s| ctx.add_def(Definition::new(&s.0, s.1, 0)));

    parse_with(
        &mut ctx,
        &buffer[args.start..args.start + args.read.unwrap_or(0)],
        &arch_refs,
        &mut |ctx, parsed| {
            writeln!(
                out,
                "{}",
                parsed.output(
                    ctx,
                    &args.line_prefix,
                    &args.line_postfix,
                    &args.label_prefix,
                    &args.label_postfix
                )
            )
            .expect("Write error");
        },
    )
    .expect("Parser error");
}
