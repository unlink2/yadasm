import argparse
import logging
from typing import IO, Dict, List, Optional

from lyadasm.core.archs.arch65c02 import Parser65C02, Parser65C02Bytes
from lyadasm.core.archs.arch65c816 import Parser65C816, Parser65C816Bytes
from lyadasm.core.archs.arch6502 import Parser6502, Parser6502Bytes
from lyadasm.core.context import Context, Middleware
from lyadasm.core.file import Binary, PrintOutput, StreamOutput

_archs = {
    "6502": Parser6502(),
    "6502-byte": Parser6502Bytes(),
    "65c02": Parser65C02(),
    "65c02-byte": Parser65C02Bytes(),
    "65c816": Parser65C816(),
    "65c816-byte": Parser65C816Bytes(),
    "65c816-emu": Parser65C816(start_immediate_len=2),
    "65c816-emu-byte": Parser65C816Bytes(start_immediate_len=2),
}


def _read_from_file(file_path: str) -> bytes:
    with open(file_path, mode="rb") as file:
        file_content = file.read()
        return file_content
    return ""


def main(
    argv: List[str],
    middlewares: List[Middleware] = None,
    middlewareout: Dict[str, IO] = None,
) -> int:
    if middlewareout is None:
        middlewareout = {}

    parser = argparse.ArgumentParser(description="yadasm")
    parser.add_argument("file", type=str, help="the input file")
    parser.add_argument(
        "--arch",
        type=str,
        default="6502-byte",
        help=f"The target cpu architecture {_archs.keys()}",
    )
    parser.add_argument(
        "--file-start-offset",
        "-b",
        type=int,
        default=0,
        help="Start parsing input file at offset",
    )
    parser.add_argument(
        "--file-end-offset",
        "-x",
        type=int,
        default=None,
        help="Stop parsing input file at offset",
    )
    parser.add_argument(
        "--start-addr",
        "-s",
        type=int,
        default=0,
        help="Starting address",
    )
    parser.add_argument(
        "--end-addr",
        "-e",
        type=int,
        default=None,
        help="End addres",
    )
    parser.add_argument(
        "--label-postfix",
        "-p",
        type=str,
        default=":",
        help="",
    )
    parser.add_argument(
        "--loglevel",
        "-v",
        type=str,
        default="ERROR",
        help="Loglevel: DEBUG;INFO;WARNING;ERROR;CRITICAL;FATAL;NOTSET",
    )
    parser.add_argument(
        "-o",
        "--out",
        type=str,
        default=None,
        help="Output file",
    )
    parser.add_argument(
        "--quiet",
        "-q",
        action="store_true",
        help="Disables all logging output",
    )
    parser.add_argument(
        "--append",
        action="store_true",
        help="Append to output file",
    )
            
help=""
    # parser.add_argument(
    #    "-middleware",
    #    "-m",
    #    action="append",
    #    type=str,
    #    default=None,
    #    help="Load a module containing a Middleware class",
    # )
    # parser.add_argument(
    #    "--middleware-out",
    #    "-w",
    #    action="append",
    #    type=str,
    #    nargs=2,
    #    default=None,
    #    help="Middleware output tag/file",
    # )
    args = parser.parse_args(argv)

    numeric_level = getattr(logging, args.loglevel.upper(), None)
    if not isinstance(numeric_level, int):
        print(f"Invalid log level: {args.loglevel}")
        return -1
    logging.basicConfig(level=numeric_level)
    logging.getLogger().disabled = args.quiet

    if args.arch not in _archs:
        print(f"Invalid architecture: {args.arch}")
        return -1

    file_content = _read_from_file(args.file)
    bin_file = Binary(
        file_content, args.file_start_offset, args.file_end_offset
    )

    output: Optional[StreamOutput | PrintOutput] = None
    if args.out is None:
        output = PrintOutput()
    else:
        outmode = "w"
        if args.append:
            outmode = "a"
        output = StreamOutput(open(args.out, outmode, encoding="UTF-8"))

    ctx = Context(
        args.start_addr,
        symbol_poxtfix=args.label_postfix,
        end_address=args.end_addr,
        middlewares=middlewares,
        output=output,
        unbuffered_lines=True,
    )
    _archs[args.arch].parse(ctx, bin_file)

    output.close()

    return 0
