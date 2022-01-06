import argparse
import logging
from typing import List, Optional

from lyadasm.core.archs.arch65c02 import Parser65C02, Parser65C02Bytes
from lyadasm.core.archs.arch65c816 import (
    Parser65C816,
    Parser65C816Bytes,
    Parser65C816Emulated,
    Parser65C816EmulatedBytes,
)
from lyadasm.core.archs.arch6502 import Parser6502, Parser6502Bytes
from lyadasm.core.context import Context, Middleware
from lyadasm.core.file import Binary

_archs = {
    "6502": Parser6502(),
    "6502-byte": Parser6502Bytes(),
    "65c02": Parser65C02(),
    "65c02-byte": Parser65C02Bytes(),
    "65c816": Parser65C816(),
    "65c816-byte": Parser65C816Bytes(),
    "65c816-emu": Parser65C816Emulated(),
    "65c816-emu-byte": Parser65C816EmulatedBytes(),
}


def _write_to_file(out: Optional[str], lines: List[str], append: bool) -> None:
    if out is None:
        for line in lines:
            print(line)
    else:
        outmode = "w"
        if append:
            outmode = "a"

        with open(out, outmode, encoding="UTF-8") as outfile:
            outfile.write("\n".join(lines))


def _read_from_file(file_path: str) -> bytes:
    with open(file_path, mode="rb") as file:
        file_content = file.read()
        return file_content
    return ""


def main(argv: List[str], middlewares: List[Middleware] = None) -> int:
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
        type=int,
        default=0,
        help="Start parsing input file at offset",
    )
    parser.add_argument(
        "--file-end-offset",
        type=int,
        default=None,
        help="Stop parsing input file at offset",
    )
    parser.add_argument(
        "--start-addr",
        type=int,
        default=0,
        help="Starting address",
    )
    parser.add_argument(
        "--end-addr",
        type=int,
        default=None,
        help="End addres",
    )
    parser.add_argument(
        "--label-postfix",
        type=str,
        default=":",
        help="",
    )
    parser.add_argument(
        "--loglevel",
        type=str,
        default="ERROR",
        help="Loglevel: DEBUG;INFO;WARNING;ERROR;CRITICAL;FATAL;NOTSET",
    )
    parser.add_argument(
        "-o",
        type=str,
        default=None,
        help="Output file",
    )
    parser.add_argument(
        "--quiet",
        action="store_true",
        help="Disables all logging output",
    )
    parser.add_argument(
        "--append",
        action="store_true",
        help="Append to output file",
    )
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
    ctx = Context(
        args.start_addr,
        symbol_poxtfix=args.label_postfix,
        end_address=args.end_addr,
        middlewares=middlewares,
    )
    lines = _archs[args.arch].parse(ctx, bin_file)

    _write_to_file(args.o, lines, args.append)

    return 0
