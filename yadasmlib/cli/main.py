import argparse
from typing import List
from yadasmlib.core.archs.arch6502 import Parser6502, Parser6502Bytes
from yadasmlib.core.archs.arch65c02 import Parser65C02, Parser65C02Bytes
from yadasmlib.core.archs.arch65c816 import (
    Parser65C816,
    Parser65C816Bytes,
    Parser65C816Emulated,
    Parser65C816EmulatedBytes,
)
from yadasmlib.core.context import Context, Middleware
from yadasmlib.core.file import Binary

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
    args = parser.parse_args(argv)

    if args.arch not in _archs:
        print("Invalid architecture")
        return -1

    with open(args.file, mode="rb") as file:
        file_content = file.read()
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

        for line in lines:
            print(line)

    return 0
