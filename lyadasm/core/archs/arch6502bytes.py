from typing import List

from ..context import Context, Line
from ..file import Binary
from ..node import Node
from ..parser import Parser
from ..reader import read_i8_le, read_i16_le, read_i24_le
from ..numfmt import IntFmt, Endianess
from ..comparator import always_true


class Byte6502Node(Node):
    def make_line(self, text: str, size: int) -> Line:
        return Line(text, size, "", "")


class ByteParser6502(Parser):
    """Custom node-based parser for byte data on a 6502-family cpu"""

    def __init__(self, nodes: List[Node] = None, length: int = 0) -> None:
        Parser.__init__(self, nodes, should_build_lookup=False)
        self.length = length

    def _should_parse(self, ctx: Context, file: Binary) -> bool:
        return self.length > 0 and Parser._should_parse(self, ctx, file)

    def _next(self) -> None:
        self.length -= 1

    def _read_opcode(self, _ctx: Context, file: Binary) -> int:
        byte = file.read(1)
        if byte is None:
            return -1
        opcode = int.from_bytes(byte, Endianess.LITTLE.to_literal())

        return opcode

    @staticmethod
    def _get_prefix(mode: IntFmt) -> str:
        if mode == IntFmt.HEX:
            return "$"
        elif mode == IntFmt.OCTAL:
            return ""
        elif mode == IntFmt.BINARY:
            return "%"
        else:
            return ""

    @staticmethod
    def make_i8_node(
        children: List[Node],
        prefix: str = "",
        padding: int = 2,
        mode: IntFmt = IntFmt.HEX,
    ) -> Node:
        return Byte6502Node(
            read_i8_le,
            [],
            always_true,
            lambda ctx, i: f"{prefix}!byte {ByteParser6502._get_prefix(mode)}"
            f"{i:0{padding}{mode.to_literal()}}",
            children=children,
        )

    @staticmethod
    def make_i16_node(
        children: List[Node],
        prefix: str = "",
        padding: int = 2,
        mode: IntFmt = IntFmt.HEX,
    ) -> Node:
        return Byte6502Node(
            read_i16_le,
            [],
            always_true,
            lambda ctx, i: f"{prefix}!word {ByteParser6502._get_prefix(mode)}"
            f"{i:0{padding}{mode.to_literal()}}",
            children=children,
        )

    @staticmethod
    def make_i24_node(
        children: List[Node],
        prefix: str = "",
        padding: int = 2,
        mode: IntFmt = IntFmt.HEX,
    ) -> Node:
        return Byte6502Node(
            read_i24_le,
            [],
            always_true,
            lambda ctx, i: f"{prefix}!le24 {ByteParser6502._get_prefix(mode)}"
            f"{i:0{padding}{mode.to_literal()}}",
            children=children,
        )
