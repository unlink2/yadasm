from enum import Enum
from typing import Callable, Dict, List

from ..comparator import always_true
from ..context import Context
from ..node import Node
from ..numfmt import IntFmt
from ..reader import read_i8_le, read_i16_le, read_none


class InstructionModes(Enum):
    IMMEDIATE = 0
    ZEROPAGE = 1
    ZEROPAGEX = 2
    ABSOLUTE = 3
    ABSOLUTEX = 4
    ABSOLUTEY = 5
    INDIRECTX = 6
    INDIRECTY = 7


def _get_prefix(mode: IntFmt) -> str:
    if mode == IntFmt.HEX:
        return "$"
    elif mode == IntFmt.OCTAL:
        return ""
    elif mode == IntFmt.BINARY:
        return "%"
    else:
        return ""


def _read_i8_hex_node(
    prefix: str = "",
    padding: int = 2,
    mode: IntFmt = IntFmt.HEX,
) -> Node:
    return Node(
        read_i8_le,
        [],
        always_true,
        lambda ctx, i: f"{prefix}{_get_prefix(mode)}"
        f"{i:0{padding}{mode.to_literal()}}",
    )


def _read_i16_hex_node(
    prefix: str = "",
    padding: int = 4,
    mode: IntFmt = IntFmt.HEX,
) -> Node:
    return Node(
        read_i16_le,
        [],
        always_true,
        lambda ctx, i: f"{prefix}{_get_prefix(mode)}"
        f"{i:0{padding}{mode.to_literal()}}",
    )


def _append_str(value: str = "") -> Node:
    return Node(read_none, [], always_true, lambda ctx, i: value)


def _make_comparator(opcode: int) -> Callable[[Context, int], bool]:
    return lambda ctx, i: i == opcode


# helper to create instruction nodes for the most common
# instruction modes
def _make_instruction(
    name: str, modes: Dict[InstructionModes, int]
) -> List[Node]:
    nodes: List[Node] = []

    for mode, opcode in modes.items():
        if mode == InstructionModes.IMMEDIATE:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [_read_i8_hex_node("#")],
                )
            )
        if mode == InstructionModes.ZEROPAGE:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [_read_i8_hex_node()],
                )
            )
        if mode == InstructionModes.ZEROPAGEX:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [_read_i8_hex_node(), _append_str(", x")],
                )
            )
        elif mode == InstructionModes.ABSOLUTE:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [_read_i16_hex_node()],
                )
            )
        elif mode == InstructionModes.ABSOLUTEX:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [_read_i16_hex_node(), _append_str(", x")],
                )
            )
        elif mode == InstructionModes.ABSOLUTEY:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [_read_i16_hex_node(), _append_str(", y")],
                )
            )
        elif mode == InstructionModes.INDIRECTX:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [
                        _append_str("("),
                        _read_i8_hex_node(),
                        _append_str(", x)"),
                    ],
                )
            )
        elif mode == InstructionModes.INDIRECTY:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    _make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [
                        _append_str("("),
                        _read_i8_hex_node(),
                        _append_str("), y"),
                    ],
                )
            )

    return nodes


arch6502: List[Node] = (
    _make_instruction(
        "lda",
        {
            InstructionModes.IMMEDIATE: 0xA9,
            InstructionModes.ZEROPAGE: 0xA5,
            InstructionModes.ZEROPAGEX: 0xB5,
            InstructionModes.ABSOLUTE: 0xAD,
            InstructionModes.ABSOLUTEX: 0xBD,
            InstructionModes.ABSOLUTEY: 0xB9,
            InstructionModes.INDIRECTX: 0xA1,
            InstructionModes.INDIRECTY: 0xB1,
        },
    )
    + _make_instruction("sta", {InstructionModes.ABSOLUTE: 0x8D})
)
