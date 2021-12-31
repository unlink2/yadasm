from enum import Enum
from typing import Callable, Dict, List

from ..comparator import always_true
from ..context import Context
from ..node import Node
from ..numfmt import IntFmt
from ..parser import Parser
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


class Parser6502(Parser):
    def __init__(self) -> None:
        nodes: List[Node] = (
            self._make_instruction("lda", self._make_load(0xA0))
            + self._make_instruction("sta", self._make_store(0x80))
            + self._make_instruction("adc", self._make_load(0x60))
            + self._make_instruction("and", self._make_load(0x20))
        )

        Parser.__init__(self, nodes)

    def _get_prefix(self, mode: IntFmt) -> str:
        if mode == IntFmt.HEX:
            return "$"
        elif mode == IntFmt.OCTAL:
            return ""
        elif mode == IntFmt.BINARY:
            return "%"
        else:
            return ""

    def _read_i8_hex_node(
        self,
        prefix: str = "",
        padding: int = 2,
        mode: IntFmt = IntFmt.HEX,
    ) -> Node:
        return Node(
            read_i8_le,
            [],
            always_true,
            lambda ctx, i: f"{prefix}{self._get_prefix(mode)}"
            f"{i:0{padding}{mode.to_literal()}}",
        )

    def _read_i16_hex_node(
        self,
        prefix: str = "",
        padding: int = 4,
        mode: IntFmt = IntFmt.HEX,
    ) -> Node:
        return Node(
            read_i16_le,
            [],
            always_true,
            lambda ctx, i: f"{prefix}{self._get_prefix(mode)}"
            f"{i:0{padding}{mode.to_literal()}}",
        )

    def _append_str(self, value: str = "") -> Node:
        return Node(read_none, [], always_true, lambda ctx, i: value)

    def _make_comparator(self, opcode: int) -> Callable[[Context, int], bool]:
        return lambda ctx, i: i == opcode

    def _make_load(self, high: int) -> Dict[InstructionModes, int]:
        """
        creates a load instruction based on common instruction modes
        only supply hi nibble
        """
        return {
            InstructionModes.IMMEDIATE: high | 0x09,
        } | self._make_store(high)

    def _make_store(self, high: int) -> Dict[InstructionModes, int]:
        return {
            InstructionModes.ZEROPAGE: high | 0x05,
            InstructionModes.ZEROPAGEX: high | 0x15,
            InstructionModes.ABSOLUTE: high | 0x0D,
            InstructionModes.ABSOLUTEX: high | 0x1D,
            InstructionModes.ABSOLUTEY: high | 0x19,
            InstructionModes.INDIRECTX: high | 0x01,
            InstructionModes.INDIRECTY: high | 0x11,
        }

    # helper to create instruction nodes for the most common
    # instruction modes
    def _make_instruction(
        self, name: str, modes: Dict[InstructionModes, int]
    ) -> List[Node]:
        nodes: List[Node] = []

        for mode, opcode in modes.items():
            if mode == InstructionModes.IMMEDIATE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node("#")],
                    )
                )
            if mode == InstructionModes.ZEROPAGE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node()],
                    )
                )
            if mode == InstructionModes.ZEROPAGEX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionModes.ABSOLUTE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i16_hex_node()],
                    )
                )
            elif mode == InstructionModes.ABSOLUTEX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i16_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionModes.ABSOLUTEY:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i16_hex_node(), self._append_str(", y")],
                    )
                )
            elif mode == InstructionModes.INDIRECTX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("("),
                            self._read_i8_hex_node(),
                            self._append_str(", x)"),
                        ],
                    )
                )
            elif mode == InstructionModes.INDIRECTY:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("("),
                            self._read_i8_hex_node(),
                            self._append_str("), y"),
                        ],
                    )
                )

        return nodes
