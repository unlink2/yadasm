from enum import Enum
from typing import Callable, Dict, List

from ..comparator import always_true
from ..context import Context
from ..node import Node
from ..numfmt import IntFmt
from ..parser import Parser
from ..reader import read_i8_le, read_i16_le, read_none

# instruction encoding:
# aaabbbcc;
# aaa and cc bits determine opcode;
# bbb bits determine addressing mode;


class InstructionModes(Enum):
    IMMEDIATE = 1
    ZEROPAGE = 2
    ZEROPAGEX = 3
    ABSOLUTE = 4
    ABSOLUTEX = 5
    ABSOLUTEY = 6
    INDIRECTX = 7
    INDIRECTY = 8
    ACCUMULATOR = 9

    def mask(self) -> int:
        if self == self.IMMEDIATE:
            return self._apply(0x69)
        elif self == self.ZEROPAGE:
            return self._apply(0x65)
        elif self == self.ZEROPAGEX:
            return self._apply(0x75)
        elif self == self.ABSOLUTE:
            return self._apply(0x6D)
        elif self == self.ABSOLUTEX:
            return self._apply(0x7D)
        elif self == self.ABSOLUTEY:
            return self._apply(0x79)
        elif self == self.INDIRECTX:
            return self._apply(0x61)
        elif self == self.INDIRECTY:
            return self._apply(0x71)
        elif self == self.ACCUMULATOR:
            return self._apply(0x0A)
        else:
            return 0x00

    def _apply(self, unmasked: int) -> int:
        # addressing mode mask
        addr_mode_mask = 0b00011100
        return unmasked & addr_mode_mask


class Parser6502(Parser):
    def __init__(self) -> None:
        nodes: List[Node] = (
            self._make_instruction("lda", self._make_load(self._opcode(0xA9)))
            + self._make_instruction(
                "sta", self._make_store(self._opcode(0x85))
            )
            + self._make_instruction(
                "adc", self._make_load(self._opcode(0x69))
            )
            + self._make_instruction(
                "and", self._make_load(self._opcode(0x29))
            )
            + self._make_instruction(
                "asl", self._make_logic(self._opcode(0x1E))
            )
        )

        Parser.__init__(self, nodes)

    def _opcode(self, unmasked: int) -> int:
        # opcode mask
        opcode_mask = 0b11100011
        return unmasked & opcode_mask

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

    def _make_load(self, mask: int) -> Dict[InstructionModes, int]:
        """
        creates a load instruction based on common instruction modes
        """
        return {
            InstructionModes.IMMEDIATE: mask
            | InstructionModes.IMMEDIATE.mask(),
        } | self._make_store(mask)

    def _make_store(self, mask: int) -> Dict[InstructionModes, int]:
        return {
            InstructionModes.ZEROPAGE: mask | InstructionModes.ZEROPAGE.mask(),
            InstructionModes.ZEROPAGEX: mask
            | InstructionModes.ZEROPAGEX.mask(),
            InstructionModes.ABSOLUTE: mask | InstructionModes.ABSOLUTE.mask(),
            InstructionModes.ABSOLUTEX: mask
            | InstructionModes.ABSOLUTEX.mask(),
            InstructionModes.ABSOLUTEY: mask
            | InstructionModes.ABSOLUTEY.mask(),
            InstructionModes.INDIRECTX: mask
            | InstructionModes.INDIRECTX.mask(),
            InstructionModes.INDIRECTY: mask
            | InstructionModes.INDIRECTY.mask(),
        }

    def _make_logic(self, mask: int) -> Dict[InstructionModes, int]:
        return {
            InstructionModes.ACCUMULATOR: mask
            | InstructionModes.ACCUMULATOR.mask(),
            InstructionModes.ZEROPAGE: mask | InstructionModes.ZEROPAGE.mask(),
            InstructionModes.ZEROPAGEX: mask
            | InstructionModes.ZEROPAGEX.mask(),
            InstructionModes.ABSOLUTE: mask | InstructionModes.ABSOLUTE.mask(),
            InstructionModes.ABSOLUTEX: mask
            | InstructionModes.ABSOLUTEX.mask(),
        }

    # helper to create instruction nodes for the most common
    # instruction modes
    def _make_instruction(
        self, name: str, modes: Dict[InstructionModes, int]
    ) -> List[Node]:
        nodes: List[Node] = []

        for mode, opcode in modes.items():
            if mode == InstructionModes.ACCUMULATOR:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} A",
                        [],
                    )
                )
            elif mode == InstructionModes.IMMEDIATE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node("#")],
                    )
                )
            elif mode == InstructionModes.ZEROPAGE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node()],
                    )
                )
            elif mode == InstructionModes.ZEROPAGEX:
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
