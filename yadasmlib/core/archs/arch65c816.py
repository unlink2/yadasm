import ctypes
from enum import Enum
from typing import Any, Dict, List

from ..comparator import always_true
from ..context import Context
from ..node import Node
from ..numfmt import IntFmt
from ..reader import read_i8_le, read_i16_le, read_i24_le
from .arch65c02 import Parser65C02
from .arch6502 import InstructionMode as InstructionMode6502
from .arch6502 import fmt_hex_label, grab_label


def rel_i16_to_addr(ctx: Context, i: Any) -> int:
    """converts a python int to a relative 8 bit value"""
    return ctx.address + int(ctypes.c_int16(i).value) + 2


def grab_label_i8_rel(ctx: Context, i: Any) -> Any:
    addr = rel_i16_to_addr(ctx, i)
    return grab_label(ctx, addr)


class InstructionModeException(Exception):
    pass


class InstructionMode(Enum):
    STACKS = 0
    DIRECT_PAGE_INDIRECT_LONG = 1

    ABSOLUTE_LONG = 2

    STACKS_Y = 3
    DIRECT_PAGE_INDIRECT_LONG_Y = 4
    ABSOLUTE_LONG_Y = 5

    IMPLIED = 6

    JMP_ABSOLUTE_LONG = 7

    def mask(self, opcode: int) -> int:
        """returns the mask used to create the final instruction (bits bbbb)"""
        ccbits = self._extract_ccbits(opcode)

        if ccbits == 0b11:
            if self == self.STACKS:
                return self._apply(0x03)
            elif self == self.DIRECT_PAGE_INDIRECT_LONG:
                return self._apply(0x07)
            elif self == self.ABSOLUTE_LONG:
                return self._apply(0x0F)
            elif self == self.STACKS_Y:
                return self._apply(0x13)
            elif self == self.DIRECT_PAGE_INDIRECT_LONG_Y:
                return self._apply(0x17)
            elif self == self.ABSOLUTE_LONG_Y:
                return self._apply(0x1F)
            elif self == self.IMPLIED:
                return self._apply(opcode)
            else:
                raise InstructionModeException
        elif self in (self.IMPLIED, self.JMP_ABSOLUTE_LONG):
            # single byte instructions!
            return self._apply(opcode)
        else:
            raise InstructionModeException()

    def _extract_ccbits(self, opcode: int) -> int:
        ccbits_mask = 0b00000011
        return opcode & ccbits_mask

    def _apply(self, unmasked: int) -> int:
        # addressing mode mask
        addr_mode_mask = 0b00011100
        return unmasked & addr_mode_mask


class Parser65C816Emulated(Parser65C02):
    """Emulated mode for 65C816"""

    def __init__(self, nodes: List[Node] = None) -> None:
        Parser65C02.__init__(self, nodes)
        self.nodes += self._make_instruction(
            "xce", self._make_implied(0xFB)
        ) + self._make_instruction_65c816_emulated("cop", self._make_cop(0x02))

    def _make_cop(self, mask: int) -> Dict[InstructionMode6502, int]:
        return {InstructionMode6502.IMMEDIATE: mask}

    def _make_instruction_65c816_emulated(
        self, name: str, modes: Dict[InstructionMode6502, int]
    ) -> List[Node]:
        nodes = []
        for mode, opcode in modes.items():
            if mode == InstructionMode6502.IMMEDIATE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [Parser65C02._read_immediate_hex_node(self, "#")],
                    )
                )
        return nodes


class Parser65C816(Parser65C816Emulated):
    """Native mode for 65C816"""

    def __init__(self, nodes: List[Node] = None) -> None:
        Parser65C816Emulated.__init__(self, nodes)

        self.nodes += (
            self._make_instruction_65c816(
                "ora", self._make_extended(self._opcode(0x03))
            )
            + self._make_instruction_65c816(
                "and", self._make_extended(self._opcode(0x23))
            )
            + self._make_instruction_65c816(
                "eor", self._make_extended(self._opcode(0x43))
            )
            + self._make_instruction_65c816(
                "adc", self._make_extended(self._opcode(0x63))
            )
            + self._make_instruction_65c816(
                "sta", self._make_extended(self._opcode(0x83))
            )
            + self._make_instruction_65c816(
                "lda", self._make_extended(self._opcode(0xA3))
            )
            + self._make_instruction_65c816(
                "cmp", self._make_extended(self._opcode(0xC3))
            )
            + self._make_instruction_65c816(
                "sbc", self._make_extended(self._opcode(0xE3))
            )
            + self._make_instruction("phd", self._make_implied(0x0B))
            + self._make_instruction("pld", self._make_implied(0x2B))
            + self._make_instruction("phk", self._make_implied(0x4B))
            + self._make_instruction("rtl", self._make_implied(0x6B))
            + self._make_instruction("phb", self._make_implied(0x8B))
            + self._make_instruction("plb", self._make_implied(0xAB))
            + self._make_instruction("wai", self._make_implied(0xCB))
            + self._make_instruction("xba", self._make_implied(0xEB))
            + self._make_instruction("tcs", self._make_implied(0x1B))
            + self._make_instruction("tsc", self._make_implied(0x3B))
            + self._make_instruction("tcd", self._make_implied(0x5B))
            + self._make_instruction("tdc", self._make_implied(0x7B))
            + self._make_instruction("txy", self._make_implied(0x9B))
            + self._make_instruction("tyx", self._make_implied(0xBB))
            + self._make_instruction("stp", self._make_implied(0xDB))
        )

    def _make_extended(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ABSOLUTE_LONG: mask
            | InstructionMode.ABSOLUTE_LONG.mask(mask),
            InstructionMode.ABSOLUTE_LONG_Y: mask
            | InstructionMode.ABSOLUTE_LONG_Y.mask(mask),
            InstructionMode.DIRECT_PAGE_INDIRECT_LONG: mask
            | InstructionMode.DIRECT_PAGE_INDIRECT_LONG.mask(mask),
            InstructionMode.DIRECT_PAGE_INDIRECT_LONG_Y: mask
            | InstructionMode.DIRECT_PAGE_INDIRECT_LONG_Y.mask(mask),
            InstructionMode.STACKS: mask | InstructionMode.STACKS.mask(mask),
            InstructionMode.STACKS_Y: mask
            | InstructionMode.STACKS_Y.mask(mask),
        }

    def _read_immediate_hex_node(
        self, prefix: str = "", padding: int = 2, mode: IntFmt = IntFmt.HEX
    ) -> Node:
        return self._read_hex_node(prefix, padding, mode)

    def _read_rel_label_node(self) -> Node:
        return Node(
            read_i16_le,
            [grab_label_i8_rel],
            always_true,
            lambda ctx, i: f"label_{fmt_hex_label(i)}",
            [],
        )

    def _read_long_hex_node(
        self,
        prefix: str = "",
        padding: int = 4,
        mode: IntFmt = IntFmt.HEX,
    ) -> Node:
        return Node(
            read_i24_le,
            [],
            always_true,
            lambda ctx, i: f"{prefix}{self._get_prefix(mode)}"
            f"{i:0{padding}{mode.to_literal()}}",
        )

    def _make_instruction_65c816(
        self, name: str, modes: Dict[InstructionMode, int]
    ) -> List[Node]:
        nodes: List[Node] = []
        for mode, opcode in modes.items():
            if mode == InstructionMode.ABSOLUTE_LONG:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_long_hex_node()],
                    )
                )
            elif mode == InstructionMode.ABSOLUTE_LONG_Y:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_long_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionMode.DIRECT_PAGE_INDIRECT_LONG:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("["),
                            self._read_short_hex_node(),
                            self._append_str("]"),
                        ],
                    )
                )
            elif mode == InstructionMode.DIRECT_PAGE_INDIRECT_LONG_Y:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("["),
                            self._read_short_hex_node(),
                            self._append_str("], y"),
                        ],
                    )
                )
            elif mode == InstructionMode.STACKS:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._read_short_hex_node(),
                            self._append_str(", s"),
                        ],
                    )
                )
            elif mode == InstructionMode.STACKS_Y:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("("),
                            self._read_short_hex_node(),
                            self._append_str(", s), y"),
                        ],
                    )
                )

        return nodes
