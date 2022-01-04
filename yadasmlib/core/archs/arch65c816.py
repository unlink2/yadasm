import ctypes
from enum import Enum
from typing import List, Dict, Any

from ..node import Node
from .arch6502 import grab_label, fmt_hex_label
from .arch65c02 import Parser65C02
from ..reader import read_i16_le, read_i8_le, read_i24_le
from ..comparator import always_true
from ..context import Context
from ..numfmt import IntFmt


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

    def mask(self, opcode: int) -> int:
        """returns the mask used to create the final instruction (bits bbbb)"""
        ccbits = self._extract_ccbits(opcode)
        aaaabits = self._extract_aaaabits(opcode)

        if ccbits == 0b11 and aaaabits == 0b01:
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
        elif aaaabits in (0b010, 0b110):
            # single byte instructions!
            return self._apply(opcode)
        else:
            # remaining instructions are assigned "randomly"
            return self._apply(opcode)

    def _extract_ccbits(self, opcode: int) -> int:
        ccbits_mask = 0b00000011
        return opcode & ccbits_mask

    def _extract_aaaabits(self, opcode: int) -> int:
        aaaa_mask = 0b11100000
        return (opcode & aaaa_mask) >> 5

    def _apply(self, unmasked: int) -> int:
        # addressing mode mask
        addr_mode_mask = 0b00011100
        return unmasked & addr_mode_mask


class Parser65C816(Parser65C02):
    def __init__(self, nodes: List[Node] = None) -> None:
        if nodes is None:
            nodes = []

        Parser65C02.__init__(self, nodes)

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

        return nodes
