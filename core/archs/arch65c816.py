from enum import Enum
from typing import List

from ..node import Node
from .arch65c02 import Parser65C02


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
