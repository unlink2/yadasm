from enum import Enum
from typing import Dict, List

from ..node import Node
from ..reader import read_i8_le
from .arch6502 import Parser6502


class InstructionModeException(Exception):
    pass


class InstructionMode(Enum):
    ZEROPAGE_INDIRECT = 0

    JMP_ABSOLUTE_X = 1

    ZEROPAGEX = 2
    ABSOLUTEX = 3

    ZEROPAGE = 4
    ABSOLUTE = 5

    RELATIVE = 6
    IMPLIED = 7
    IMMEDIATE = 8
    ACCUMULATOR = 9

    def mask(self, opcode: int) -> int:
        """returns the mask used to create the final instruction (bits bbbb)"""
        ccbits = self._extract_ccbits(opcode)

        if ccbits == 0b10:
            if self == self.ZEROPAGE_INDIRECT:
                return self._apply(0x12)
            else:
                raise InstructionModeException()
        elif ccbits == 0b00:
            return self._mask_cc00()
        elif self == self.IMMEDIATE:
            # BIT #
            return self._apply(0x89)
        else:
            return self._apply(opcode)

    def _mask_cc00(self) -> int:
        if self == self.ZEROPAGEX:
            return self._apply(0x04)
        elif self == self.ABSOLUTE:
            return self._apply(0x1C)
        elif self == self.JMP_ABSOLUTE_X:
            return self._apply(0x7C)
        elif self == self.ZEROPAGEX:
            return self._apply(0x74)
        elif self == self.ABSOLUTEX:
            return self._apply(0x9E)
        elif self == self.ZEROPAGE:
            return self._apply(0x14)
        else:
            raise InstructionModeException()

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


class Parser65C02(Parser6502):
    def __init__(self, nodes: List[Node] = None) -> None:
        Parser6502.__init__(self, nodes)

        self.nodes += (
            self._make_instruction_65c02(
                "ora", self._make_zp_indirect(self._opcode(0x12))
            )
            + self._make_instruction_65c02(
                "and", self._make_zp_indirect(self._opcode(0x32))
            )
            + self._make_instruction_65c02(
                "eor", self._make_zp_indirect(self._opcode(0x52))
            )
            + self._make_instruction_65c02(
                "adc", self._make_zp_indirect(self._opcode(0x72))
            )
            + self._make_instruction_65c02(
                "sta", self._make_zp_indirect(self._opcode(0x92))
            )
            + self._make_instruction_65c02(
                "lda", self._make_zp_indirect(self._opcode(0xB2))
            )
            + self._make_instruction_65c02(
                "cmp", self._make_zp_indirect(self._opcode(0xD2))
            )
            + self._make_instruction_65c02(
                "sbc", self._make_zp_indirect(self._opcode(0xF2))
            )
            + self._make_instruction_65c02(
                "jmp", self._make_jmp_abs_x(self._opcode(0x7C))
            )
            + self._make_instruction_65c02("bit", self._make_bit_ext())
            + self._make_instruction_65c02("tsb", self._make_tsb())
            + self._make_instruction_65c02(
                "trb", self._make_trb(self._opcode(0x1C))
            )
            + self._make_instruction_65c02(
                "stz", self._make_stz(self._opcode(0x74))
            )
            + self._make_instruction("bra", self._make_branch(0x80))
            + self._make_instruction("inc", self._make_acc(0x1A))
            + self._make_instruction("dec", self._make_acc(0x3A))
            + self._make_instruction("phy", self._make_implied(0x5A))
            + self._make_instruction("ply", self._make_implied(0x7A))
            + self._make_instruction("phx", self._make_implied(0xDA))
            + self._make_instruction("plx", self._make_implied(0xFA))
        )

    def _make_zp_indirect(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE_INDIRECT: mask
            | InstructionMode.ZEROPAGE_INDIRECT.mask(mask)
        }

    def _make_jmp_abs_x(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.JMP_ABSOLUTE_X: mask
            | InstructionMode.JMP_ABSOLUTE_X.mask(mask)
        }

    def _make_bit_ext(self) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGEX: 0x34,
            InstructionMode.ABSOLUTEX: 0x3C,
            InstructionMode.IMMEDIATE: 0x89,  # special case
        }

    def _make_trb(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
        }

    def _make_stz(self, _mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGEX: 0x74,
            InstructionMode.ABSOLUTEX: 0x9E,
            InstructionMode.ZEROPAGE: 0x64,
            InstructionMode.ABSOLUTE: 0x9C,
        }

    def _make_tsb(self) -> Dict[InstructionMode, int]:
        return {InstructionMode.ZEROPAGE: 0x04, InstructionMode.ABSOLUTE: 0x0C}

    def _make_instruction_65c02(
        self, name: str, modes: Dict[InstructionMode, int]
    ) -> List[Node]:
        nodes: List[Node] = []

        for mode, opcode in modes.items():
            if mode == InstructionMode.ZEROPAGE_INDIRECT:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("("),
                            self._read_short_hex_node(),
                            self._append_str(")"),
                        ],
                    )
                )
            elif mode == InstructionMode.JMP_ABSOLUTE_X:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [
                            self._append_str("("),
                            self._read_abs_label_node(),
                            self._append_str(", x)"),
                        ],
                    )
                )
            elif mode == InstructionMode.IMMEDIATE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_short_hex_node("#")],
                    )
                )
            elif mode == InstructionMode.ZEROPAGEX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_short_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionMode.ABSOLUTEX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionMode.ABSOLUTE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_hex_node()],
                    )
                )
            elif mode == InstructionMode.ZEROPAGE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_short_hex_node()],
                    )
                )

        return nodes


class Parser65C02Bytes(Parser65C02):
    def __init__(self, nodes: List[Node] = None):
        Parser65C02.__init__(self, nodes)
        self.nodes.append(self._read_short_hex_node("!byte "))
