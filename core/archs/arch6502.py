import ctypes
from enum import Enum
from typing import Any, Callable, Dict, List

from ..comparator import always_true
from ..context import Context
from ..node import Node
from ..numfmt import IntFmt
from ..operation import grab_label
from ..parser import Parser
from ..reader import read_i8_le, read_i16_le, read_none


def rel_i8_to_addr(ctx: Context, i: Any) -> int:
    """converts a python int to a relative 8 bit value"""
    return ctx.address + int(ctypes.c_int8(i).value) + 2


def grab_label_i8_rel(ctx: Context, i: Any) -> Any:
    addr = rel_i8_to_addr(ctx, i)
    return grab_label(ctx, addr)


class InstructionMode(Enum):
    IMMEDIATE = 1
    ZEROPAGE = 2
    ZEROPAGEX = 3
    ABSOLUTE = 4
    ABSOLUTEX = 5
    ABSOLUTEY = 6
    INDIRECTX = 7
    INDIRECTY = 8
    ACCUMULATOR = 9
    IMPLIED = 10
    RELATIVE = 11

    def mask(self, opcode: int) -> int:
        """returns the mask used to create the final instruction"""
        ccbits = self._extract_ccbits(opcode)
        # 11 does not exist on the 6502!
        if ccbits == 0x10:
            return self.mask_cc10()
        elif ccbits == 0x00:
            return self.mask_cc00()
        else:
            return self.mask_cc01()

    def mask_cc00(self) -> int:
        if self == self.IMMEDIATE:
            return self._apply(0xE0)
        elif self == self.ZEROPAGE:
            return self._apply(0xE4)
        elif self == self.ABSOLUTE:
            return self._apply(0xEC)
        elif self == self.RELATIVE:
            return self._apply(0x10)
        else:
            return 0

    def mask_cc10(self) -> int:
        if self == self.IMMEDIATE:
            return self._apply(0xA2)
        elif self == self.ZEROPAGE:
            return self._apply(0xA6)
        elif self == self.ABSOLUTE:
            return self._apply(0xAE)
        elif self == self.ABSOLUTEY:
            return self._apply(0xBE)
        else:
            return 0

    def mask_cc01(self) -> int:
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
        elif self == self.IMPLIED:
            return self._apply(0x00)
        elif self == self.RELATIVE:
            return self._apply(0x00)
        else:
            return 0x00

    def _extract_ccbits(self, opcode: int) -> int:
        ccbits_mask = 0b00000011
        return opcode & ccbits_mask

    def _apply(self, unmasked: int) -> int:
        # addressing mode mask
        addr_mode_mask = 0b00011100
        return unmasked & addr_mode_mask


class Parser6502(Parser):
    def __init__(self) -> None:
        # builds a list of all instructions
        # the make functions are common instruction patterns that can be
        # re-used
        # the final opcode is a result of the combination
        # of the address mode  mask, the cc bits and the actual opcode bits
        # instruction encoding:
        # aaabbbcc;
        # aaa and cc bits determine opcode;
        # bbb bits determine addressing mode;
        # cc bits change addressing mode bits;
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
            + self._make_instruction("bit", self._make_bit(self._opcode(0x24)))
            + self._make_instruction(
                "bpl", self._make_branch(self._opcode(0x10))
            )
            + self._make_instruction(
                "bmi", self._make_branch(self._opcode(0x30))
            )
            + self._make_instruction(
                "bvc", self._make_branch(self._opcode(0x50))
            )
            + self._make_instruction(
                "bvs", self._make_branch(self._opcode(0x70))
            )
            + self._make_instruction(
                "bcc", self._make_branch(self._opcode(0x90))
            )
            + self._make_instruction(
                "bcs", self._make_branch(self._opcode(0xB0))
            )
            + self._make_instruction(
                "bne", self._make_branch(self._opcode(0xD0))
            )
            + self._make_instruction(
                "beq", self._make_branch(self._opcode(0xF0))
            )
            + self._make_instruction(
                "brk", self._make_implied(self._opcode(0x00))
            )
            + self._make_instruction(
                "cmp", self._make_load(self._opcode(0xC9))
            )
            + self._make_instruction(
                "cpx", self._make_compare_index(self._opcode(0xE0))
            )
            + self._make_instruction(
                "cpy", self._make_compare_index(self._opcode(0xC0))
            )
            + self._make_instruction("dec", self._make_dec(self._opcode(0xC6)))
            + self._make_instruction(
                "eor", self._make_load(self._opcode(0x49))
            )
            + self._make_instruction(
                "clc", self._make_implied(self._opcode(0x18))
            )
            + self._make_instruction(
                "sec", self._make_implied(self._opcode(0x38))
            )
            + self._make_instruction(
                "cli", self._make_implied(self._opcode(0x58))
            )
            + self._make_instruction(
                "sei", self._make_implied(self._opcode(0x78))
            )
            + self._make_instruction(
                "clv", self._make_implied(self._opcode(0xB8))
            )
            + self._make_instruction(
                "cld", self._make_implied(self._opcode(0xD8))
            )
            + self._make_instruction(
                "sed", self._make_implied(self._opcode(0xF8))
            )
            + self._make_instruction("inc", self._make_dec(self._opcode(0xE6)))
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

    def _read_i8_rel_label_node(self) -> Node:
        return Node(
            read_i8_le,
            [grab_label_i8_rel],
            always_true,
            lambda ctx, i: f"label_{hex(i)[2:]}",
            [],
        )

    def _append_str(self, value: str = "") -> Node:
        return Node(read_none, [], always_true, lambda ctx, i: value)

    def _make_comparator(self, opcode: int) -> Callable[[Context, int], bool]:
        return lambda ctx, i: i == opcode

    def _make_load(self, mask: int) -> Dict[InstructionMode, int]:
        """
        creates a load instruction based on common instruction modes
        """
        return {
            InstructionMode.IMMEDIATE: mask
            | InstructionMode.IMMEDIATE.mask(mask),
        } | self._make_store(mask)

    def _make_store(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEX: mask
            | InstructionMode.ZEROPAGEX.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
            InstructionMode.ABSOLUTEX: mask
            | InstructionMode.ABSOLUTEX.mask(mask),
            InstructionMode.ABSOLUTEY: mask
            | InstructionMode.ABSOLUTEY.mask(mask),
            InstructionMode.INDIRECTX: mask
            | InstructionMode.INDIRECTX.mask(mask),
            InstructionMode.INDIRECTY: mask
            | InstructionMode.INDIRECTY.mask(mask),
        }

    def _make_logic(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ACCUMULATOR: mask
            | InstructionMode.ACCUMULATOR.mask(mask),
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEX: mask
            | InstructionMode.ZEROPAGEX.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
            InstructionMode.ABSOLUTEX: mask
            | InstructionMode.ABSOLUTEX.mask(mask),
        }

    def _make_bit(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
        }

    def _make_branch(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.RELATIVE: mask
            | InstructionMode.RELATIVE.mask(mask)
        }

    def _make_implied(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.IMPLIED: mask | InstructionMode.IMPLIED.mask(mask)
        }

    def _make_compare_index(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.IMMEDIATE: mask
            | InstructionMode.IMMEDIATE.mask(mask),
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
        }

    def _make_dec(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEX: mask
            | InstructionMode.ZEROPAGEX.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
            InstructionMode.ABSOLUTEX: mask
            | InstructionMode.ABSOLUTEX.mask(mask),
        }

    # helper to create instruction nodes for the most common
    # instruction modes
    def _make_instruction(
        self, name: str, modes: Dict[InstructionMode, int]
    ) -> List[Node]:
        nodes: List[Node] = []

        for mode, opcode in modes.items():
            if mode == InstructionMode.IMPLIED:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name}",
                        [],
                    )
                )
            elif mode == InstructionMode.RELATIVE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_rel_label_node()],
                    )
                )
            elif mode == InstructionMode.ACCUMULATOR:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} A",
                        [],
                    )
                )
            elif mode == InstructionMode.IMMEDIATE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node("#")],
                    )
                )
            elif mode == InstructionMode.ZEROPAGE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node()],
                    )
                )
            elif mode == InstructionMode.ZEROPAGEX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i8_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionMode.ABSOLUTE:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i16_hex_node()],
                    )
                )
            elif mode == InstructionMode.ABSOLUTEX:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i16_hex_node(), self._append_str(", x")],
                    )
                )
            elif mode == InstructionMode.ABSOLUTEY:
                nodes.append(
                    Node(
                        read_i8_le,
                        [],
                        self._make_comparator(opcode),
                        lambda ctx, i: f"{name} ",
                        [self._read_i16_hex_node(), self._append_str(", y")],
                    )
                )
            elif mode == InstructionMode.INDIRECTX:
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
            elif mode == InstructionMode.INDIRECTY:
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
