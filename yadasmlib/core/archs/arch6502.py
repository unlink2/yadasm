import ctypes
from enum import Enum
from typing import Any, Callable, Dict, List

from ..comparator import always_true
from ..context import Context, Symbol
from ..numfmt import Endianess
from ..file import Binary
from ..node import Node
from ..numfmt import IntFmt
from ..parser import Parser
from ..reader import read_i8_le, read_i16_le, read_none


def rel_i8_to_addr(ctx: Context, i: Any) -> int:
    """converts a python int to a relative 8 bit value"""
    return ctx.address + int(ctypes.c_int8(i).value) + 2


def fmt_hex_label(i: int) -> str:
    return f"{i:0{0}{IntFmt.HEX.to_literal()}}"


def grab_label(ctx: Context, i: Any) -> Any:
    """Grab a label and add it as a symbol"""
    if ctx.is_in_address_range(i):
        ctx.add_symbol(Symbol(i, f"label_{fmt_hex_label(i)}"))
    return i


def grab_label_i8_rel(ctx: Context, i: Any) -> Any:
    addr = rel_i8_to_addr(ctx, i)
    return grab_label(ctx, addr)


class InstructionModeException(Exception):
    pass


class InstructionMode(Enum):
    """All instruction modes for the 6502"""

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
    ABSOLUTE_JMP = 12
    INDIRECT_JMP = 13
    ZEROPAGEY = 14

    def mask(self, opcode: int) -> int:
        """returns the mask used to create the final instruction (bits bbbb)"""
        # the hex numbers here are instructions that are being makes
        # to make it easier to compare to hexdumps
        ccbits = self._extract_ccbits(opcode)
        # 11 does not exist on the 6502!
        if self == self.IMPLIED:
            # implied is an exception, just return the input
            return opcode
        elif ccbits == 0b10:
            return self.mask_cc10()
        elif ccbits == 0b00:
            return self.mask_cc00()
        else:
            return self.mask_cc01()

    def mask_cc00(self) -> int:
        if self == self.IMMEDIATE:
            return self._apply(0xE0)
        elif self == self.ZEROPAGE:
            return self._apply(0xE4)
        elif self == self.ZEROPAGEX:
            return self._apply(0xB4)
        elif self == self.ABSOLUTEX:
            return self._apply(0xBC)
        elif self in (self.ABSOLUTE, self.ABSOLUTE_JMP):
            return self._apply(0xEC)
        elif self == self.RELATIVE:
            return self._apply(0x00)
        elif self == self.INDIRECT_JMP:
            # jmp indirect breaks the rules!
            return 0x6C
        elif self == self.IMPLIED:
            return self._apply(0x00)
        else:
            raise InstructionModeException()

    def mask_cc10(self) -> int:
        if self == self.IMMEDIATE:
            return self._apply(0xA2)
        elif self == self.ZEROPAGE:
            return self._apply(0xA6)
        elif self == self.ZEROPAGEX:
            return self._apply(0x75)
        elif self == self.ZEROPAGEY:
            return self._apply(0xB6)
        elif self == self.ABSOLUTE:
            return self._apply(0xAE)
        elif self == self.ABSOLUTEX:
            return self._apply(0x7D)
        elif self == self.ABSOLUTEY:
            return self._apply(0xBE)
        elif self == self.ACCUMULATOR:
            return self._apply(0x0A)
        else:
            raise InstructionModeException()

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
        elif self == self.IMPLIED:
            return self._apply(0x00)
        elif self == self.RELATIVE:
            return self._apply(0x10)
        else:
            raise InstructionModeException()

    def _extract_ccbits(self, opcode: int) -> int:
        ccbits_mask = 0b00000011
        return opcode & ccbits_mask

    def _apply(self, unmasked: int) -> int:
        # addressing mode mask
        addr_mode_mask = 0b00011100
        return unmasked & addr_mode_mask


class Parser6502(Parser):
    def __init__(self, nodes: List[Node] = None) -> None:
        # the opcode that is passed in is usually the aaaa and cc bits
        # apart from a few exceptions that is ored with the apropriate bbb bits
        # to make a full opcode.
        # builds a list of all instructions
        # the make functions are common instruction patterns that can be
        # re-used
        # the final opcode is a result of the combination
        # of the address mode  mask, the cc bits and the actual opcode bits
        # the opcodes are being makes as needed
        # instruction encoding:
        # aaabbbcc;
        # aaa and cc bits determine opcode;
        # bbb bits determine addressing mode;
        # cc bits change addressing mode bits;
        Parser.__init__(self, nodes)
        self.nodes += (
            self._make_instruction("adc", self._make_load(self._opcode(0x69)))
            + self._make_instruction(
                "and", self._make_load(self._opcode(0x29))
            )
            + self._make_instruction(
                "asl", self._make_logic(self._opcode(0x1E))
            )
            + self._make_instruction("bit", self._make_bit(self._opcode(0x24)))
            + self._make_instruction("bpl", self._make_branch(0x10))
            + self._make_instruction("bmi", self._make_branch(0x30))
            + self._make_instruction("bvc", self._make_branch(0x50))
            + self._make_instruction("bvs", self._make_branch(0x70))
            + self._make_instruction("bcc", self._make_branch(0x90))
            + self._make_instruction("bcs", self._make_branch(0xB0))
            + self._make_instruction("bne", self._make_branch(0xD0))
            + self._make_instruction("beq", self._make_branch(0xF0))
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
            + self._make_instruction("clc", self._make_implied(0x18))
            + self._make_instruction("sec", self._make_implied(0x38))
            + self._make_instruction("cli", self._make_implied(0x58))
            + self._make_instruction("sei", self._make_implied(0x78))
            + self._make_instruction("clv", self._make_implied(0xB8))
            + self._make_instruction("cld", self._make_implied(0xD8))
            + self._make_instruction("sed", self._make_implied(0xF8))
            + self._make_instruction("inc", self._make_dec(self._opcode(0xE6)))
            + self._make_instruction(
                "jmp", self._make_jump(self._opcode(0x4C))
            )
            + self._make_instruction("jsr", self._make_jsr(self._opcode(0x20)))
            + self._make_instruction(
                "lda", self._make_load(self._opcode(0xA9))
            )
            + self._make_instruction("ldx", self._make_ldx(self._opcode(0xA2)))
            + self._make_instruction("ldy", self._make_ldy(self._opcode(0xA0)))
            + self._make_instruction(
                "lsr", self._make_logic(self._opcode(0x4A))
            )
            + self._make_instruction("nop", self._make_implied(0xEA))
            + self._make_instruction(
                "ora", self._make_load(self._opcode(0x09))
            )
            + self._make_instruction("tax", self._make_implied(0xAA))
            + self._make_instruction("txa", self._make_implied(0x8A))
            + self._make_instruction("dex", self._make_implied(0xCA))
            + self._make_instruction("inx", self._make_implied(0xE8))
            + self._make_instruction("tay", self._make_implied(0xA8))
            + self._make_instruction("tya", self._make_implied(0x98))
            + self._make_instruction("dey", self._make_implied(0x88))
            + self._make_instruction("iny", self._make_implied(0xC8))
            + self._make_instruction(
                "rol", self._make_logic(self._opcode(0x2A))
            )
            + self._make_instruction(
                "ror", self._make_logic(self._opcode(0x6A))
            )
            + self._make_instruction("rti", self._make_implied(0x40))
            + self._make_instruction("rts", self._make_implied(0x60))
            + self._make_instruction(
                "sbc", self._make_load(self._opcode(0xE9))
            )
            + self._make_instruction(
                "sta", self._make_store(self._opcode(0x85))
            )
            + self._make_instruction("txs", self._make_implied(0x9A))
            + self._make_instruction("tsx", self._make_implied(0xBA))
            + self._make_instruction("pha", self._make_implied(0x48))
            + self._make_instruction("pla", self._make_implied(0x68))
            + self._make_instruction("php", self._make_implied(0x08))
            + self._make_instruction("plp", self._make_implied(0x28))
            + self._make_instruction("stx", self._make_stx(self._opcode(0x86)))
            + self._make_instruction("sty", self._make_sty(self._opcode(0x84)))
        )

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

    def _read_short_hex_node(
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

    def _read_immediate_hex_node(
        self, prefix: str = "", padding: int = 2, mode: IntFmt = IntFmt.HEX
    ) -> Node:
        return self._read_short_hex_node(prefix, padding, mode)

    def _read_hex_node(
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

    def _read_rel_label_node(self) -> Node:
        return Node(
            read_i8_le,
            [grab_label_i8_rel],
            always_true,
            lambda ctx, i: f"label_{fmt_hex_label(i)}",
            [],
        )

    def _read_abs_label_node(self) -> Node:
        return Node(
            read_i16_le,
            [grab_label],
            always_true,
            lambda ctx, i: f"label_{fmt_hex_label(i)}",
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

    def _make_acc(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ACCUMULATOR: mask
            | InstructionMode.ACCUMULATOR.mask(mask)
        }

    def _make_logic(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEX: mask
            | InstructionMode.ZEROPAGEX.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
            InstructionMode.ABSOLUTEX: mask
            | InstructionMode.ABSOLUTEX.mask(mask),
        } | self._make_acc(mask)

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
        """implied should just receive the whole opcode"""
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

    def _make_jump(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ABSOLUTE_JMP: mask
            | InstructionMode.ABSOLUTE_JMP.mask(mask),
            InstructionMode.INDIRECT_JMP: InstructionMode.INDIRECT_JMP.mask(
                mask
            ),
        }

    def _make_jsr(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            # jsr breaks the rules
            InstructionMode.ABSOLUTE_JMP: mask,
        }

    def _make_ldx(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.IMMEDIATE: mask
            | InstructionMode.IMMEDIATE.mask(mask),
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEY: mask
            | InstructionMode.ZEROPAGEY.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
            InstructionMode.ABSOLUTEY: mask
            | InstructionMode.ABSOLUTEY.mask(mask),
        }

    def _make_ldy(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.IMMEDIATE: mask
            | InstructionMode.IMMEDIATE.mask(mask),
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEX: mask
            | InstructionMode.ZEROPAGEX.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
            InstructionMode.ABSOLUTEX: mask
            | InstructionMode.ABSOLUTEX.mask(mask),
        }

    def _make_stx(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEY: mask
            | InstructionMode.ZEROPAGEY.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
        }

    def _make_sty(self, mask: int) -> Dict[InstructionMode, int]:
        return {
            InstructionMode.ZEROPAGE: mask
            | InstructionMode.ZEROPAGE.mask(mask),
            InstructionMode.ZEROPAGEX: mask
            | InstructionMode.ZEROPAGEX.mask(mask),
            InstructionMode.ABSOLUTE: mask
            | InstructionMode.ABSOLUTE.mask(mask),
        }

    def _read_opcode(self, _ctx: Context, file: Binary) -> int:
        byte = file.read(1)
        if byte is None:
            return -1
        opcode = int.from_bytes(byte, Endianess.LITTLE.to_literal())

        return opcode

    # helper to create instruction nodes for the most common
    # instruction modes
    def _make_instruction(
        self, name: str, modes: Dict[InstructionMode, int]
    ) -> List[Node]:
        nodes: List[Node] = []

        for mode, opcode in modes.items():
            self._make_instruction_immediate(name, mode, opcode, nodes)
            self._make_instruction_absolute(name, mode, opcode, nodes)
            self._make_instruction_indirect(name, mode, opcode, nodes)

        return nodes

    def _make_instruction_immediate(
        self, name: str, mode: InstructionMode, opcode: int, nodes: List[Node]
    ) -> List[Node]:
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
                    [self._read_rel_label_node()],
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
                    [self._read_immediate_hex_node("#")],
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
        elif mode == InstructionMode.ZEROPAGEY:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    self._make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [self._read_short_hex_node(), self._append_str(", y")],
                )
            )
        return nodes

    def _make_instruction_absolute(
        self, name: str, mode: InstructionMode, opcode: int, nodes: List[Node]
    ) -> List[Node]:
        if mode == InstructionMode.ABSOLUTE:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    self._make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [self._read_hex_node()],
                )
            )
        elif mode == InstructionMode.ABSOLUTE_JMP:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    self._make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [self._read_abs_label_node()],
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
        elif mode == InstructionMode.ABSOLUTEY:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    self._make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [self._read_hex_node(), self._append_str(", y")],
                )
            )
        return nodes

    def _make_instruction_indirect(
        self, name: str, mode: InstructionMode, opcode: int, nodes: List[Node]
    ) -> List[Node]:
        if mode == InstructionMode.INDIRECTX:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    self._make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [
                        self._append_str("("),
                        self._read_short_hex_node(),
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
                        self._read_short_hex_node(),
                        self._append_str("), y"),
                    ],
                )
            )
        elif mode == InstructionMode.INDIRECT_JMP:
            nodes.append(
                Node(
                    read_i8_le,
                    [],
                    self._make_comparator(opcode),
                    lambda ctx, i: f"{name} ",
                    [
                        self._append_str("("),
                        self._read_abs_label_node(),
                        self._append_str(")"),
                    ],
                )
            )

        return nodes


class Parser6502Bytes(Parser6502):
    """A 6502 parser that defaults to parsing unknown opcodes as bytes"""

    def __init__(self, nodes: List[Node] = None):
        Parser6502.__init__(self, nodes)

        self.nodes.append(self._read_short_hex_node("!byte "))
