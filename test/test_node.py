import unittest

from core.comparator import always_true
from core.context import Context
from core.file import Binary
from core.node import Node
from core.reader import read_i8_le


class TestComparator(unittest.TestCase):
    def test_it_should_parse_1_byte_opcode(self) -> None:
        node = Node(read_i8_le, [], lambda i: i == 0xEA, lambda i, ctx: "nop")

        result = node.parse(Context(), Binary(bytes([0xEA])))

        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result[0], "nop")
            self.assertEqual(result[1], 1)

    def test_it_should_not_parse_1_byte_opcode(self) -> None:
        node = Node(read_i8_le, [], lambda i: i == 0xEA, lambda i, ctx: "nop")

        result = node.parse(Context(), Binary(bytes([0xEB])))

        self.assertEqual(result, None)

    def test_it_should_parse_2_byte_opcode(self) -> None:
        node = Node(
            read_i8_le,
            [],
            lambda i: i == 0x09,
            lambda i, ctx: "ora ",
            [Node(read_i8_le, [], always_true, lambda i, ctx: f"#${hex(i)[2:]}")],
        )

        file = Binary(bytes([0x09, 0xFB]))
        result = node.parse(Context(), file)

        self.assertEqual(file.offset(), 2)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result[0], "ora #$fb")
            self.assertEqual(result[1], 2)

    def test_it_should_not_parse_2_byte_opcode(self) -> None:
        node = Node(
            read_i8_le,
            [],
            lambda i: i == 0x09,
            lambda i, ctx: "ora ",
            [Node(read_i8_le, [], always_true, lambda i, ctx: f"#${hex(i)[2:]}")],
        )

        file = Binary(bytes([0x09]))
        result = node.parse(Context(), file)

        self.assertEqual(file.offset(), 0)
        self.assertEqual(result, None)
