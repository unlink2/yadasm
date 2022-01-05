import unittest

from lyadasm.core.comparator import always_true
from lyadasm.core.context import Context
from lyadasm.core.file import Binary
from lyadasm.core.node import Node
from lyadasm.core.reader import read_i8_le, read_i16_le


class TestNode(unittest.TestCase):
    def test_it_should_parse_1_byte_instruction(self) -> None:
        node = Node(
            read_i8_le, [], lambda ctx, i: i == 0xEA, lambda ctx, i: "nop"
        )

        result = node.parse(Context(), Binary(bytes([0xEA])))

        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result.text, "nop")
            self.assertEqual(result.size, 1)

    def test_it_should_not_parse_1_byte_instruction(self) -> None:
        node = Node(
            read_i8_le, [], lambda ctx, i: i == 0xEA, lambda ctx, i: "nop"
        )

        result = node.parse(Context(), Binary(bytes([0xEB])))

        self.assertEqual(result, None)

    def test_it_should_parse_2_byte_instruction(self) -> None:
        node = Node(
            read_i8_le,
            [],
            lambda ctx, i: i == 0x09,
            lambda ctx, i: "ora ",
            [
                Node(
                    read_i8_le,
                    [],
                    always_true,
                    lambda ctx, i: f"#${hex(i)[2:]}",
                )
            ],
        )

        file = Binary(bytes([0x09, 0xFB]))
        result = node.parse(Context(), file)

        self.assertEqual(file.offset(), 2)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result.text, "ora #$fb")
            self.assertEqual(result.size, 2)

    def test_it_should_not_parse_2_byte_instruction(self) -> None:
        node = Node(
            read_i8_le,
            [],
            lambda ctx, i: i == 0x09,
            lambda ctx, i: "ora ",
            [
                Node(
                    read_i8_le,
                    [],
                    always_true,
                    lambda ctx, i: f"#${hex(i)[2:]}",
                )
            ],
        )

        file = Binary(bytes([0x09]))
        result = node.parse(Context(), file)

        self.assertEqual(file.offset(), 0)
        self.assertEqual(result, None)

    def test_it_should_parse_3_byte_instruction(self) -> None:
        node = Node(
            read_i8_le,
            [],
            lambda ctx, i: i == 0x19,
            lambda ctx, i: "ora ",
            [
                Node(
                    read_i16_le,
                    [],
                    always_true,
                    lambda ctx, i: f"#${hex(i)[2:]}",
                )
            ],
        )

        file = Binary(bytes([0x19, 0xFB, 0x00]))
        result = node.parse(Context(), file)

        self.assertEqual(file.offset(), 3)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result.text, "ora #$fb")
            self.assertEqual(result.size, 3)

    def test_it_should_apply_modifiers(self) -> None:
        node = Node(
            read_i8_le,
            [lambda ctx, i: i + 1, lambda ctx, i: i + 1],
            lambda ctx, i: i == 0xEA,
            lambda ctx, i: "nop",
        )

        result = node.parse(Context(), Binary(bytes([0xE8])))

        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result.text, "nop")
            self.assertEqual(result.size, 1)
