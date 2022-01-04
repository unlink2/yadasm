import unittest
from core.context import Context
from core.file import Binary
from core.archs.arch65c816 import Parser65C816


class TestArch65C816(unittest.TestCase):
    def test_it_should_use_long_branch_and_immediate(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xA9, 0x12, 0x34, 0xF0, 0x6A, 0xFF])),
        )

        self.assertEqual(ctx.address, 1542)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(result, ["    lda #$3412", "    beq label_56f"])
