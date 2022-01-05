import unittest
from lyadasm.core.archs.arch65c02 import Parser65C02
from lyadasm.core.context import Context
from lyadasm.core.file import Binary


class TestArch65C02(unittest.TestCase):
    def test_it_should_parse_zp_indirect(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x12, 0xAA])),
        )

        self.assertEqual(ctx.address, 1538)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    ora ($aa)"])

    def test_it_should_parse_jmp_abs_x(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x7C, 0xAA, 0xBB])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    jmp (label_bbaa, x)", "label_bbaa"])

    def test_it_should_parse_bit_ext(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x89, 0xAA, 0x34, 0xBB, 0x3C, 0xDD, 0xCC])),
        )

        self.assertEqual(ctx.address, 1543)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result, ["    bit #$aa", "    bit $bb, x", "    bit $ccdd, x"]
            )

    def test_it_should_parse_tsb(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x04, 0xAA, 0x0C, 0xAA, 0xBB])),
        )

        self.assertEqual(ctx.address, 1541)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    tsb $aa", "    tsb $bbaa"])

    def test_it_should_parse_trb(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x14, 0xAA, 0x1C, 0xAA, 0xBB])),
        )

        self.assertEqual(ctx.address, 1541)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    trb $aa", "    trb $bbaa"])

    def test_it_should_parse_stz(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x74, 0xAA, 0x9E, 0xAA, 0xBB])),
        )

        self.assertEqual(ctx.address, 1541)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    stz $aa, x", "    stz $bbaa, x"])

    def test_it_should_parse_bra(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x80, 0xAA])),
        )

        self.assertEqual(ctx.address, 1538)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    bra label_5ac"])

    def test_it_should_parse_single_byte(self) -> None:
        parser = Parser65C02()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA])),
        )

        self.assertEqual(ctx.address, 1542)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    inc A",
                    "    dec A",
                    "    phy",
                    "    ply",
                    "    phx",
                    "    plx",
                ],
            )
