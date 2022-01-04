import unittest

from yadasmlib.core.archs.arch6502 import Parser6502, Parser6502Bytes
from yadasmlib.core.context import Context
from yadasmlib.core.file import Binary


class TestArch6502(unittest.TestCase):
    def test_it_should_parse_regular_arch6502_modes(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0xAB,
                        0xA5,
                        0xAB,
                        0xB5,
                        0xAB,
                        0xAD,
                        0x00,
                        0x02,
                        0xBD,
                        0x00,
                        0x02,
                        0xB9,
                        0x00,
                        0x02,
                        0xA1,
                        0xAB,
                        0xB1,
                        0xAB,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 0x613)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    lda #$ab",
                    "    lda $ab",
                    "    lda $ab, x",
                    "    lda $0200",
                    "    lda $0200, x",
                    "    lda $0200, y",
                    "    lda ($ab, x)",
                    "    lda ($ab), y",
                ],
            )

    def test_it_should_parse_logical_arch6502_modes(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0x0A,
                        0x06,
                        0x44,
                        0x16,
                        0x44,
                        0x0E,
                        0x00,
                        0x44,
                        0x1E,
                        0x00,
                        0x44,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 0x60B)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    asl A",
                    "    asl $44",
                    "    asl $44, x",
                    "    asl $4400",
                    "    asl $4400, x",
                ],
            )

    def test_it_should_parse_bit(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x24, 0x44, 0x2C, 0x00, 0x44])),
        )

        self.assertEqual(ctx.address, 1541)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    bit $44",
                    "    bit $4400",
                ],
            )

    def test_it_should_parse_branch(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0x00,
                        0x00,
                        0x00,
                        0x10,
                        0xFB,
                        0x10,
                        0xFA,
                        0x10,
                        0x03,
                        0x00,
                        0x00,
                        0x00,
                        0x10,
                        0xE0,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 1550)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "label_600",
                    "    brk",
                    "label_601",
                    "    brk",
                    "    brk",
                    "    bpl label_600",
                    "    bpl label_601",
                    "    bpl label_60c",
                    "    brk",
                    "    brk",
                    "    brk",
                    "label_60c",
                    "    bpl label_5ee",
                ],
            )

    def test_it_should_parse_compare_index(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xE0, 0x44, 0xE4, 0x44, 0xEC, 0x00, 0x44])),
        )

        self.assertEqual(ctx.address, 1543)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result, ["    cpx #$44", "    cpx $44", "    cpx $4400"]
            )

    def test_it_should_parse_dec(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xC6,
                        0x44,
                        0xD6,
                        0x44,
                        0xCE,
                        0x00,
                        0x44,
                        0xDE,
                        0x00,
                        0x44,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 1546)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    dec $44",
                    "    dec $44, x",
                    "    dec $4400",
                    "    dec $4400, x",
                ],
            )

    def test_it_should_parse_jmp(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x4C, 0x20, 0x54, 0x6C, 0x20, 0x54])),
        )

        self.assertEqual(ctx.address, 1542)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    jmp label_5420",
                    "    jmp (label_5420)",
                    "label_5420",
                ],
            )

    def test_it_should_parse_jsr(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x20, 0x20, 0x54])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    jsr label_5420",
                    "label_5420",
                ],
            )

    def test_it_should_parse_ldx(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA2,
                        0x44,
                        0xA6,
                        0x44,
                        0xB6,
                        0x44,
                        0xAE,
                        0x55,
                        0x44,
                        0xBE,
                        0x55,
                        0x44,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 1548)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    ldx #$44",
                    "    ldx $44",
                    "    ldx $44, y",
                    "    ldx $4455",
                    "    ldx $4455, y",
                ],
            )

    def test_it_should_parse_ldy(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA0,
                        0x44,
                        0xA4,
                        0x44,
                        0xB4,
                        0x44,
                        0xAC,
                        0x55,
                        0x44,
                        0xBC,
                        0x55,
                        0x44,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 1548)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    ldy #$44",
                    "    ldy $44",
                    "    ldy $44, x",
                    "    ldy $4455",
                    "    ldy $4455, x",
                ],
            )

    def test_it_should_parse_stx(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x86, 0x44, 0x96, 0x44, 0x8E, 0x55, 0x44])),
        )

        self.assertEqual(ctx.address, 1543)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result, ["    stx $44", "    stx $44, y", "    stx $4455"]
            )

    def test_it_should_parse_sty(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x84, 0x44, 0x94, 0x44, 0x8C, 0x55, 0x44])),
        )

        self.assertEqual(ctx.address, 1543)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result, ["    sty $44", "    sty $44, x", "    sty $4455"]
            )

    def test_it_should_parse_default(self) -> None:
        parser = Parser6502Bytes()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xFF])),
        )

        self.assertEqual(ctx.address, 1537)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(result, ["    !byte $ff"])
