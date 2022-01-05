import unittest
from yadasmlib.core.context import Context
from yadasmlib.core.file import Binary
from yadasmlib.core.archs.arch65c816 import Parser65C816, Parser65C816Emulated


class TestArch65C816(unittest.TestCase):
    def test_it_should_use_long_branch_and_immediate(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xA9, 0x12, 0x34, 0x82, 0x6A, 0xFF])),
        )

        self.assertEqual(ctx.address, 1542)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(result, ["    lda #$3412", "    brl label_56f"])

    def test_it_should_use_ext(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0x03,
                        0x11,
                        0x07,
                        0x22,
                        0x0F,
                        0x33,
                        0x44,
                        0x55,
                        0x13,
                        0x66,
                        0x17,
                        0x77,
                        0x1F,
                        0xAA,
                        0xBB,
                        0xCC,
                    ]
                )
            ),
        )

        self.assertEqual(ctx.address, 1552)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    ora $11, s",
                    "    ora [$22]",
                    "    ora $554433",
                    "    ora ($66, s), y",
                    "    ora [$77], y",
                    "    ora $ccbbaa, x",
                ],
            )

    def test_it_should_use_xce_in_native(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xFB])),
        )

        self.assertEqual(ctx.address, 1537)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    xce",
                ],
            )

    def test_it_should_use_xce_in_emu(self) -> None:
        parser = Parser65C816Emulated()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xFB])),
        )

        self.assertEqual(ctx.address, 1537)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    xce",
                ],
            )

    def test_it_should_use_cop_in_emu(self) -> None:
        parser = Parser65C816Emulated()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x02, 0x12])),
        )

        self.assertEqual(ctx.address, 1538)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    cop #$12",
                ],
            )

    def test_it_should_use_cop_in_native(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x02, 0x12])),
        )

        self.assertEqual(ctx.address, 1538)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    cop #$12",
                ],
            )

    def test_it_should_jmp_long(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x22, 0x12, 0x23, 0x45])),
        )

        self.assertEqual(ctx.address, 1540)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    jsl label_452312",
                    "label_452312",
                ],
            )

    def test_it_should_per_label(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x62, 0x12, 0x23])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    per label_2312",
                    "label_2312",
                ],
            )

    def test_it_should_per_immediate(self) -> None:
        parser = Parser65C816(per_label=False)
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x62, 0x12, 0x23])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    per #$2312",
                ],
            )

    def test_it_should_have_rep_sep(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xC2, 0x12, 0xE2, 0x13])),
        )

        self.assertEqual(ctx.address, 1540)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    rep #$12",
                    "    sep #$13",
                ],
            )

    def test_it_should_have_move(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0x44, 0x12, 0xE2])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    mvp #$12, #$e2",
                ],
            )

    def test_it_should_have_pei(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xD4, 0x12])),
        )

        self.assertEqual(ctx.address, 1538)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    pei $12",
                ],
            )

    def test_it_should_have_pea(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xF4, 0x12, 0x11])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    pea #$1112",
                ],
            )

    def test_it_should_have_jml(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xDC, 0x12, 0x11])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    jmp [label_1112]",
                    "label_1112",
                ],
            )

    def test_it_should_have_jsrx(self) -> None:
        parser = Parser65C816()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(bytes([0xFC, 0x12, 0x11])),
        )

        self.assertEqual(ctx.address, 1539)
        self.assertNotEqual(result, None)
        print(result)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    jsr (label_1112, x)",
                    "label_1112",
                ],
            )
