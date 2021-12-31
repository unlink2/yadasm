import unittest

from core.archs.arch6502 import Parser6502
from core.context import Context
from core.file import Binary


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
