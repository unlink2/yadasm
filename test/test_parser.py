import unittest

from core.archs.arch6502 import Parser6502
from core.context import Context
from core.file import Binary
from core.parser import ParsersExhaustedException


class TestParser(unittest.TestCase):
    def test_it_should_parse_valid_code(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0x01,
                        0x8D,
                        0x00,
                        0x02,
                        0xA9,
                        0x05,
                        0x8D,
                        0x01,
                        0x02,
                        0xA9,
                        0x08,
                        0x8D,
                        0x02,
                        0x02,
                    ]
                )
            ),
        )

        # data was 15 bytes long!
        self.assertEqual(ctx.address, 0x60F)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    lda #$01",
                    "    sta $0200",
                    "    lda #$05",
                    "    sta $0201",
                    "    lda #$08",
                    "    sta $0202",
                ],
            )

    def test_it_should_fail_when_all_nodes_are_exhausted(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        self.assertRaises(
            ParsersExhaustedException,
            lambda: parser.parse(
                ctx,
                Binary(
                    bytes(
                        [
                            0xA9,
                            0x01,
                            0x8D,
                            0x00,
                            0x02,
                            0xA9,
                            0x05,
                            0x8D,
                            0x01,
                            0x02,
                            0xA9,
                            0x08,
                            0xFF,
                            0x02,
                            0x02,
                        ]
                    )
                ),
            ),
        )

        # data was 15 bytes long, but the error was raised after 12!
        self.assertEqual(ctx.address, 0x60C)

    def test_it_should_parse_rgular_arch6502_modes(self) -> None:
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
        print(result)
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
