import unittest

from core.arch.arch6502 import arch6502
from core.context import Context
from core.file import Binary
from core.parser import Parser


class TestParser(unittest.TestCase):
    def test_it_should_parse_valid_code(self) -> None:
        parser = Parser(arch6502)
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
