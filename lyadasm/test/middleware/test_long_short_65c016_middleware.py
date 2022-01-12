import unittest

from lyadasm.core.archs.arch65c816 import Parser65C816
from lyadasm.core.context import Context
from lyadasm.core.file import Binary
from lyadasm.core.middleware.swap_long_short_65c816 import (
    LongShort65C816Middleware,
)


class TestLongShort65C816Middleware(unittest.TestCase):
    def test_it_should_add_al_as(self) -> None:
        middleware = LongShort65C816Middleware(
            exceptions=[0x60E], alat=[0x609], asat=[0x60C]
        )
        parser = Parser65C816()
        ctx = Context(0x600, middlewares=[middleware])
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xC2,
                        0x20,
                        0xA9,
                        0xEA,
                        0xEA,
                        0xE2,
                        0x20,
                        0xA9,
                        0xEA,
                        0xA9,
                        0xEA,
                        0xEA,
                        0xA9,
                        0xEA,
                        0xC2,
                        0x20,
                    ]
                )
            ),
        )
        print(result)
        self.assertEqual(
            result,
            [
                "!al",
                "    rep #$20",
                "    lda #$eaea",
                "!as",
                "    sep #$20",
                "    lda #$ea",
                "!al",
                "    lda #$eaea",
                "!as",
                "    lda #$ea",
                "    rep #$20",
            ],
        )
