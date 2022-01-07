import unittest

from lyadasm.core.archs.arch6502 import Parser6502
from lyadasm.core.context import Context
from lyadasm.core.file import Binary
from lyadasm.core.middleware.define import DefineMiddleware


class TestDefine(unittest.TestCase):
    def test_it_should_replace_defined_data(self) -> None:
        middleware = DefineMiddleware(
            {0xEA: lambda ctx, i: "#test_label", 0xEE: 0xED, "#$1e": 0xEF}
        )
        parser = Parser6502()
        ctx = Context(0x600, middlewares=[middleware])
        result = parser.parse(
            ctx,
            Binary(bytes([0xA9, 0xEA, 0xA9, 0xEE, 0xA2, 0x1E])),
        )

        self.assertEqual(
            result, ["    lda #test_label", "    lda #$ed", "    ldx #$ef"]
        )
