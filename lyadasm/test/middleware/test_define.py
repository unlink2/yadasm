import unittest
from typing import Any

from lyadasm.core.archs.arch6502 import Parser6502
from lyadasm.core.context import Context, Symbol
from lyadasm.core.file import Binary
from lyadasm.core.middleware.define import DefineMiddleware, Definition


def ef_modifier(ctx: Context, _i: Any, default: Any) -> Any:
    if ctx.address == 0x606:
        return 0xAA
    else:
        return default


class TestDefine(unittest.TestCase):
    def test_it_should_replace_defined_data(self) -> None:
        middleware = DefineMiddleware(
            {
                0xEA: Definition(lambda ctx, i: "#test_label"),
                0xEE: Definition(0xED),
                "#$1e": Definition(0xEF, modifier=ef_modifier),
            },
            [Symbol(0x600, "test:"), Symbol(0x400, "shadow:")],
        )
        parser = Parser6502()
        ctx = Context(0x600, middlewares=[middleware])
        result = parser.parse(
            ctx,
            Binary(bytes([0xA9, 0xEA, 0xA9, 0xEE, 0xA2, 0x1E, 0xA2, 0x1E])),
        )

        self.assertEqual(
            result,
            [
                "test:",
                "    lda #test_label",
                "    lda #$ed",
                "    ldx #$ef",
                "    ldx #$aa",
            ],
        )

    def test_it_should_output_all_symbols(self) -> None:
        middleware = DefineMiddleware(
            {
                0xEA: Definition(lambda ctx, i: "#test_label"),
                0xEE: Definition(0xED),
                "#$1e": Definition(0xEF, modifier=ef_modifier),
            },
            [Symbol(0x600, "test:"), Symbol(0x400, "shadow:")],
            force_symbol_output=True,
        )
        parser = Parser6502()
        ctx = Context(0x600, middlewares=[middleware])
        result = parser.parse(
            ctx,
            Binary(bytes([0xA9, 0xEA, 0xA9, 0xEE, 0xA2, 0x1E, 0xA2, 0x1E])),
        )

        self.assertEqual(
            result,
            [
                "shadow:",
                "test:",
                "    lda #test_label",
                "    lda #$ed",
                "    ldx #$ef",
                "    ldx #$aa",
            ],
        )
