import unittest
from typing import List

from core.context import Context, Line, Middleware, Symbol


class TestMiddleware(Middleware):
    def on_collect_begin(self, ctx: "Context", lines: List[str]) -> None:
        lines.append("middleware_begin")

    def on_collect_end(self, ctx: "Context", lines: List[str]) -> None:
        lines.append("middleware_end")

    def on_symbol(self, ctx: "Context", symbol: Symbol) -> None:
        symbol.name += "_middleware_symbol"

    def on_line(self, ctx: "Context", line: Line) -> None:
        line.text += "_middleware_line"


class TestContext(unittest.TestCase):
    def test_it_should_add_symbol(self) -> None:
        ctx = Context()
        ctx.add_symbol(Symbol(100, "lab_100"))
        ctx.advance(2)
        ctx.add_symbol(Symbol(102, "lab_102_1"))
        ctx.add_symbol(Symbol(102, "lab_102_2"))

        self.assertEqual(len(ctx.symbols.keys()), 2)
        self.assertNotEqual(ctx.symbols[100], None)
        self.assertNotEqual(ctx.symbols[102], None)

        self.assertEqual(len(ctx.symbols[100]), 1)
        self.assertEqual(len(ctx.symbols[102]), 2)

        self.assertEqual(ctx.symbols[100], [Symbol(100, "lab_100")])

        self.assertEqual(
            ctx.symbols[102],
            [Symbol(102, "lab_102_1"), Symbol(102, "lab_102_2")],
        )

    def test_it_should_add_lines(self) -> None:
        ctx = Context()
        ctx.add_line(Line("line1"))
        ctx.advance(2)
        ctx.add_line(Line("line2.0"))
        ctx.add_line(Line("line2.1"))

        self.assertEqual(len(ctx.lines.keys()), 2)
        self.assertNotEqual(ctx.lines[0], None)
        self.assertNotEqual(ctx.lines[2], None)

        self.assertEqual(len(ctx.lines[0]), 1)
        self.assertEqual(len(ctx.lines[2]), 2)

        self.assertEqual(ctx.lines[0], [Line("line1")])
        self.assertEqual(ctx.lines[2], [Line("line2.0"), Line("line2.1")])

    def test_it_should_collect_lines(self) -> None:
        ctx = Context()
        ctx.add_symbol(Symbol(0, "begin:"))
        ctx.add_line(Line("line1"))
        ctx.advance(2)
        ctx.add_symbol(Symbol(1, "middle:"))
        ctx.add_line(Line("line2.0"))
        ctx.add_line(Line("line2.1"))
        ctx.add_symbol(Symbol(3, "end:"))
        ctx.advance(2)
        ctx.add_line(Line("line3"))

        self.assertEqual(
            ctx.collect(),
            [
                "begin:",
                "    line1",
                "middle:",
                "    line2.0",
                "    line2.1",
                "end:",
                "    line3",
            ],
        )

    def test_is_int_address_range(self) -> None:
        ctx = Context(address=100, end_address=150)

        self.assertTrue(ctx.is_in_address_range(150))
        self.assertTrue(ctx.is_in_address_range(100))
        self.assertTrue(ctx.is_in_address_range(110))
        self.assertFalse(ctx.is_in_address_range(99))
        self.assertFalse(ctx.is_in_address_range(2))
        self.assertFalse(ctx.is_in_address_range(151))
        self.assertFalse(ctx.is_in_address_range(251))

    def test_it_should_call_middleware(self) -> None:
        ctx = Context(middlewares=[TestMiddleware()])
        ctx.add_symbol(Symbol(0, "test"))
        ctx.add_line(Line("text"))
        self.assertEqual(
            ctx.collect(),
            [
                "middleware_begin",
                "test_middleware_symbol",
                "    text_middleware_line",
                "middleware_end",
            ],
        )
