import unittest
from typing import Any, Optional
from io import StringIO

from lyadasm.core.archs.arch6502 import Parser6502, Parser6502Bytes
from lyadasm.core.context import Context, Line, Middleware, Symbol
from lyadasm.core.file import Binary, Output, StreamOutput
from lyadasm.core.node import Node


class MiddlewareTest(Middleware):
    next_called = 0
    parse_begin = False
    parse_end = False

    def on_parse_begin(self, ctx: "Context") -> None:
        self.parse_begin = True

    def on_parse_end(self, ctx: "Context") -> None:
        self.parse_end = True

    def on_collect_begin(self, ctx: "Context", output: Output) -> None:
        output.on_line("middleware_begin")

    def on_collect_end(self, ctx: "Context", output: Output) -> None:
        output.on_line("middleware_end")

    def on_symbol(self, ctx: "Context", symbol: Symbol) -> None:
        symbol.name += "_middleware_symbol"

    def on_line(self, ctx: "Context", line: Line) -> None:
        line.text += "_middleware_line"

    def on_next(self, ctx: "Context", file: Binary) -> None:
        self.next_called += 1

    def on_unparsed(self, ctx: "Context", file: Binary) -> Optional[Line]:
        file.advance(1)
        ctx.advance(1)
        return Line("Unparsed")

    def on_node_parsed(
        self,
        ctx: "Context",
        node: Node,
        file: Binary,
        prefix: str,
        postfix: str,
        data: Any,
    ) -> Any:
        if data == 0xEA:
            return data + 1
        else:
            return None


class AbortMiddlewareTest(Middleware):
    def on_node_parsed(
        self,
        ctx: "Context",
        node: Node,
        file: Binary,
        prefix: str,
        postfix: str,
        data: Any,
    ) -> Any:
        if data == 0xEA and "nop" in node.response(ctx, data):
            node.abort()


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

    def test_it_should_not_emit_symbol_at_end_address(self) -> None:
        ctx = Context(address=100, end_address=110)
        ctx.add_symbol(
            Symbol(
                99, "not_ok_symbol1:", shadow=not ctx.is_in_address_range(99)
            )
        )
        ctx.add_symbol(
            Symbol(100, "ok_symbol1:", shadow=not ctx.is_in_address_range(100))
        )
        ctx.add_symbol(
            Symbol(109, "ok_symbol2:", shadow=not ctx.is_in_address_range(109))
        )
        ctx.add_symbol(
            Symbol(
                110, "not_ok_symbol2:", shadow=not ctx.is_in_address_range(110)
            )
        )
        self.assertEqual(ctx.collect(), ["ok_symbol1:", "ok_symbol2:"])

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

        self.assertTrue(ctx.is_in_address_range(149))
        self.assertTrue(ctx.is_in_address_range(100))
        self.assertTrue(ctx.is_in_address_range(110))
        self.assertFalse(ctx.is_in_address_range(99))
        self.assertFalse(ctx.is_in_address_range(2))
        self.assertFalse(ctx.is_in_address_range(151))
        self.assertFalse(ctx.is_in_address_range(251))

    def test_it_should_call_middleware(self) -> None:
        parser = Parser6502()
        middelware = MiddlewareTest()
        ctx = Context(middlewares=[Middleware(), middelware])
        ctx.add_symbol(Symbol(0, "test"))
        ctx.add_line(Line("text"))

        collected = parser.parse(ctx, Binary(bytes([0xE6, 0xEA, 0xEA])))

        self.assertEqual(
            collected,
            [
                "middleware_begin",
                "test_middleware_symbol",
                "    text_middleware_line",
                "    inc $eb_middleware_line",
                "    nop_middleware_line",
                "middleware_end",
            ],
        )
        self.assertEqual(middelware.next_called, 4)
        self.assertTrue(middelware.parse_begin)
        self.assertTrue(middelware.parse_end)

    def test_it_should_abort_parser_conditionally(self) -> None:
        parser = Parser6502Bytes()
        ctx = Context(middlewares=[AbortMiddlewareTest()])

        collected = parser.parse(ctx, Binary(bytes([0xEA, 0xE6, 0xAA])))

        self.assertEqual(collected, ["    !byte $ea", "    inc $aa"])

    def test_it_should_call_unparsed(self) -> None:
        parser = Parser6502()
        ctx = Context(middlewares=[Middleware(), MiddlewareTest()])

        collected = parser.parse(ctx, Binary(bytes([0xEA, 0xFF])))
        print(collected)
        self.assertEqual(
            collected,
            [
                "middleware_begin",
                "    nop_middleware_line",
                "    Unparsed_middleware_line",
                "middleware_end",
            ],
        )

    def test_symbol_should_use_postfix_and_prefix(self) -> None:
        sym = Symbol(0x100, "test")
        self.assertEqual(sym.fmt(">", ":"), ">test:")

    def test_symbol_should_use_custom_postfix_and_prefix(self) -> None:
        sym = Symbol(0x100, "test", prefix="<<", postfix=">>")
        self.assertEqual(sym.fmt(">", ":"), "<<test>>")

    def test_symbol_should_use_prefix(self) -> None:
        sym = Symbol(0x100, "test", prefix="; ")
        self.assertEqual(sym.fmt(""), "; test")

    def test_flags(self) -> None:
        ctx = Context()

        self.assertFalse(ctx.has_flag("test"))
        self.assertEqual(ctx.get_flag("test"), None)

        ctx.set_flag("test", True)

        self.assertTrue(ctx.has_flag("test"))
        self.assertNotEqual(ctx.get_flag("test"), None)

    def test_symbol_getter(self) -> None:
        ctx = Context()
        ctx.add_symbol(Symbol(0x100, "test", order=2))

        self.assertEqual(ctx.get_symbol_at(0x100, "default"), "test")
        self.assertEqual(ctx.get_symbol_at(0x101, "default"), "default")

        ctx.add_symbol(Symbol(0x100, "test2", order=1))
        self.assertEqual(ctx.get_symbol_at(0x100, "default"), "test2")

    def test_it_should_not_output_shadowed_label(self) -> None:
        ctx = Context()
        ctx.add_symbol(Symbol(0x100, "test", shadow=True))

        self.assertEqual(ctx.collect(), [])

    def test_it_should_disable_middleware(self) -> None:
        ctx = Context(middlewares=[Middleware(), Middleware()])
        ctx.disable_middleware()

        self.assertEqual(len(ctx.middlewares), 0)
        ctx.restore_middleware()
        self.assertEqual(len(ctx.middlewares), 2)

        ctx.disable_middleware([Middleware()])
        self.assertEqual(len(ctx.middlewares), 1)
        ctx.restore_middleware()
        self.assertEqual(len(ctx.middlewares), 2)

    def test_it_should_not_add_lines_if_dsabled(self) -> None:
        ctx = Context()
        ctx.disable_lines()
        ctx.add_line(Line("test"))
        self.assertEqual(len(ctx.lines), 0)

    def test_it_should_not_add_symbols_if_dsabled(self) -> None:
        ctx = Context()
        ctx.disable_symbols()
        ctx.add_line(Line("test"))
        self.assertEqual(len(ctx.symbols), 0)

    def test_it_should_use_unbuffered_output(self) -> None:
        strio = StringIO()
        ctx = Context(
            address=1, unbuffered_lines=True, output=StreamOutput(strio)
        )
        ctx.add_symbol(Symbol(1, "Symbol1"))
        ctx.add_symbol(Symbol(1, "Symbol2"))
        ctx.add_line(Line("test"))

        self.assertEqual(ctx.collect(), [])
        strio.seek(0)
        self.assertEqual(
            strio.readlines(), ["Symbol1\n", "Symbol2\n", "    test\n"]
        )

    def test_it_should_count_resets(self) -> None:
        ctx = Context()
        self.assertEqual(ctx.pass_count, 0)
        ctx.reset()
        ctx.reset()
        self.assertEqual(ctx.pass_count, 2)
