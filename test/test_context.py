import unittest

from core.context import Context, Symbol


class TestContext(unittest.TestCase):
    def test_it_should_add_symbol(self) -> None:
        ctx = Context()
        ctx.add_symbol(Symbol(100, "lab_100", 0))
        ctx.advance(2)
        ctx.add_symbol(Symbol(102, "lab_102", 1))

        self.assertEqual(len(ctx.symbols.keys()), 2)
        self.assertNotEqual(ctx.symbols[100], None)
        self.assertNotEqual(ctx.symbols[102], None)

        self.assertEqual(ctx.symbols[100], Symbol(100, "lab_100", 0))

        self.assertEqual(ctx.symbols[102], Symbol(102, "lab_102", 1))

    def test_it_should_add_lines(self) -> None:
        ctx = Context()
        ctx.add_line("line1")
        ctx.advance(2)
        ctx.add_line("line2.0")
        ctx.add_line("line2.1")

        self.assertEqual(len(ctx.lines.keys()), 2)
        self.assertNotEqual(ctx.lines[0], None)
        self.assertNotEqual(ctx.lines[2], None)

        self.assertEqual(len(ctx.lines[0]), 1)
        self.assertEqual(len(ctx.lines[2]), 2)

        self.assertEqual(ctx.lines[0], ["line1"])
        self.assertEqual(ctx.lines[2], ["line2.0", "line2.1"])
