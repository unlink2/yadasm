from typing import List

from .context import Context, Line
from .file import Binary
from .node import Node


class Parser:
    def __init__(self, nodes: List[Node]):
        self.nodes = nodes

    def parse(self, ctx: Context, file: Binary) -> List[str]:
        while not file.is_at_end():
            # parse until the first match is found
            parsed = None
            for node in self.nodes:
                parsed = node.parse(ctx, file)
                if parsed is not None:
                    break

            if parsed is None:
                raise ParsersExhaustedException(ctx, file, self)
            else:
                ctx.add_line(Line(parsed[0]))
                ctx.advance(parsed[1])

        return ctx.collect()


class ParsersExhaustedException(Exception):
    def __init__(
        self,
        ctx: Context,
        file: Binary,
        parser: Parser,
    ):
        Exception.__init__(self)
        self.ctx = ctx
        self.parser = parser
        self.file = file
