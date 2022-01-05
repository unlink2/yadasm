from typing import Dict, List, Optional

from .context import Context, Line
from .file import Binary
from .node import Node


class Parser:
    def __init__(
        self,
        nodes: List[Node] = None,
        should_build_lookup: bool = True,
        max_opcode: int = 255,
    ):
        if nodes is None:
            nodes = []
        # lookup table for nodes -> maps opcode to index
        self.node_lookup: Dict[int, int] = {}
        self.nodes = nodes
        self.should_build_lookup = should_build_lookup
        self.max_opcode = max_opcode

    def build_lookup(self, ctx: Context, max_opcode: int = 0xFF) -> None:
        """Builds a lookup table for opcode -> node index"""
        self.node_lookup = {}
        for opcode in range(0, max_opcode + 1):
            for (i, node) in enumerate(self.nodes):
                if node.comparator(ctx, opcode):
                    self.node_lookup[opcode] = i
                    break

    def _read_opcode(self, _ctx: Context, _file: Binary) -> int:
        """
        Optional method, read opcode and do not advance.
        This is used for node_lookup. May improve performance a lot!
        """
        return -1

    def _parse(self, ctx: Context, file: Binary) -> Optional[Line]:
        # first attempt fast lookup
        opcode = self._read_opcode(ctx, file)
        if opcode in self.node_lookup:
            parsed = self.nodes[self.node_lookup[opcode]].parse(ctx, file)
            if parsed is not None:
                return parsed

        # if fast lookup fails attempt iteration
        for node in self.nodes:
            parsed = node.parse(ctx, file)
            if parsed is not None:
                return parsed

        # return None if nothing worked
        return None

    def parse(self, ctx: Context, file: Binary) -> List[str]:
        if self.should_build_lookup:
            self.build_lookup(ctx, self.max_opcode)

        while not file.is_at_end():
            # parse until the first match is found
            parsed = self._parse(ctx, file)

            if parsed is None:
                raise ParsersExhaustedException(ctx, file, self)
            else:
                ctx.add_line(parsed)
                ctx.advance(parsed.size)

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
