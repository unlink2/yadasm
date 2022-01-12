import logging
from typing import Dict, List, Optional, IO

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
        logging.info("Building lookup table for nodes...")

        self.node_lookup = {}
        for opcode in range(0, max_opcode + 1):
            for (i, node) in enumerate(self.nodes):
                if node.comparator(ctx, opcode):
                    self.node_lookup[opcode] = i
                    break

        if len(self.node_lookup.keys()) != len(self.nodes):
            logging.warning(
                "Lookup table has less elements than the node list"
            )

    def _read_opcode(self, _ctx: Context, _file: Binary) -> int:
        """
        Optional method, read opcode and do not advance.
        This is used for node_lookup. May improve performance a lot!
        """
        logging.warning("_read_opcode should always be overridden")
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
        logging.error(
            "Node was not parsed successfully! last byte: %x,"
            "address: %x, file offset: %d",
            opcode,
            ctx.address,
            file.offset(),
        )

        return ctx.emit_on_unparsed(file)

    def _should_parse(self, _ctx: Context, file: Binary) -> bool:
        """Parser loop condition"""
        return not file.is_at_end()

    def _next(self) -> None:
        """advance parser loop, called exactly once per iteration"""

    def parse(self, ctx: Context, file: Binary) -> List[str]:
        if self.should_build_lookup:
            self.build_lookup(ctx, self.max_opcode)

        ctx.emit_on_parse_begin()

        while self._should_parse(ctx, file):
            self._next()
            ctx.emit_on_next(file)
            # it is possible that on_next advanced the file too far
            # check again to avoid parser errors
            if not self._should_parse(ctx, file):
                break
            # parse until the first match is found
            parsed = self._parse(ctx, file)

            if parsed is None:
                raise ParsersExhaustedException(ctx, file, self)
            else:
                ctx.add_line(parsed)
                ctx.advance(parsed.size)

        ctx.emit_on_parse_end()

        return ctx.collect()

    def output(
        self,
        ctx: Context,
        outstream: IO,
        data: List[str],
        middleware_streams: Dict[str, IO] = None,
    ) -> None:
        """
        Outputs the parser result to an IO stream.
        Allows supplying a dict of IOs mapped to middleware tags
        to call their respective output event
        """
        outstream.write("\n".join(data))
        outstream.write("\n")
        if middleware_streams is not None:
            ctx.emit_on_output(middleware_streams)


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
