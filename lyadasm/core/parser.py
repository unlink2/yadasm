import logging
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

    def reset(self) -> None:
        """Called before 2nd pass"""

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
            logging.debug("Lookup table has less elements than the node list")

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
        return self.parse_double(ctx, file)

    def parse_double(self, ctx: Context, file: Binary) -> List[str]:
        """
        Puts the context in 2 pass mode
            - reads symbol only
            - resets file and context
            - reads everything and returns it
        This is best used with unbuffered line mode to trade memory for
        execution time!
        In unbuffered mode context will emit lines to the supplied
        output immediatly rahter than buffering the parsed lines.
        Symbols will still be buffered as usual.
        It is important to reset all states before the second pass!
        All parsing operations should always happen in both passes
        to guarantee an optimal parsing output.
        This is best used with unbuffered line mode!
        """
        # first pass: symbols only
        ctx.disable_lines = True
        ctx.disable_symbols = False
        self.parse_single(ctx, file)
        # second pass: lines and symbols
        ctx.disable_lines = False
        ctx.disable_symbols = True
        # need to reset some things to do another pass!
        file.reset()
        ctx.reset()
        self.reset()
        return self.parse_single(ctx, file)

    def parse_single(self, ctx: Context, file: Binary) -> List[str]:
        """
        Parses without a double pass.
        Useful for middleware-calls.
        This is best called for each pass, or directly in buffered line mode!
        """
        if self.should_build_lookup:
            self.build_lookup(ctx, self.max_opcode)

        logging.debug("Begin parsing")
        ctx.emit_on_parse_begin()

        while self._should_parse(ctx, file):
            ctx.emit_on_next(file)
            # it is possible that on_next advanced the file too far
            # check again to avoid parser errors
            if not self._should_parse(ctx, file):
                logging.warning(
                    "Middleware caused parse loop to exit early."
                    "Address: %s; File offset: %d; Middleware: %s",
                    hex(ctx.address),
                    file.offset(),
                    list(map(lambda n: type(n).__name__, ctx.middlewares)),
                )
                break
            self._next()
            # parse until the first match is found
            parsed = self._parse(ctx, file)

            if parsed is None:
                raise ParsersExhaustedException(ctx, file, self)
            else:
                ctx.add_line(parsed)
                ctx.advance(parsed.size)

        ctx.emit_on_parse_end()
        logging.debug("End parsing")

        logging.debug("Emitting middleware output")
        ctx.emit_on_output()

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
