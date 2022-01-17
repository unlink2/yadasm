import logging
from typing import IO, TYPE_CHECKING, Any, Callable, Dict, List, Optional, Set

from .file import Binary, Output
from .reset import Resetable

# avoids cyclic import; always False at runtime!
if TYPE_CHECKING:
    from .node import Node


class Fmt:
    def __init__(self, prefix: str = None, postfix: str = None):
        self.prefix = prefix
        self.postfix = postfix

    def content(self) -> str:
        return ""

    def fmt(self, prefix: str = "", postfix: str = "") -> str:
        if self.postfix is not None:
            postfix = self.postfix
        if self.prefix is not None:
            prefix = self.prefix
        return f"{prefix}{self.content()}{postfix}"


class Symbol(Fmt):
    def __init__(
        self,
        address: int,
        name: str,
        shadow: bool = False,
        order: int = 0,
        prefix: str = None,
        postfix: str = None,
    ):
        """
        If a symbol defines a custom postfix it will ignore the global postfix
        provided by the parser
        Shadow labels are defined, but will not be output
        Order is used to sort symbols lower order is sorted first
        """
        Fmt.__init__(self, prefix, postfix)
        self.address = address
        self.name = name
        self.shadow = shadow
        self.order = order

    def content(self) -> str:
        return self.name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Symbol):
            return self.address == other.address and self.name == other.name
        else:
            return False


class Line(Fmt):
    def __init__(
        self, text: str, size: int = 0, prefix: str = None, postfix: str = None
    ):
        Fmt.__init__(self, prefix, postfix)
        self.text = text
        self.size = size

    def content(self) -> str:
        return self.text

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Line):
            return other.text == self.text
        else:
            return False


class Middleware:
    """
    A middleware is called whenever a line or symbol is added.
    It has the ability to modify the recorded content dynamically.
    Intended usecases:
        - rename labels
        - replace addresses with variable names
    The middleware operates using simple string manipulation.
    """

    def __init__(self, tag: str = "") -> None:
        self.tag = tag

    def on_parse_begin(self, ctx: "Context") -> None:
        pass

    def on_parse_end(self, ctx: "Context") -> None:
        pass

    def on_collect_begin(self, ctx: "Context", output: Output) -> None:
        pass

    def on_collect_end(self, ctx: "Context", output: Output) -> None:
        pass

    def on_symbol(self, ctx: "Context", symbol: Symbol) -> None:
        pass

    def on_line(self, ctx: "Context", line: Line) -> None:
        pass

    def on_next(self, ctx: "Context", file: Binary) -> None:
        """Emmited before the next node is parsed"""

    def on_unparsed(self, _ctx: "Context", _file: Binary) -> Optional[Line]:
        """
        Called whenever no sutable parser is found.
        It is up to this method to handle this case or return None.
        This method should advance the file and ctx addresses as needed.
        """
        return None

    def on_output(self, _ctx: "Context", _stream: IO) -> None:
        pass

    def on_node_parsed(
        self,
        ctx: "Context",
        node: "Node",
        file: Binary,
        prefix: str,
        postfix: str,
        data: Any,
    ) -> Optional[Any]:
        """
        Called for ever node that has successfully applied its comparator.
        The first node that does not return None will return its updated value
        to the caller.
        It may also return a new Response function to override the
        node's output.
        Note:
            A node's children may still fail parsing even if this event is
            emmitted!
            If the fully parsed node is required use on_line instead!
            Sometimes the changed result is fully ignored by the parser
            after the compare step (e.g. if an opcode is replaced with a
            textual representation)
        """


class Context(Resetable):
    """
    The context class is the global result object.
    It contains all lines, keeps track of the current address
    and keeps track of labels and symbols.
    """

    def __init__(
        self,
        address: int = 0,
        code_indent: int = 4,
        symbol_indent: int = 0,
        indent_char: str = " ",
        symbol_poxtfix: str = "",
        end_address: Optional[int] = None,
        middlewares: List[Middleware] = None,
        output: Output = None,
        middleware_streams: Dict[str, IO] = None,
        unbuffered_lines: bool = False,
    ):
        if middlewares is None:
            middlewares = []

        if output is None:
            output = Output()

        self.address = address
        self.end_address = end_address
        self.start_address = address
        # buffer for all addresses in symbols and keys
        self.all_addresses: Set[int] = set()
        self.symbols: Dict[int, List[Symbol]] = {}
        self.lines: Dict[int, List[Line]] = {}
        self.code_indent = code_indent
        self.symbol_indent = symbol_indent
        self.indent_char = indent_char
        self.symbol_postfix = symbol_poxtfix
        self.middlewares = middlewares
        self.__disabled_middlewares: List[Middleware] = []
        self.output = output
        self.middleware_streams = middleware_streams

        # symbols only can be used as a first pass
        self._disable_symbols = False
        self._disable_lines = False
        # unbuffere dlines will emit lines to output immediatly
        self.unbuffered_lines = unbuffered_lines
        self.pass_count = 0  # counts how often ctx was reset

        # flags can be used for storing custom information
        self.flags: Dict[str, Any] = {}

    def reset(self) -> None:
        """Resets address"""
        self.address = self.start_address
        self.pass_count += 1

    def disable_middleware(
        self, replace_with: List[Middleware] = None
    ) -> None:
        if replace_with is None:
            replace_with = []
        self.__disabled_middlewares = self.middlewares
        self.middlewares = replace_with

    def restore_middleware(self) -> None:
        self.middlewares = self.__disabled_middlewares
        self.__disabled_middlewares = []

    def set_flag(self, flag: str, data: Any) -> None:
        self.flags[flag] = data

    def get_flag(self, flag: str) -> Optional[Any]:
        if self.has_flag(flag):
            return self.flags[flag]
        else:
            return None

    def has_flag(self, flag: str) -> bool:
        return flag in self.flags

    def is_in_address_range(self, addr: int) -> bool:
        return self.start_address <= addr and (
            self.end_address is None or self.end_address > addr
        )

    def add_symbol(self, symbol: Symbol) -> None:
        """Add a label for a certain address"""

        self.emit_on_symbol(symbol)
        self.add_symbol_no_emit(symbol)

    def add_symbol_no_emit(self, symbol: Symbol) -> None:
        if self.symbols_disabled():
            return
        if not symbol.shadow:
            logging.debug(
                "Symbol %s=%s added to all addresses",
                symbol.name,
                hex(symbol.address),
            )
            self.all_addresses.add(symbol.address)
        if symbol.address not in self.symbols:
            self.symbols[symbol.address] = [symbol]
        elif symbol not in self.symbols[symbol.address]:
            self.symbols[symbol.address].append(symbol)
            self.symbols[symbol.address] = sorted(
                self.symbols[symbol.address], key=lambda x: x.order
            )

    def is_symbol_at(self, address: int) -> bool:
        return address in self.symbols and len(self.symbols[address]) > 0

    def get_symbol_at(self, address: int, default: str = "") -> str:
        if self.is_symbol_at(address):
            return self.symbols[address][0].fmt()
        else:
            return default

    def add_line(self, line: Line) -> None:
        """Add a line at the current address"""

        self.emit_on_line(line)
        self.add_line_no_emit(line)

    def add_line_no_emit(self, line: Line) -> None:
        # bail if we only emit symbols
        if self.lines_disabled():
            return
        self.all_addresses.add(self.address)
        if self.address not in self.lines:
            self.lines[self.address] = [line]
        else:
            self.lines[self.address].append(line)

    def advance(self, inc_by: int) -> None:
        """increment the current address"""
        self.__collect_unbuffered()
        self.address += inc_by

    def __collect_unbuffered(self) -> None:
        # if we do not buffer, also emit all symbols and remove
        # lines for the current address now!
        if not self.unbuffered_lines or self.lines_disabled():
            return
        if self.address in self.all_addresses:
            logging.debug(
                "Collecting unbuffered lines and symbols at %s",
                hex(self.address),
            )
            self.__collect_symbols(self.address)
            self.__collect_lines(self.address)
            self.all_addresses.remove(self.address)

    def __collect(
        self,
        key: int,
        collection: Dict[int, Any],
        mapper: Callable[[Any], str],
    ) -> List[str]:
        if key in collection:
            return list(
                map(
                    mapper,
                    collection[key],
                )
            )
        else:
            return []

    def __collect_symbols(self, address: int) -> None:
        self.output.on_lines(
            self.__collect(
                address,
                self.symbols,
                lambda symbol: symbol.fmt(
                    "".ljust(self.symbol_indent, self.indent_char),
                    self.symbol_postfix,
                ),
            )
        )

    def __collect_lines(self, address: int) -> None:
        self.output.on_lines(
            self.__collect(
                address,
                self.lines,
                lambda line: line.fmt(
                    "".ljust(self.code_indent, self.indent_char)
                ),
            )
        )
        if self.unbuffered_lines and address in self.lines:
            logging.debug(
                "Removing lines from buffer at %s", hex(self.address)
            )
            self.lines.pop(address)

    def collect(self) -> List[str]:
        """Collects all lines and symbols"""
        # this will do nothing if lines are unbuffered!
        if self.lines_disabled():
            return []
        self.output.begin()

        self.emit_on_collect_begin(self.output)
        # get a list of all keys and remove duplicates
        all_keys = sorted(list(self.all_addresses))

        logging.debug(
            "Collecting lines for addresses: %s",
            list(map(hex, all_keys)),
        )
        for key in all_keys:
            self.__collect_symbols(key)
            self.__collect_lines(key)

        self.emit_on_collect_end(self.output)
        self.output.finish()

        return self.output.collect()

    def symbols_disabled(self) -> bool:
        return self._disable_symbols

    def lines_disabled(self) -> bool:
        return self._disable_lines

    def disable_lines(self) -> None:
        self._disable_lines = True

    def enable_lines(self) -> None:
        self._disable_lines = False

    def disable_symbols(self) -> None:
        self._disable_symbols = True

    def enable_symbols(self) -> None:
        self._disable_symbols = False

    def pass_one(self, parser: Resetable, file: Resetable) -> None:
        """enables pass phase 1"""
        # first pass: symbols only
        file.reset()
        parser.reset()
        self.disable_lines()
        self.enable_symbols()

    def pass_two(self, parser: Resetable, file: Resetable) -> None:
        """enables pass phase 2"""
        # second pass: lines and symbols
        self.reset()
        file.reset()
        parser.reset()
        self.enable_lines()
        self.disable_symbols()

    def single_pass(self, _parser: Resetable, _file: Resetable) -> None:
        """enables single pass mode"""
        self.enable_lines()
        self.enable_symbols()

    def emit_on_parse_begin(self) -> None:
        logging.debug("Emitting parse_begin")
        for middleware in self.middlewares:
            middleware.on_parse_begin(self)

    def emit_on_parse_end(self) -> None:
        logging.debug("Emitting parse_end")
        for middleware in self.middlewares:
            middleware.on_parse_end(self)

    def emit_on_symbol(self, symbol: Symbol) -> None:
        logging.debug("Emitting on_symbol")
        for middleware in self.middlewares:
            middleware.on_symbol(self, symbol)

    def emit_on_line(self, line: Line) -> None:
        logging.debug("Emitting on_line")
        for middleware in self.middlewares:
            middleware.on_line(self, line)

    def emit_on_collect_begin(self, output: Output) -> None:
        logging.debug("Emitting on_collect_begin")
        for middleware in self.middlewares:
            middleware.on_collect_begin(self, output)

    def emit_on_collect_end(self, output: Output) -> None:
        logging.debug("Emitting on_collect_end")
        for middleware in self.middlewares:
            middleware.on_collect_end(self, output)

    def emit_on_next(self, file: Binary) -> None:
        logging.debug("Emitting on_next")
        for middleware in self.middlewares:
            middleware.on_next(self, file)

    def emit_on_output(self) -> None:
        logging.debug("Emitting on_output")
        if self.middleware_streams is None:
            return
        for middleware in self.middlewares:
            if middleware.tag in self.middleware_streams:
                middleware.on_output(
                    self, self.middleware_streams[middleware.tag]
                )

    def emit_on_unparsed(self, file: Binary) -> Optional[Line]:
        logging.debug("Emitting on_unparsed")
        for middleware in self.middlewares:
            line = middleware.on_unparsed(self, file)
            if line is not None:
                return line
        return None

    def emit_on_node_parsed(
        self,
        node: "Node",
        file: Binary,
        prefix: str,
        postfix: str,
        data: Any,
    ) -> Optional[Any]:
        """
        Calls node parser middleware.
        Returns the first not None value to the caller.
        """
        logging.debug("Emitting on_node_parsed")
        for middleware in self.middlewares:
            res = middleware.on_node_parsed(
                self, node, file, prefix, postfix, data
            )
            if res is not None:
                return res

        return None
