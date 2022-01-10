from typing import IO, TYPE_CHECKING, Any, Callable, Dict, List, Optional

from .file import Binary

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

    def on_collect_begin(self, ctx: "Context", lines: List[str]) -> None:
        pass

    def on_collect_end(self, ctx: "Context", lines: List[str]) -> None:
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


class Context:
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
    ):
        if middlewares is None:
            middlewares = []

        self.address = address
        self.end_address = end_address
        self.start_address = address
        # buffer for all addresses in symbols and keys
        self.all_addresses: List[int] = []
        self.symbols: Dict[int, List[Symbol]] = {}
        self.lines: Dict[int, List[Line]] = {}
        self.code_indent = code_indent
        self.symbol_indent = symbol_indent
        self.indent_char = indent_char
        self.symbol_postfix = symbol_poxtfix
        self.middlewares = middlewares

        # flags can be used for storing custom information
        self.flags: Dict[str, Any] = {}

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
        if not symbol.shadow:
            self.all_addresses.append(symbol.address)
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
        self.all_addresses.append(self.address)
        if self.address not in self.lines:
            self.lines[self.address] = [line]
        else:
            self.lines[self.address].append(line)

    def advance(self, inc_by: int) -> None:
        """increment the current address"""
        self.address += inc_by

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

    def collect(self) -> List[str]:
        """Collects all lines and symbols"""
        lines: List[str] = []
        self.emit_on_collect_begin(lines)
        # get a list of all keys and remove duplicates
        all_keys = sorted(list(set(self.all_addresses)))

        for key in all_keys:
            lines += self.__collect(
                key,
                self.symbols,
                lambda symbol: symbol.fmt(
                    "".ljust(self.symbol_indent, self.indent_char),
                    self.symbol_postfix,
                ),
            )
            lines += self.__collect(
                key,
                self.lines,
                lambda line: line.fmt(
                    "".ljust(self.code_indent, self.indent_char)
                ),
            )

        self.emit_on_collect_end(lines)

        return lines

    def emit_on_parse_begin(self) -> None:
        for middleware in self.middlewares:
            middleware.on_parse_begin(self)

    def emit_on_parse_end(self) -> None:
        for middleware in self.middlewares:
            middleware.on_parse_end(self)

    def emit_on_symbol(self, symbol: Symbol) -> None:
        for middleware in self.middlewares:
            middleware.on_symbol(self, symbol)

    def emit_on_line(self, line: Line) -> None:
        for middleware in self.middlewares:
            middleware.on_line(self, line)

    def emit_on_collect_begin(self, lines: List[str]) -> None:
        for middleware in self.middlewares:
            middleware.on_collect_begin(self, lines)

    def emit_on_collect_end(self, lines: List[str]) -> None:
        for middleware in self.middlewares:
            middleware.on_collect_end(self, lines)

    def emit_on_next(self, file: Binary) -> None:
        for middleware in self.middlewares:
            middleware.on_next(self, file)

    def emit_on_output(self, streams: Dict[str, IO]) -> None:
        for middleware in self.middlewares:
            if middleware.tag in streams:
                middleware.on_output(self, streams[middleware.tag])

    def emit_on_unparsed(self, file: Binary) -> Optional[Line]:
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
        for middleware in self.middlewares:
            res = middleware.on_node_parsed(
                self, node, file, prefix, postfix, data
            )
            if res is not None:
                return res

        return None
