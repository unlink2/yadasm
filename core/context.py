from typing import Any, Callable, Dict, List


class Symbol:
    def __init__(self, address: int, name: str):
        self.address = address
        self.name = name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Symbol):
            return self.address == other.address and self.name == other.name
        else:
            return False


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
    ):
        self.address = address
        self.start_address = address
        # buffer for all addresses in symbols and keys
        self.all_addresses: List[int] = []
        self.symbols: Dict[int, List[Symbol]] = {}
        self.lines: Dict[int, List[str]] = {}
        self.code_indent = code_indent
        self.symbol_indent = symbol_indent
        self.indent_char = indent_char
        self.symbol_postfix = symbol_poxtfix

    def add_symbol(self, symbol: Symbol) -> None:
        """Add a label for a certain address"""
        self.all_addresses.append(symbol.address)
        if symbol.address not in self.symbols:
            self.symbols[symbol.address] = [symbol]
        else:
            self.symbols[symbol.address].append(symbol)

    def add_line(self, line: str) -> None:
        """Add a line at the current address"""
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
        # get a list of all keys and remove duplicates
        all_keys = sorted(list(set(self.all_addresses)))
        lines: List[str] = []

        for key in all_keys:
            lines += self.__collect(
                key,
                self.symbols,
                lambda symbol: (
                    f"{''.ljust(self.symbol_indent, self.indent_char)}"
                    f"{symbol.name}"
                    f"{self.symbol_postfix}"
                ),
            )
            lines += self.__collect(
                key,
                self.lines,
                lambda line: (
                    f"{''.ljust(self.code_indent, self.indent_char)}{line}"
                ),
            )

        return lines
