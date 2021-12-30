from typing import Dict, List


class Symbol:
    def __init__(self, address: int, name: str, line: int):
        self.address = address
        self.name = name
        self.line = line

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Symbol):
            return (
                self.address == other.address
                and self.name == other.name
                and self.line == other.line
            )
        else:
            return False


class Context:
    """
    The context class is the global result object.
    It contains all lines, keeps track of the current address
    and keeps track of labels and symbols.
    """

    def __init__(self, address: int = 0):
        self.address = address
        self.symbols: Dict[int, Symbol] = {}
        self.lines: Dict[int, List[str]] = {}

    def add_symbol(self, symbol: Symbol) -> None:
        """Add a label for a certain address"""
        self.symbols[symbol.address] = symbol

    def add_line(self, line: str) -> None:
        """Add a line at the current address"""
        if self.address not in self.lines:
            self.lines[self.address] = [line]
        else:
            self.lines[self.address].append(line)

    def advance(self, inc_by: int) -> None:
        """increment the current address"""
        self.address += inc_by
