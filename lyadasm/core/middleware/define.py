import logging

from typing import Any, Callable, Dict, List, Optional

from ..context import Context, Middleware, Symbol
from ..file import Binary
from ..node import Node

# context, input data, default reutrn value -> new value
DefinitionModifier = Callable[[Context, Any, Any], Any]
DefinitionCondition = Callable[[Context, Node, Binary, str, str, Any], bool]


def always_true(
    _ctx: Context,
    _node: Node,
    _file: Binary,
    _prefix: str,
    _postfix: str,
    _data: Any,
) -> bool:
    return True


def always_false(
    _ctx: Context,
    _node: Node,
    _file: Binary,
    _prefix: str,
    _postfix: str,
    _data: Any,
) -> bool:
    return False


class Definition:
    def __init__(
        self,
        data: Any,
        modifier: Optional[DefinitionModifier] = None,
        condition: DefinitionCondition = always_true,
    ) -> None:
        self.default_data = data
        self.modifier = modifier
        self.condition = condition

    def get_data(
        self,
        ctx: Context,
        node: Node,
        file: Binary,
        prefix: str,
        postfix: str,
        data: Any,
    ) -> Any:
        if not self.condition(ctx, node, file, prefix, postfix, data):
            return None
        elif self.modifier is None:
            return self.default_data
        else:
            return self.modifier(ctx, data, self.default_data)


class DefineMiddleware(Middleware):
    def __init__(
        self,
        definitions: Dict[Any, Definition] = None,
        symbols: List[Symbol] = None,
        force_symbol_output: bool = False,
        tag: str = "Define",
    ):
        Middleware.__init__(self, tag)
        if definitions is None:
            definitions = {}
        if symbols is None:
            symbols = []
        self.symbols = symbols
        self.definitions = definitions
        self.force_symbol_output = force_symbol_output

    def add_symbol(self, symbol: Symbol) -> "DefineMiddleware":
        self.symbols.append(symbol)
        return self

    def add_definition(
        self, address: int, definition: Definition
    ) -> "DefineMiddleware":
        self.definitions[address] = definition
        return self

    def add_symbol_definition(
        self, symbol: Symbol, condition: DefinitionCondition = always_true
    ) -> "DefineMiddleware":
        self.add_symbol(symbol)
        self.add_definition(
            symbol.address,
            self._make_symbol_definition(symbol.fmt(), condition),
        )
        return self

    def _make_symbol_definition(
        self, constant: str, condition: DefinitionCondition = always_true
    ) -> Definition:
        return Definition(lambda ctx, i: constant, condition=condition)

    def on_parse_begin(self, ctx: Context) -> None:
        for symbol in self.symbols:
            # set the shadow flag when needed
            symbol.shadow = (
                not ctx.is_in_address_range(symbol.address)
                and not self.force_symbol_output
            )

            logging.debug(
                "Adding new symbol at %s = %s; is_shadowed=%s",
                hex(symbol.address),
                symbol.name,
                symbol.shadow,
            )

            ctx.add_symbol(symbol, False)

    def on_node_parsed(
        self,
        ctx: Context,
        node: Node,
        file: Binary,
        prefix: str,
        postfix: str,
        data: Any,
    ) -> Optional[Any]:
        response = None
        if data in self.definitions:
            return self.definitions[data].get_data(
                ctx, node, file, prefix, postfix, data
            )
        elif node.response(ctx, data) in self.definitions:
            return self.definitions[node.response(ctx, data)].get_data(
                ctx, node, file, prefix, postfix, data
            )
        else:
            return response
