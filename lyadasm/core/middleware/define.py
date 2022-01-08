from typing import Any, Callable, Dict, List, Optional

from ..context import Context, Middleware, Symbol
from ..file import Binary
from ..node import Node

# context, input data, default reutrn value -> new value
DefinitionModifier = Callable[[Context, Any, Any], Any]


class Definition:
    def __init__(
        self, data: Any, modifier: Optional[DefinitionModifier] = None
    ) -> None:
        self.default_data = data
        self.modifier = modifier

    def get_data(self, ctx: Context, data: Any) -> Any:
        if self.modifier is None:
            return self.default_data
        else:
            return self.modifier(ctx, data, self.default_data)


class DefineMiddleware(Middleware):
    def __init__(
        self,
        definitions: Dict[Any, Definition],
        symbols: List[Symbol] = None,
        tag: str = "Define",
    ):
        Middleware.__init__(self, tag)
        if symbols is None:
            symbols = []
        self.symbols = symbols
        self.definitions = definitions

    def on_parse_begin(self, ctx: Context) -> None:
        for symbol in self.symbols:
            ctx.add_symbol_no_emit(symbol)

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
            return self.definitions[data].get_data(ctx, data)
        elif node.response(ctx, data) in self.definitions:
            return self.definitions[node.response(ctx, data)].get_data(
                ctx, data
            )
        else:
            return response
