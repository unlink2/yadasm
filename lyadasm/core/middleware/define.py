from typing import Dict, Any, Optional, List

from ..context import Middleware, Context, Symbol
from ..file import Binary
from ..node import Node


class DefineMiddleware(Middleware):
    def __init__(
        self,
        definitions: Dict[Any, Any],
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
        if data in self.definitions:
            return self.definitions[data]
        elif node.response(ctx, data) in self.definitions:
            return self.definitions[node.response(ctx, data)]
        else:
            return None
