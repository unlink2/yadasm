from typing import Dict, Any, Optional

from ..context import Middleware, Context
from ..file import Binary
from ..node import Node


class DefineMiddleware(Middleware):
    def __init__(self, definitions: Dict[Any, Any], tag: str = "Define"):
        Middleware.__init__(self, tag)
        self.definitions = definitions

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
