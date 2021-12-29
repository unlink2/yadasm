from typing import List
from .context import Context
from .file import Binary
from .response import Response
from .node import Node


class Parser:
    def __init__(self, opcode: Node, data: List[Node]):
        self.opcode = opcode
        self.data = data

    def parse(self, ctx: Context, file: Binary) -> Response:
        pass
