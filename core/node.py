from typing import List
from .response import Response
from .file import Binary
from .context import Context
from .comparator import Comparator
from .operation import Operation


class Node:
    def __init__(
        self,
        operation: List[Operation],
        comparator: Comparator,
        response: Response,
        mask: int,
        prefix: str = "",
        postfix: str = "",
    ):
        self.mask = mask
        self.prefix = prefix
        self.poxtfix = postfix
        self.operation = operation
        self.comparator = comparator
        self.response = response

    def parse(self, ctx: Context, file: Binary) -> str:
        pass
