from typing import List, Optional, Tuple

from .comparator import Comparator
from .context import Context
from .file import Binary
from .operation import Operation
from .response import Response
from .reader import Reader


class Node:
    def __init__(
        self,
        reader: Reader,
        modifiers: List[Operation],
        comparator: Comparator,
        response: Response,
        children: List["Node"] = None,
        prefix: str = "",
        postfix: str = "",
    ):
        self.reader = reader
        self.prefix = prefix
        self.postfix = postfix
        self.modifiers = modifiers
        self.comparator = comparator
        self.response = response
        self.children = [] if children is None else children

    def __parse_children(self, ctx: Context, file: Binary) -> Optional[Tuple[str, int]]:
        result = ""
        size = 0
        for child in self.children:
            next_res = child.parse(ctx, file, False)
            if next_res is None:
                return None
            else:
                (next_value, next_size) = next_res
                result += next_value
                size += next_size
        return (result, size)

    def parse(
        self, ctx: Context, file: Binary, advance: bool = True
    ) -> Optional[Tuple[str, int]]:
        """
        A node reads data from a reader objects and
        compares the read data to the mask using an operation.
        If the supplied comparator returns true, the file is advanced and the
        data is read using more nodes. Finally all operations are combined
        and returned as a result string and the total size read.
        """
        data, size = self.reader(file)
        for modifier in self.modifiers:
            data = modifier(data)

        if self.comparator(data):
            child_res = self.__parse_children(ctx, file)

            if child_res is None:
                return None
            else:
                child_value, child_size = child_res
                size += child_size
                if advance:
                    file.advance(size)

                return (
                    f"{self.prefix}{self.response(data, ctx)}{self.postfix}{child_value}",
                    size,
                )
        else:
            return None
