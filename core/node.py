from typing import List, Optional, Tuple

from .comparator import Comparator
from .context import Context
from .file import Binary
from .operation import Operation
from .reader import Reader
from .response import Response


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

    def __parse_children(
        self,
        ctx: Context,
        file: Binary,
        prefix: str = "",
        postfix: str = "",
        size: int = 0,
    ) -> Optional[Tuple[str, int]]:
        result = f"{prefix}{postfix}"
        for child in self.children:
            next_res = child.parse(ctx, file, result, postfix)
            if next_res is None:
                return None
            else:
                (next_value, next_size) = next_res
                result = next_value
                size += next_size
        return (result, size)

    def parse(
        self,
        ctx: Context,
        file: Binary,
        prefix: str = "",
        postfix: str = "",
    ) -> Optional[Tuple[str, int]]:
        """
        A node reads data from a reader objects and
        compares the read data to the mask using an operation.
        If the supplied comparator returns true, the file is advanced and the
        data is read using more nodes. Finally all operations are combined
        and returned as a result string and the total size read.
        """
        data, size = self.reader(file) or (None, None)
        if data is None or size is None:
            return None

        for modifier in self.modifiers:
            data = modifier(ctx, data)

        if self.comparator(ctx, data):
            file.advance(size)
            res = self.__parse_children(
                ctx,
                file,
                prefix=(
                    f"{self.prefix}{prefix}"
                    f"{self.response(ctx, data)}{postfix}{self.postfix}"
                ),
                postfix="",
                size=size,
            )

            if res is None:
                file.rewind(size)
                return None
            else:
                value, size = res
                return (value, size)
        else:
            return None
