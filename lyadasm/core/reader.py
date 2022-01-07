"""
A reader reads data from the binary and returns the converted
content as well as an integer representing the amount of bytes read
"""

from typing import Any, Callable, Optional, Tuple

from .context import Context
from .file import Binary
from .numfmt import Endianess

Reader = Callable[[Context, Binary], Optional[Tuple[Any, int]]]


def read_int_generic(
    file: Binary, size: int, endianess: Endianess
) -> Optional[Tuple[Any, int]]:
    data = file.read(size)
    if data is None:
        return None
    else:
        return (int.from_bytes(data, endianess.to_literal()), size)


def read_i8_le(_ctx: Context, file: Binary) -> Optional[Tuple[Any, int]]:
    return read_int_generic(file, 1, Endianess.LITTLE)


def read_i16_le(_ctx: Context, file: Binary) -> Optional[Tuple[Any, int]]:
    return read_int_generic(file, 2, Endianess.LITTLE)


def read_i24_le(_ctx: Context, file: Binary) -> Optional[Tuple[Any, int]]:
    return read_int_generic(file, 3, Endianess.LITTLE)


def read_none(_ctx: Context, _file: Binary) -> Optional[Tuple[Any, int]]:
    return (0, 0)


def read_idyn_le(ctx: Context, file: Binary) -> Optional[Tuple[Any, int]]:
    length = ctx.get_flag("read_idyn_le")
    if length is None or not isinstance(length, int):
        return read_int_generic(file, 1, Endianess.LITTLE)
    else:
        return read_int_generic(file, length, Endianess.LITTLE)
