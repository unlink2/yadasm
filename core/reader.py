"""
A reader reads data from the binary and returns the converted
content as well as an integer representing the amount of bytes read
"""

from typing import Any, Callable, Optional, Tuple

from .file import Binary
from .numfmt import Endianess

Reader = Callable[[Binary], Optional[Tuple[Any, int]]]


def read_int_generic(
    file: Binary, size: int, endianess: Endianess
) -> Optional[Tuple[Any, int]]:
    data = file.read(size)
    if data is None:
        return None
    else:
        return (int.from_bytes(data, endianess.to_literal()), size)


def read_i8_le(file: Binary) -> Optional[Tuple[Any, int]]:
    return read_int_generic(file, 1, Endianess.LITTLE)
