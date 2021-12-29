"""
A reader reads data from the binary and returns the converted
content as well as an integer representing the amount of bytes read
"""

from typing import Callable, Any, Tuple
from .file import Binary

Reader = Callable[[Binary], Tuple[Any, int]]
