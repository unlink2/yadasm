"""
A response function converts data provided by a node
into a string
"""

from typing import Any, Callable
from .context import Context

Response = Callable[[Context, Any], str]
