from typing import Callable, Any
from .context import Context

Operation = Callable[[Context, Any], Any]
