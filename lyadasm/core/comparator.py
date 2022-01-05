from typing import Any, Callable
from .context import Context

Comparator = Callable[[Context, Any], bool]


def always_true(_ctx: Context, _lhs: Any) -> bool:
    return True


def always_false(_ctx: Context, _lhs: Any) -> bool:
    return False
