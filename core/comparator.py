from typing import Any, Callable

Comparator = Callable[[Any], bool]


def always_true(_lhs: Any) -> bool:
    return True


def always_false(_lhs: Any) -> bool:
    return False
