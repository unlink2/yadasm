from typing import Any

from .error import AbstractException


class Comparator:
    """Base compare operation"""

    def compare(self, lhs: Any, rhs: Any) -> bool:
        raise AbstractException()


class AlwaysTrue(Comparator):
    def compare(self, lhs: Any, rhs: Any) -> bool:
        return True


class AlwaysFalse(Comparator):
    def compare(self, lhs: Any, rhs: Any) -> bool:
        return False


class Eq(Comparator):
    def compare(self, lhs: Any, rhs: Any) -> bool:
        return lhs == rhs


class NotEq(Comparator):
    def compare(self, lhs: Any, rhs: Any) -> bool:
        return lhs != rhs


class And(Comparator):
    def __init__(self, op1: Comparator, op2: Comparator):
        self.op1 = op1
        self.op2 = op2

    def compare(self, lhs: Any, rhs: Any) -> bool:
        return self.op1.compare(lhs, rhs) and self.op2.compare(lhs, rhs)


class Or(Comparator):
    def __init__(self, op1: Comparator, op2: Comparator):
        self.op1 = op1
        self.op2 = op2

    def compare(self, lhs: Any, rhs: Any) -> bool:
        return self.op1.compare(lhs, rhs) or self.op2.compare(lhs, rhs)
