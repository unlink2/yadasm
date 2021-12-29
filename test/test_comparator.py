import unittest

from core.comparator import AlwaysTrue, AlwaysFalse, Eq, NotEq, And, Or


class TestComparator(unittest.TestCase):
    def test_it_should_always_be_true(self) -> None:
        self.assertTrue(AlwaysTrue().compare(0, 1))

    def test_it_should_always_be_false(self) -> None:
        self.assertFalse(AlwaysFalse().compare(1, 1))

    def test_it_should_eq_success(self) -> None:
        self.assertTrue(Eq().compare(1, 1))

    def test_it_should_eq_fail(self) -> None:
        self.assertFalse(Eq().compare(0, 1))

    def test_it_should_not_eq_success(self) -> None:
        self.assertTrue(NotEq().compare(1, 0))

    def test_it_should_not_eq_fail(self) -> None:
        self.assertFalse(NotEq().compare(1, 1))

    def test_it_should_and_success(self) -> None:
        self.assertTrue(And(AlwaysTrue(), AlwaysTrue()).compare(1, 1))

    def test_it_should_and_fail(self) -> None:
        self.assertFalse(And(AlwaysFalse(), AlwaysTrue()).compare(1, 1))

    def test_it_should_or_success(self) -> None:
        self.assertTrue(Or(AlwaysTrue(), AlwaysFalse()).compare(1, 1))

    def test_it_should_or_fail(self) -> None:
        self.assertFalse(Or(AlwaysFalse(), AlwaysFalse()).compare(1, 1))
