import unittest

from core.comparator import always_true, always_false
from core.context import Context


class TestComparator(unittest.TestCase):
    def test_it_should_always_be_true(self) -> None:
        self.assertTrue(always_true(Context(), 0))

    def test_it_should_always_be_false(self) -> None:
        self.assertFalse(always_false(Context(), 1))
