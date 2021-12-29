import unittest

from core.comparator import AlwaysTrue, AlwaysFalse


class TestComparator(unittest.TestCase):
    def test_it_should_always_be_true(self) -> None:
        self.assertTrue(AlwaysTrue().compare(0, 1))

    def test_it_should_always_be_false(self) -> None:
        self.assertFalse(AlwaysFalse().compare(1, 1))
