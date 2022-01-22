import unittest

from lyadasm.core.file import Binary


class TestFile(unittest.TestCase):
    def test_it_should_read(self) -> None:
        file = Binary(bytes([0, 1, 2, 3, 4]))

        self.assertEqual(file.read(2), bytes([0, 1]))
        self.assertEqual(file.offset(), 0)
        self.assertFalse(file.is_at_end())
        file.advance(2)

        self.assertEqual(file.read(2), bytes([2, 3]))
        self.assertEqual(file.offset(), 2)
        self.assertFalse(file.is_at_end())
        file.advance(2)

        self.assertEqual(file.read(2), None)
        self.assertEqual(file.offset(), 4)
        self.assertFalse(file.is_at_end())

        self.assertEqual(file.read(1), bytes([4]))
        self.assertEqual(file.next(), 4)
        file.advance(1)
        self.assertEqual(file.offset(), 5)
        self.assertTrue(file.is_at_end())

        self.assertEqual(file.read(1), None)
        self.assertEqual(file.next(), None)

        self.assertFalse(file.is_empty())

    def test_it_should_rewind(self) -> None:
        file = Binary(bytes([0, 1, 2]))
        file.advance(2)
        self.assertEqual(file.offset(), 2)
        file.rewind(1)
        self.assertEqual(file.offset(), 1)
        file.rewind(4)
        self.assertEqual(file.offset(), 0)
        file.advance(4)
        self.assertEqual(file.offset(), 4)
        file.reset()
        self.assertEqual(file.offset(), 0)

        self.assertFalse(file.is_empty())

    def test_it_should_be_empty(self) -> None:
        file = Binary(bytes([]))
        self.assertTrue(file.is_empty())
