import unittest

from lyadasm.cli.main import main


class TestMain(unittest.TestCase):
    def test_it_should_fail_with_invalid_arch(self) -> None:
        self.assertEqual(main(["--arch", "invalid", "file"]), -1)

    def test_it_should_fail_with_invalid_log_level(self) -> None:
        self.assertEqual(main(["--loglevel", "invalid", "file"]), -1)
