import unittest

from lyadasm.core.archs.arch6502 import Parser6502
from lyadasm.core.context import Context
from lyadasm.core.file import Binary
from lyadasm.core.middleware.bytecollector import ByteCollectorMiddelware


class TestByteCollector(unittest.TestCase):
    def test_it_should_collect_bytes_in_range(self) -> None:
        bytemiddleware = ByteCollectorMiddelware(0x602, 0x605)
        parser = Parser6502()
        ctx = Context(0x600, middlewares=[bytemiddleware])
        result = parser.parse(
            ctx,
            Binary(bytes([0xEA, 0xEA, 0xFF, 0xFE, 0xFC, 0xEA, 0xEA])),
        )

        self.assertEqual(result, ["    nop", "    nop", "    nop", "    nop"])
        self.assertEqual(bytemiddleware.data, bytes([0xFF, 0xFE, 0xFC]))
