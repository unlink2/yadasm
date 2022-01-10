import unittest

from lyadasm.core.archs.arch6502bytes import ByteParser6502
from lyadasm import Context, Binary


class TestArch6502Bytes(unittest.TestCase):
    def test_it_should_parse_byte_structures_fully(self) -> None:
        parser = ByteParser6502(
            nodes=[
                ByteParser6502.make_i24_node(
                    [
                        ByteParser6502.make_i16_node(
                            [ByteParser6502.make_i8_node([], prefix="\n")],
                            prefix="\n",
                        )
                    ]
                )
            ],
            length=12,
        )

        result = parser.parse(
            Context(),
            Binary(
                bytes(
                    [
                        0x11,
                        0x11,
                        0x11,
                        0x22,
                        0x22,
                        0x33,
                        0x11,
                        0x11,
                        0x11,
                        0x22,
                        0x22,
                        0x33,
                    ]
                )
            ),
        )

        self.assertEqual(
            result,
            [
                "!le24 $111111\n!word $2222\n!byte $33",
                "!le24 $111111\n!word $2222\n!byte $33",
            ],
        )

    def test_it_should_parse_byte_structures_partial(self) -> None:
        parser = ByteParser6502(
            nodes=[
                ByteParser6502.make_i24_node(
                    [
                        ByteParser6502.make_i16_node(
                            [ByteParser6502.make_i8_node([], prefix="\n")],
                            prefix="\n",
                        )
                    ]
                )
            ],
            length=1,
        )

        result = parser.parse(
            Context(),
            Binary(
                bytes(
                    [
                        0x11,
                        0x11,
                        0x11,
                        0x22,
                        0x22,
                        0x33,
                        0x11,
                        0x11,
                        0x11,
                        0x22,
                        0x22,
                        0x33,
                    ]
                )
            ),
        )

        self.assertEqual(
            result,
            [
                "!le24 $111111\n!word $2222\n!byte $33",
            ],
        )
