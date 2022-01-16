import unittest
from io import BytesIO, StringIO

from lyadasm.core.archs.arch6502 import Parser6502
from lyadasm.core.context import Context, Symbol
from lyadasm.core.file import Binary, StreamOutput
from lyadasm.core.middleware.bytecollector import ByteCollectorMiddelware
from lyadasm.core.parser import ParsersExhaustedException


class TestParser(unittest.TestCase):
    def test_it_should_parse_valid_code(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0x01,
                        0x8D,
                        0x00,
                        0x02,
                        0xA9,
                        0x05,
                        0x8D,
                        0x01,
                        0x02,
                        0xA9,
                        0x08,
                        0x8D,
                        0x02,
                        0x02,
                    ]
                )
            ),
        )

        # data was 15 bytes long!
        self.assertEqual(ctx.address, 0x60F)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    lda #$01",
                    "    sta $0200",
                    "    lda #$05",
                    "    sta $0201",
                    "    lda #$08",
                    "    sta $0202",
                ],
            )

    def test_it_should_parse_valid_code_single_pass(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        result = parser.parse_single(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0x01,
                        0x8D,
                        0x00,
                        0x02,
                        0xA9,
                        0x05,
                        0x8D,
                        0x01,
                        0x02,
                        0xA9,
                        0x08,
                        0x8D,
                        0x02,
                        0x02,
                        0x4C,
                        0x00,
                        0x06,
                    ]
                )
            ),
        )

        # data was 15 bytes long!
        self.assertEqual(ctx.address, 0x612)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "label_600",
                    "    lda #$01",
                    "    sta $0200",
                    "    lda #$05",
                    "    sta $0201",
                    "    lda #$08",
                    "    sta $0202",
                    "    jmp label_600",
                ],
            )

    def test_it_should_parse_valid_code_without_map(self) -> None:
        parser = Parser6502()
        parser.should_build_lookup = False
        ctx = Context(0x600)
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0x01,
                        0x8D,
                        0x00,
                        0x02,
                        0xA9,
                        0x05,
                        0x8D,
                        0x01,
                        0x02,
                        0xA9,
                        0x08,
                        0x8D,
                        0x02,
                        0x02,
                    ]
                )
            ),
        )

        # data was 15 bytes long!
        self.assertEqual(ctx.address, 0x60F)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    lda #$01",
                    "    sta $0200",
                    "    lda #$05",
                    "    sta $0201",
                    "    lda #$08",
                    "    sta $0202",
                ],
            )

    def test_it_should_fail_when_all_nodes_are_exhausted(self) -> None:
        parser = Parser6502()
        ctx = Context(0x600)
        self.assertRaises(
            ParsersExhaustedException,
            lambda: parser.parse(
                ctx,
                Binary(
                    bytes(
                        [
                            0xA9,
                            0x01,
                            0x8D,
                            0x00,
                            0x02,
                            0xA9,
                            0x05,
                            0x8D,
                            0x01,
                            0x02,
                            0xA9,
                            0x08,
                            0xFF,
                            0x02,
                            0x02,
                        ]
                    )
                ),
            ),
        )

        # data was 15 bytes long, but the error was raised after 12!
        self.assertEqual(ctx.address, 0x60C)

    def test_it_should_call_output_on_parser_and_middleware(self) -> None:
        strio = StringIO()
        bio = BytesIO()

        bytemiddleware = ByteCollectorMiddelware(0x602, 0x605)
        parser = Parser6502()
        ctx = Context(
            0x600,
            middlewares=[bytemiddleware],
            middleware_streams={"ByteCollector": bio},
            output=StreamOutput(strio),
        )
        parser.parse(
            ctx,
            Binary(bytes([0xEA, 0xEA, 0x31, 0x32, 0x33, 0xEA, 0xEA])),
        )

        strio.seek(0)
        bio.seek(0)
        self.assertEqual(
            strio.readlines(),
            ["    nop\n", "    nop\n", "    nop\n", "    nop\n"],
        )
        self.assertEqual(bio.read(), b"123")

    def test_it_should_parse_unbuffered_output(self) -> None:
        strio = StringIO()
        parser = Parser6502()
        ctx = Context(
            0x600,
            end_address=0x610,
            output=StreamOutput(strio),
            unbuffered_lines=True,
        )
        ctx.add_symbol_no_emit(Symbol(0x600, "start:"))
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0x01,
                        0x8D,
                        0x00,
                        0x02,
                        0xA9,
                        0x05,
                        0x8D,
                        0x01,
                        0x02,
                        0xA9,
                        0x08,
                        0x8D,
                        0x02,
                        0x02,
                        0x4C,
                        0x60,
                        0x10,
                        0x4C,
                        0x00,
                        0x06,
                        0x4C,
                        0x00,
                        0x05,
                        0x4C,
                        0x02,
                        0x06,
                    ]
                )
            ),
        )

        # data was 15 bytes long!
        self.assertEqual(ctx.address, 0x61B)
        self.assertNotEqual(result, None)
        strio.seek(0)
        if result is not None:
            self.assertEqual(result, [])
            self.assertEqual(
                strio.readlines(),
                [
                    "start:\n",
                    "    lda #$01\n",
                    "label_602\n",
                    "    sta $0200\n",
                    "    lda #$05\n",
                    "    sta $0201\n",
                    "    lda #$08\n",
                    "    sta $0202\n",
                    "    jmp label_1060\n",
                    "    jmp start:\n",
                    "    jmp label_500\n",
                    "    jmp label_602\n",
                ],
            )
