import unittest

from lyadasm.core.archs.arch6502 import Parser6502
from lyadasm.core.middleware.comment import CommentMiddleware
from lyadasm import Context, Binary, Line


class TestCommentMiddleware(unittest.TestCase):
    def test_it_should_add_comments(self) -> None:
        parser = Parser6502()
        ctx = Context(
            0x600,
            middlewares=[
                CommentMiddleware()
                .add_comment(0x600, Line("first"))
                .add_comment(0x606, Line("second"))
                .add_comment(0x606, Line("third"))
                .add_post_comment(0x609, Line("post"))
            ],
        )
        result = parser.parse(
            ctx,
            Binary(
                bytes(
                    [
                        0xA9,
                        0xAB,
                        0xA5,
                        0xAB,
                        0xB5,
                        0xAB,
                        0xAD,
                        0x00,
                        0x02,
                        0xBD,
                        0x00,
                        0x02,
                        0xB9,
                        0x00,
                        0x02,
                        0xA1,
                        0xAB,
                        0xB1,
                        0xAB,
                    ]
                )
            ),
        )
        self.assertEqual(ctx.address, 0x613)
        self.assertNotEqual(result, None)
        if result is not None:
            self.assertEqual(
                result,
                [
                    "    ; first",
                    "    lda #$ab",
                    "    lda $ab",
                    "    lda $ab, x",
                    "    ; second",
                    "    ; third",
                    "    lda $0200",
                    "    lda $0200, x ; post",
                    "    lda $0200, y",
                    "    lda ($ab, x)",
                    "    lda ($ab), y",
                ],
            )
