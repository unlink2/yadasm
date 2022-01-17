import logging
from typing import Dict, List

from ..context import Context, Line, Middleware
from ..file import Binary


class CommentMiddleware(Middleware):
    def __init__(
        self,
        comments: Dict[int, List[Line]] = None,
        post_comments: Dict[int, Line] = None,
        prefix: str = "; ",
        line_comment_prefix: str = " ; ",
        comment_addr: bool = False,
        align: int = 20,
        tag: str = "CommentMiddleware",
    ):
        Middleware.__init__(self, tag)
        if comments is None:
            comments = {}
        if post_comments is None:
            post_comments = {}
        self.comments = comments
        self.post_comments = post_comments
        self.prefix = prefix
        self.line_comment_prefix = line_comment_prefix
        self.comment_addr = comment_addr
        self.align = align

    def add_comment(self, address: int, line: Line) -> "CommentMiddleware":
        if address in self.comments:
            self.comments[address].append(line)
        else:
            self.comments[address] = [line]
        return self

    def add_post_comment(
        self, address: int, line: Line
    ) -> "CommentMiddleware":
        self.post_comments[address] = line
        return self

    def on_line(self, ctx: "Context", line: Line) -> None:
        if ctx.address in self.post_comments:
            logging.debug(
                "Adding line comment at %s='%s'",
                hex(ctx.address),
                self.post_comments[ctx.address].text,
            )
            line.text = (
                f"{line.text: <{self.align}}{self.line_comment_prefix}"
                f"{self.post_comments[ctx.address].fmt()}"
            )
        elif self.comment_addr:
            logging.debug(
                "Adding Address-line comment at %s",
                hex(ctx.address),
            )
            line.text = (
                f"{line.text : <{self.align}}"
                f"{self.line_comment_prefix}{hex(ctx.address)}"
            )

    def on_next(self, ctx: Context, file: Binary) -> None:
        if (
            ctx.address in self.comments
            and ctx.is_in_address_range(ctx.address)
            and not ctx.disable_lines
        ):
            for line in self.comments[ctx.address]:
                logging.debug(
                    "Adding comment at %s='%s'", hex(ctx.address), line.text
                )
                line.text = f"{self.prefix}{line.text}"
                ctx.add_line_no_emit(line)
