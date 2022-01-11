from typing import Dict, List

from ..context import Context, Line, Middleware
from ..file import Binary


class CommentMiddleware(Middleware):
    def __init__(
        self,
        comments: Dict[int, List[Line]] = None,
        post_comments: Dict[int, Line] = None,
        prefix: str = "; ",
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
            line.text = (
                f"{line.text} {self.prefix}"
                f"{self.post_comments[ctx.address].fmt()}"
            )

    def on_next(self, ctx: Context, file: Binary) -> None:
        if ctx.address in self.comments:
            for line in self.comments[ctx.address]:
                line.text = f"{self.prefix}{line.text}"
                ctx.add_line(line)
