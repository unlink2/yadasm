from typing import Dict, List

from ..context import Context, Line, Middleware
from ..file import Binary


class CommentMiddleware(Middleware):
    def __init__(
        self,
        comments: Dict[int, List[Line]] = None,
        prefix: str = "; ",
        tag: str = "CommentMiddleware",
    ):
        Middleware.__init__(self, tag)
        if comments is None:
            comments = {}
        self.comments = comments
        self.prefix = prefix

    def add_comment(self, address: int, line: Line) -> "CommentMiddleware":
        if address in self.comments:
            self.comments[address].append(line)
        else:
            self.comments[address] = [line]
        return self

    def on_next(self, ctx: Context, file: Binary) -> None:
        if ctx.address in self.comments:
            for line in self.comments[ctx.address]:
                line.text = f"{self.prefix}{line.text}"
                ctx.add_line(line)
