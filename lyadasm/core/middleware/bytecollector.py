from typing import IO
from ..context import Context, Middleware
from ..file import Binary


class ByteCollectorMiddelware(Middleware):
    """
    This middleware collects bytes from the stream
    before the noe parser executes and stores them.
    It is intended as a base class and will not output any
    useful information on its own
    """

    def __init__(
        self, start: int, end: int, tag: str = "ByteCollector"
    ) -> None:
        Middleware.__init__(self, tag)
        self.start = start
        self.end = end
        self.data = bytearray()

    def on_next(self, ctx: Context, file: Binary) -> None:
        """Emmited before the next node is parsed"""
        while (
            ctx.address >= self.start
            and ctx.address < self.end
            and not file.is_at_end()
        ):
            if not ctx.lines_disabled():
                next_byte = file.next()
                if next_byte is not None:
                    self.data.append(next_byte)
            file.advance(1)
            ctx.advance(1)

    def on_output(self, _ctx: Context, stream: IO) -> None:
        stream.write(self.data)
