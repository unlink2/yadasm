from ..context import Context, Middleware
from ..file import Binary


class ByteCollectorMiddelware(Middleware):
    """
    This middleware collects bytes from the stream
    before the noe parser executes and stores them.
    It is intended as a base class and will not output any
    useful information on its own
    """

    def __init__(self, start: int, end: int) -> None:
        self.start = start
        self.end = end
        self.data = bytes()

    def on_next(self, ctx: Context, file: Binary) -> None:
        """Emmited before the next node is parsed"""
        while (
            ctx.address >= self.start
            and ctx.address < self.end
            and not file.is_at_end()
        ):
            next_byte = file.read(1)
            if next_byte is not None:
                self.data += next_byte
                file.advance(1)
                ctx.advance(1)
