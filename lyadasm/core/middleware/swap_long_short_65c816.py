import logging
from typing import List

from ..context import Context, Line, Middleware, Symbol
from ..file import Binary


class LongShort65C816Middleware(Middleware):
    def __init__(
        self,
        ascmp: str = "sep #$20",
        alcmp: str = "rep #$20",
        asstr: str = "!as",
        alstr: str = "!al",
        exceptions: List[int] = None,
        alat: List[int] = None,
        asat: List[int] = None,
        order: int = -1,
        tag: str = "LongShort65C816",
    ):
        """
        This middleware should preferably be called before any
        other middleware that might modify the line!
        """
        Middleware.__init__(self, tag)
        if alat is None:
            alat = []
        if asat is None:
            asat = []
        if exceptions is None:
            exceptions = []

        self.ascmp = ascmp
        self.alcmp = alcmp
        self.asstr = asstr
        self.alstr = alstr
        self.alat = alat
        self.asat = asat
        self.exceptions = exceptions
        self.order = order

    def on_line(self, ctx: Context, line: Line) -> None:
        if line.text == self.alcmp and ctx.address not in self.exceptions:
            self._set_al(ctx)
        elif line.text == self.ascmp and ctx.address not in self.exceptions:
            self._set_as(ctx)

    def on_next(self, ctx: Context, file: Binary) -> None:
        if ctx.address in self.asat:
            self._set_as(ctx)
        elif ctx.address in self.alat:
            self._set_al(ctx)

    def _set_as(self, ctx: Context) -> None:
        logging.debug("Adding as at %s", hex(ctx.address))
        ctx.set_flag("read_idyn_le", 1)
        ctx.add_symbol_no_emit(
            Symbol(ctx.address, self.asstr, postfix="", order=self.order)
        )

    def _set_al(self, ctx: Context) -> None:
        logging.debug("Adding al at %s", hex(ctx.address))
        ctx.set_flag("read_idyn_le", 2)
        ctx.add_symbol_no_emit(
            Symbol(ctx.address, self.alstr, postfix="", order=self.order)
        )
