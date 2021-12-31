from typing import Callable, Any
import ctypes
from .context import Context, Symbol

Operation = Callable[[Context, Any], Any]


def rel_i8_to_addr(ctx: Context, i: Any) -> int:
    return ctx.address + int(ctypes.c_int8(i).value)


def grab_label(ctx: Context, i: Any) -> Any:
    """Grab a label and add it as a symbol"""
    addr = rel_i8_to_addr(ctx, i)
    if ctx.start_address <= addr:
        ctx.add_symbol(Symbol(addr, f"label_{hex(addr)[2:]}"))
    return i
