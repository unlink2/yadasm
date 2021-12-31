from typing import Callable, Any
from .context import Context, Symbol

Operation = Callable[[Context, Any], Any]


def grab_label(ctx: Context, i: Any) -> Any:
    """Grab a label and add it as a symbol"""
    if ctx.start_address <= i:
        ctx.add_symbol(Symbol(i, f"label_{hex(i)[2:]}"))
    return i
