from typing import Callable, Any
from .context import Context, Symbol

Operation = Callable[[Context, Any], Any]


def grab_label(ctx: Context, i: Any) -> Any:
    """Grab a label and add it as a symbol"""
    if ctx.is_in_address_range(i):
        ctx.add_symbol(Symbol(i, f"label_{hex(i)[2:]}"))
    return i
