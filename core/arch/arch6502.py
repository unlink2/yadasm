from typing import List, Literal

from ..comparator import always_true
from ..node import Node
from ..reader import read_i8_le, read_i16_le


def read_i8_hex_node(
    prefix: str = "",
    padding: int = 2,
    mode: Literal["x"] | Literal["b"] | Literal[""] = "x",
) -> Node:
    return Node(
        read_i8_le,
        [],
        always_true,
        lambda ctx, i: f"{prefix}${i:0{padding}{mode}}",
    )


def read_i16_hex_node(
    prefix: str = "",
    padding: int = 4,
    mode: Literal["x"] | Literal["b"] | Literal[""] = "x",
) -> Node:
    return Node(
        read_i16_le,
        [],
        always_true,
        lambda ctx, i: f"{prefix}${i:0{padding}{mode}}",
    )


arch6502: List[Node] = [
    # lda
    # ==========
    # lda immediate
    Node(
        read_i8_le,
        [],
        lambda ctx, i: i == 0xA9,
        lambda ctx, i: "lda ",
        [read_i8_hex_node("#")],
    ),
    # sta
    # =========
    # sta absolute
    Node(
        read_i8_le,
        [],
        lambda ctx, i: i == 0x8D,
        lambda ctx, i: "sta ",
        [read_i16_hex_node()],
    ),
]
