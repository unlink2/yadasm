from enum import Enum
from typing import Literal


class Endianess(Enum):
    LITTLE = 0
    BIG = 1

    def to_literal(self) -> Literal["little"] | Literal["big"]:
        if self == Endianess.LITTLE:
            return "little"
        else:
            return "big"


class IntFmt(Enum):
    DECIMAL = 0
    BINARY = 1
    OCTAL = 2
    HEX = 3

    def to_literal(
        self,
    ) -> Literal["x"] | Literal["b"] | Literal["o"] | Literal[""]:
        if self == IntFmt.BINARY:
            return "b"
        elif self == IntFmt.OCTAL:
            return "o"
        elif self == IntFmt.HEX:
            return "x"
        else:
            return ""


class FloatFmt(Enum):
    FULL = 0
    SCIENTIFIC = 1
