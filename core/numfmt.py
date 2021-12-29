from enum import Enum


class Endianess(Enum):
    LITTLE = 0
    BIG = 1


class IntFmt(Enum):
    DECIMAL = 0
    BINARY = 1
    OCTAL = 2
    HEX = 3


class FloatFmt(Enum):
    FULL = 0
    SCIENTIFIC = 1
