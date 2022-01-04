from typing import Optional


class Binary:
    def __init__(self, data: bytes, current: int = 0, end: Optional[int] = None):
        self.data = data
        self.current = current
        self.end = end

    def read(self, count: int) -> Optional[bytes]:
        if self.len() >= self.offset() + count:
            return self.data[self.offset() : self.offset() + count]
        else:
            return None

    def advance(self, count: int) -> None:
        self.current += count

    def rewind(self, count: int) -> None:
        self.current -= count
        self.current = max(self.current, 0)

    def is_at_end(self) -> bool:
        return self.offset() >= self.len()

    def offset(self) -> int:
        return self.current

    def len(self) -> int:
        if self.end is None:
            return len(self.data)
        else:
            return min(len(self.data), self.end)

    def is_empty(self) -> bool:
        pass
