import logging
from typing import IO, List, Optional
from .reset import Resettable


class Binary(Resettable):
    def __init__(
        self, data: bytes, current: int = 0, end: Optional[int] = None
    ):
        self.data = data
        self.current = current
        self.start = current
        self.end = end

    def reset(self) -> None:
        """Resets file position"""
        self.current = self.start

    def read(self, count: int) -> Optional[bytes]:
        if self.len() >= self.offset() + count:
            return self.data[self.offset() : self.offset() + count]
        else:
            return None

    def next(self) -> Optional[int]:
        """Reads next single byte value"""
        if not self.is_at_end():
            return self.data[self.offset()]
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


class Output:
    def __init__(self) -> None:
        self.lines: List[str] = []

    def begin(self) -> None:
        logging.debug("Starting output")
        self.lines = []

    def finish(self) -> None:
        self.close()

    def close(self) -> None:
        pass

    def on_line(self, line: str) -> None:
        self.lines.append(line)

    def on_lines(self, lines: List[str]) -> None:
        self.lines += lines

    def collect(self) -> List[str]:
        return self.lines


class StreamOutput(Output):
    def __init__(self, stream: IO, autoclose: bool = False) -> None:
        Output.__init__(self)
        self.stream = stream
        self.autoclose = autoclose

    def finish(self) -> None:
        logging.debug("Finishing output")
        if self.autoclose:
            self.close()

    def close(self) -> None:
        logging.debug("Closing output")
        self.stream.close()

    def on_line(self, line: str) -> None:
        self.stream.write(line)
        self.stream.write("\n")

    def on_lines(self, lines: List[str]) -> None:
        if len(lines) > 0:
            self.stream.write("\n".join(lines))
            self.stream.write("\n")


class PrintOutput(Output):
    def __init__(self) -> None:
        Output.__init__(self)

    def on_line(self, line: str) -> None:
        print(line)

    def on_lines(self, lines: List[str]) -> None:
        for line in lines:
            print(line)
