class Response:
    """A response is a type that can be converted to string"""

    def __init__(self, value: str | int | None):
        self.value = value

    def to_str(self) -> str:
        if self.value is None:
            return ""
        else:
            return str(self.value)
