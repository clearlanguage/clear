class Bad:
    x: int

    function __add__(self, other: Bad) -> Bad:
        return other

function main() -> int32:
    return 0

// expect-error
