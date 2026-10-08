class Bad:
    x: int

    operator plus(self, other: Bad) -> Bad:
        return other

function main() -> int32:
    return 0

// expect-error
