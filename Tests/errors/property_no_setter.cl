class Box:
    w: int

    property double(self: *Box) -> int:
        return self.w * 2

function main() -> int32:
    let b = Box(1)
    b.double = 4
    return 0

// expect-error
