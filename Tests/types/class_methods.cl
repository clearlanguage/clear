declare printf(fmt: *int8, args: ...) -> int32

class Vec:
    x: int
    y: int

    function sum(self: *Vec) -> int:
        return self.x + self.y

    function scale(self: *Vec, k: int):
        self.x *= k
        self.y *= k

function main() -> int32:
    let v = Vec { 3, 4 }
    v.scale(2)
    printf("%d %d %d\n", v.x, v.y, v.sum())
    return 0

// expect:
// 6 8 14
