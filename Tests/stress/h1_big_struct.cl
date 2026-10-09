// stress test H1_big_struct
// expect:
// 1 2


class Big:
    data: [512; int64]
    tail: int
    other: int
function make(t: int) -> Big:
    let b = Big { }
    b.tail = t
    b.other = 2
    return b
function main() -> int32:
    let b = make(1)
    print(b.tail, b.other)
    return 0
