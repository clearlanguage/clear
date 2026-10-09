// N15 / H1: sizeof of array, optional and tuple types is the room one takes in memory (the array stride)
function size_of[T](x: T) -> uint64:
    return sizeof T

class Pair:
    a: int64
    b: bool

function main() -> int32:
    let v: ?int64 = 3 as int64
    let s: ?String = none
    print(sizeof [3; int8], sizeof [4; int32], sizeof [2; ?int64])
    print(sizeof ?int64, size_of(v), sizeof ?String, size_of(s), sizeof ?int, sizeof ?float64)
    print(sizeof (bool, int64), sizeof Pair, sizeof *Pair, sizeof String, sizeof int16)
    let arr: [5; uint16] = {}
    print(sizeof arr, size_of(arr))
    return 0

// expect:
// 3 16 32
// 16 16 32 32 16 16
// 16 16 8 24 2
// 10 10
