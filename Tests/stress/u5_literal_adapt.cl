// stress test U5_literal_adapt
// expect:
// 97
// 2048.0
// 1099511627776


function main() -> int32:
    let a: int8 = 65
    let b: int8 = a + 32
    print(b)
    let f: float32 = 1024.0
    let g: float32 = f * 2.0
    print(g)
    let big: int64 = 1 << 40
    print(big)
    return 0
