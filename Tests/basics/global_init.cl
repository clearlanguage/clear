declare abs(x: int32) -> int32

let base = abs(-20)
let doubled = base * 2
let label = when base > 10 use "big" otherwise "small"
const ANSWER = 42

function main() -> int32:
    print(base, doubled, label, ANSWER)
    base += 1
    print(base)
    return 0

// expect:
// 20 40 big 42
// 21
