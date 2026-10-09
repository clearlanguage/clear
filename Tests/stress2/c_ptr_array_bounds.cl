// indexing through a *[N; T] is bounds-checked like the array itself
function poke(a: *[4; int], i: int):
    a[i] = 1

function main() -> int32:
    let a: [4; int] = {}
    poke(&a, 3)
    print(a[3])
    poke(&a, 4)
    return 0

// flags: --checks
// expect:
// 1
// expect-exit: -6
// expect-stderr: index out of range for an array of 4
