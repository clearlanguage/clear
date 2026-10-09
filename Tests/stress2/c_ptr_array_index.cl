// a *[N; T] is indexed like the array it points at, like *List[T] is (N38)
function fill(a: *[4; int]):
    for i in 0..len(a):
        a[i] = (i * 3) as int

function sum(a: *[4; int]) -> int:
    let total = 0
    for i in 0..4:
        total += a[i]
    return total

function main() -> int32:
    let a: [4; int] = {}
    fill(&a)
    let p = &a
    p[0] += 1
    print(a, sum(&a), len(p))
    return 0

// expect:
// [1, 3, 6, 9] 19 4
