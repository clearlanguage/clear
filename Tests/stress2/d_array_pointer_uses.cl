// a *[N; T] parameter is sliced, iterated and searched like the array it points at

function show(a: *[4; int]):
    let part = a[1:3]
    print(len(part), part[0], part[1])
    let total = 0
    for x in a:
        total += x
    print(total, 30 in a, 99 in a, 99 not in a)

function main() -> int32:
    let xs: [4; int] = {10, 20, 30, 40}
    show(&xs)
    return 0

// expect:
// 2 20 30
// 100 true false true
