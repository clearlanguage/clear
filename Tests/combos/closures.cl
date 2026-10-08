function compose[F, G](f: F, g: G, x: int) -> int:
    return g(f(x))

function times[F](n: int, f: F):
    for i in 0..n:
        f(i)

function main() -> int32:
    let base = 100
    let add = lambda (x: int): x + base
    let dbl = lambda (x: int): x * 2
    print(compose(add, dbl, 1), compose(dbl, add, 1))
    base = 0
    print(add(5))
    times(3, lambda (i: int): print("tick", i))
    return 0

// expect:
// 202 102
// 105
// tick 0
// tick 1
// tick 2
