// a function returning Generator[T] produces values with yield
function count_up(start: int, stop: int) -> Generator[int]:
    let i = start
    while i < stop:
        yield i
        i += 1

function fibonacci() -> Generator[int64]:   // can be endless: values are made on demand
    let a: int64 = 0
    let b: int64 = 1
    while true:
        yield a
        a, b = b, a + b

function evens(limit: int) -> Generator[int]:
    for n in count_up(0, limit):            // generators can use generators
        if n % 2 == 0:
            yield n

function main() -> int32:
    for i in count_up(3, 6):
        print(i)

    for f in fibonacci():
        if f > 30:
            break                           // the generator is cleaned up
        print("fib", f)

    for e in evens(7):
        print("even", e)

    let g = count_up(0, 2)                  // driving one by hand
    print(g.advance(), g.value(), g.advance(), g.value(), g.advance())
    g.free()
    return 0

// expect:
// 3
// 4
// 5
// fib 0
// fib 1
// fib 1
// fib 2
// fib 3
// fib 5
// fib 8
// fib 13
// fib 21
// even 0
// even 2
// even 4
// even 6
// true 0 true 1 false
