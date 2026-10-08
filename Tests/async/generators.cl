function count_up(start: int, stop: int) -> Generator[int]:
    let i = start
    while i < stop:
        yield i
        i += 1

function fibonacci() -> Generator[int64]:
    let a: int64 = 0
    let b: int64 = 1
    while true:
        yield a
        let next = a + b
        a = b
        b = next

function evens(limit: int) -> Generator[int]:
    for n in count_up(0, limit):
        if n % 2 == 0:
            yield n

function words() -> Generator[str]:
    yield "read"
    yield "like"
    yield "python"
    return
    yield "never"

function main() -> int32:
    for i in count_up(3, 7):
        print(i)

    let total: int64 = 0
    for f in fibonacci():
        if f > 100:
            break
        total += f
    print(total)

    for e in evens(10):
        print("even", e)

    for w in words():
        print(w)

    // driving a generator by hand
    let g = count_up(0, 2)
    print(g.advance(), g.value(), g.advance(), g.value(), g.advance(), g.done())
    g.free()
    return 0

// expect:
// 3
// 4
// 5
// 6
// 232
// even 0
// even 2
// even 4
// even 6
// even 8
// read
// like
// python
// true 0 true 1 false true
