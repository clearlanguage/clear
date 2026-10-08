function work(n: int) -> int:
    defer print("leave", n)
    if n > 1:
        defer print("early", n)
        return n * 2
    print("body", n)
    return n

function main() -> int32:
    defer print("main done")
    for i in 0..3:
        defer print("iter", i)
        if i == 1:
            continue
        print("work", work(i))
    return 0

// expect:
// body 0
// leave 0
// work 0
// iter 0
// iter 1
// early 2
// leave 2
// work 4
// iter 2
// main done
