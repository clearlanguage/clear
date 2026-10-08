macro square(x):
    x * x

macro swap(a, b):
    let tmp = a
    a = b
    b = tmp

macro repeat(n, body):
    for i in 0..n:
        body

macro unless(condition, action):
    if not condition:
        action

macro max_of(a, b):
    when a > b use a otherwise b

function main() -> int32:
    print(square!(7), square!(1.5))

    let x = 1
    let y = 2
    let tmp = 99                   // the macro's own `tmp` never touches this one
    swap!(x, y)
    print(x, y, tmp)
    swap!(x, y)                    // a second expansion in the same scope is fine
    print(x, y)

    let count = 0
    repeat!(3, count += 2)
    print(count)

    let i = 100                    // nor does the loop variable `i`
    repeat!(2, print(i))

    unless!(count > 10, print("small"))
    print(max_of!(3, square!(2)))
    return 0

// expect:
// 49 2.25
// 2 1 99
// 1 2
// 6
// 100
// 100
// small
// 4
