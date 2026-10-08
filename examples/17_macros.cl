macro square(x):                 // one expression: the macro has a value
    x * x

macro swap(a, b):                // statements: pasted in where it is used
    let tmp = a
    a = b
    b = tmp

macro repeat(n, action):
    for i in 0..n:
        action

macro unless(condition, action):
    if not condition:
        action

function main() -> int32:
    print(square!(7), square!(1.5))

    let x = 1
    let y = 2
    let tmp = 99                 // the macro's own `tmp` never touches this one
    swap!(x, y)
    print(x, y, tmp)

    let count = 0
    repeat!(3, count += 5)
    print(count)

    unless!(count > 100, print("count is small"))
    return 0

// expect:
// 49 2.25
// 2 1 99
// 15
// count is small
