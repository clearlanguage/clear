function main() -> int32:
    let total = 0
    for i in 0..10:
        total += i
    print(total)
    for i in 1..=3:
        print("i", i)
    let n = 4
    for i in n-2..n:
        print("range", i)
    let arr: [4; float64] = {1.5, 2.5, 3.0, 4.0}
    let sum = 0.0
    for x in arr:
        sum += x
    print(sum)
    for i in 0..10:
        if i % 2 == 0:
            continue
        if i > 7:
            break
        print("odd", i)
    let empty = 0
    for i in 5..2:
        empty += 1
    print(empty)
    let k: uint8 = 0
    for j in 250..=255:
        k += 1
    print(k)
    return 0

// expect:
// 45
// i 1
// i 2
// i 3
// range 2
// range 3
// 11.0
// odd 1
// odd 3
// odd 5
// odd 7
// 0
// 6
