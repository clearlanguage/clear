import "memory"
import "string"

function main() -> int32:
    let buffer = allocate[float64](4)
    defer release(buffer)

    for i in 0..4:
        buffer[i] = i as float64 * 1.5

    let other = allocate[float64](4)
    copy(other, buffer, 4)
    print(other[3], buffer[0])
    release(other)

    print(length("hello"), equals("abc", "abc"), equals("abc", "abd"))
    print(starts_with("clear language", "clear"), contains("clear language", "lang"), to_int("42") + 1)
    return 0

// expect:
// 4.5 0.0
// 5 true false
// true true 43
