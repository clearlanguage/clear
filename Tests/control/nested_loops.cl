declare printf(fmt: *int8, args: ...) -> int32

function main() -> int32:
    let i = 0
    let count = 0
    while i < 5:
        let j = 0
        while j < 5:
            if j == 3:
                break
            count += 1
            j++
        i++
    printf("%d\n", count)
    return 0

// expect:
// 15
