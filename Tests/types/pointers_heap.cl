declare printf(fmt: *int8, args: ...) -> int32
declare malloc(size: uint64) -> *int8
declare free(p: *int8)

function main() -> int32:
    let buf = malloc(16) as *int32
    buf[0] = 5
    buf[3] = 7
    printf("%d %d\n", buf[0], buf[3])
    free(buf as *int8)
    let n: *int = null
    if n == null:
        printf("null\n")
    return 0

// expect:
// 5 7
// null
