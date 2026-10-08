declare printf(fmt: *int8, args: ...) -> int32
declare malloc(size: uint64) -> *int8
declare free(p: *int8)

class P:
    x: int
    y: int
function set(v: *int):
    *v = 9
function main() -> int32:
    let p = P { 1, 2 }
    set(&p.y)
    printf("%d %d\n", p.x, p.y)
    return 0

// expect:
// 1 9
