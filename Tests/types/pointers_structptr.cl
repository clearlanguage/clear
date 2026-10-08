declare printf(fmt: *int8, args: ...) -> int32
declare malloc(size: uint64) -> *int8
declare free(p: *int8)

class Node:
    value: int
    next: *Node
function main() -> int32:
    let b = Node { 2, null }
    let a = Node { 1, &b }
    printf("%d\n", a.next.value)
    return 0

// expect:
// 2
