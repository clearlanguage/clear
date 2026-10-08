class Node:
    value: int
    next: *Node

function main() -> int32:
    let n = Node(1, null)
    print(n.value)
    print(n.next.value)
    return 0

// flags: --checks
// expect:
// 1
// expect-exit: -6
