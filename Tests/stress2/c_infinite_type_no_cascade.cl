// after E104 for Node, building a Node gives no second error about its (missing) fields
class Node:
    key: int
    left: ?Node
    right: ?Node

function main() -> int32:
    let n = Node(1, none, none)
    print(n.key)
    return 0

// expect-error: E104
// expect-no-warning: E058
