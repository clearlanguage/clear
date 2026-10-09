// stress test H2_recursive
// expect:
// 1


class Node:
    children: List[Node]
function main() -> int32:
    let a = Node(List[Node]())
    a.children.push(Node(List[Node]()))
    let b = a
    print(len(b.children))
    return 0
