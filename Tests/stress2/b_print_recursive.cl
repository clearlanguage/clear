// N7: print of a type that contains itself (through a List, Map, enum or variant)
class Node:
    v: int
    kids: List[Node]

enum J:
    A
    B(items: List[J])

variant V:
    int
    List[V]

class Tree:
    name: String
    children: Map[String, Tree]

function main() -> int32:
    let n = Node(1, List[Node]())
    print(n)
    n.kids.push(Node(2, List[Node]()))
    n.kids.push(Node(3, List[Node]()))
    n.kids[0].kids.push(Node(4, List[Node]()))
    print(n)

    let items = List[J]()
    items.push(J.A)
    let inner = List[J]()
    inner.push(J.A)
    items.push(J.B(inner))
    let j = J.B(items)
    print(j)

    let vs = List[V]()
    vs.push(1)
    let deeper = List[V]()
    deeper.push(2)
    vs.push(deeper)
    let v: V = vs
    print(v)

    let t = Tree(String("root"), Map[String, Tree]())
    t.children[String("leaf")] = Tree(String("leaf"), Map[String, Tree]())
    print(t)
    let maybe: ?Node = Node(5, List[Node]())
    print(maybe, (n.v, maybe))
    return 0

// expect:
// Node(v=1, kids=[])
// Node(v=1, kids=[Node(v=2, kids=[Node(v=4, kids=[])]), Node(v=3, kids=[])])
// J.B(items=[J.A, J.B(items=[J.A])])
// [1, [2]]
// Tree(name=root, children={leaf: Tree(name=leaf, children={})})
// Node(v=5, kids=[]) (1, Node(v=5, kids=[]))
