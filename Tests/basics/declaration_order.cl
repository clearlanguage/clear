function main() -> int32:
    print(helper(3), counter, Pair { 1, 2 }.sum())
    let n = Node { 5, null }
    print(n.value)
    return 0

function helper(x: int) -> int:
    return x * multiplier()

function multiplier() -> int:
    return counter + 1

class Pair:
    a: int
    b: int

    function sum(self: *Pair) -> int:
        return self.a + self.b + helper(0)

class Node:
    value: int
    next: *Node

let counter = 10

// expect:
// 33 10 3
// 5
