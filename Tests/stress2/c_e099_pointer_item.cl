// reading a pointer out of a List[*T] copies the pointer: it does not point into the list (no E099)
class Order:
    id: int
    next: *Order

function main() -> int32:
    let a = Order(1, null)
    let b = Order(2, &a)
    let xs = List[*Order]()
    xs.push(&a)
    xs.push(&b)
    let best = xs[0]
    let after = xs[1].next
    xs.remove(0)
    xs.push(&a)
    print(best.id, after.id, len(xs))
    return 0

// expect-no-warning: E099
// expect:
// 1 1 2
