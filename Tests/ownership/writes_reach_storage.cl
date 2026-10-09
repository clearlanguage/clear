// writes through references, fields, pointers and arrays change the real value
import "list"
import "map"

class Item:
    qty: int

class Box:
    items: List[Item]
    inner: Item

    function first(self) -> *Item:
        return &self.items[0]

function main() -> int32:
    let cart = List[Item]()
    cart.push(Item(1))
    cart[0].qty = 10
    cart[0].qty += 1

    let box = Box(List[Item](), Item(2))
    box.items.push(Item(3))
    box.items[0].qty = 30
    box.inner.qty = 20
    box.first().qty += 1

    let p = &box.inner
    p.qty += 1

    let arr: [2; Item] = {Item(1), Item(2)}
    arr[1].qty = 7

    let m = Map[str, int]()
    m["a"] = 1
    m["a"] += 1
    print(cart[0].qty, box.items[0].qty, box.inner.qty, arr[1].qty, m["a"])
    return 0

// expect:
// 11 31 21 7 2
