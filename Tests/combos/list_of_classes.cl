import "list"

class Item:
    name: str
    price: float64
    qty: int

    function total(self: *Item) -> float64:
        return self.price * self.qty

function main() -> int32:
    let cart = List[Item]()
    defer cart.free()
    cart.push(Item("pen", 1.5, 4))
    cart.push(Item("book", 12.0, 1))
    cart.push(Item("bag", 30.0, 2))
    let sum = 0.0
    for item in cart:
        sum += item.total()
    print(sum, cart[1].name, cart[2].total())
    // cart[i] returns a copy: change an element by reading, editing and storing it back
    let first = cart[0]
    first.qty = 10
    cart[0] = first
    print(cart[0].qty, cart[0].total())
    return 0

// expect:
// 78.0 book 60.0
// 10 15.0
