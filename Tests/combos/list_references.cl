import "list"

class Item:
    name: str
    price: float64
    qty: int

    function total(self) -> float64:
        return self.price * self.qty

function main() -> int32:
    let cart = List[Item]()
    defer cart.free()
    cart.push(Item("pen", 1.5, 4))
    cart.push(Item("book", 12.0, 1))

    cart[0].qty = 10                 // changes the item in the list
    cart[1].qty += 2
    print(cart[0].qty, cart[1].qty, cart[0].total())

    for item in cart:                // item is the element itself
        item.price *= 2.0
    print(cart[0].price, cart[1].price)

    let copy = cart[0]               // `let` still makes a copy
    copy.qty = 99
    print(cart[0].qty, copy.qty)

    cart[1] = Item("bag", 30.0, 1)   // replace an element
    print(cart[1].name, len(cart))

    let numbers = List[int]()
    defer numbers.free()
    numbers.push(1)
    numbers.push(2)
    for n in numbers:                // numbers are copied, as in Python
        n += 100
    numbers[0] *= 7
    print(numbers[0], numbers[1])

    let shelf: [2; Item] = {Item("a", 1.0, 1), Item("b", 2.0, 2)}
    for item in shelf:               // arrays of objects work the same way
        item.qty = 0
    print(shelf[0].qty, shelf[1].qty)
    return 0

// expect:
// 10 3 15.0
// 3.0 24.0
// 10 99
// bag 2
// 7 2
// 0 0
