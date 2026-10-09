import "list"
import "map"

class Item:
    name: str
    qty: int

function main() -> int32:
    let numbers = List[int]()             // cleaned up automatically when main ends
    for i in 0..5:
        numbers.push(i * i)
    numbers[0] = 100
    print(len(numbers), numbers[0], numbers.last(), numbers.contains(16))
    for n in numbers:
        print("item", n)
    print(numbers.pop(), len(numbers))

    let ages = Map[str, int]()
    ages["ada"] = 36
    ages["alan"] = 41
    ages["ada"] += 1
    print(len(ages), ages["ada"], "alan" in ages, "bob" in ages)
    print(ages.get("bob"), ages.get_or("bob", 0))
    ages.remove("alan")

    for name in ages:                  // keys
        print(name, ages[name])

    // list[i] is the item itself, so it can be changed in place
    let cart = List[Item]()
    cart.push(Item("pen", 1))
    cart.push(Item("book", 2))
    cart[0].qty = 10
    for item in cart:                  // objects are visited in place too
        item.qty += 1
    print(cart[0].qty, cart[1].qty)
    return 0

// expect:
// 5 100 16 true
// item 100
// item 1
// item 4
// item 9
// item 16
// 16 4
// 2 37 true false
// none 0
// ada 37
// 11 3
