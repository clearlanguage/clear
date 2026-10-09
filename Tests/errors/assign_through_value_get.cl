class Item:
    qty: int

class Bag:
    item: Item

    operator get(self, i: int) -> Item:   // returns a copy, not a reference
        return self.item

function main() -> int32:
    let bag = Bag(Item(1))
    bag[0].qty = 5         // would change the copy only
    return 0

// expect-error: This changes a temporary value
