class Item:
    qty: int

function make() -> Item:
    return Item(1)

function main() -> int32:
    make().qty = 5         // changes a copy that is thrown away
    return 0

// expect-error: This changes a temporary value
