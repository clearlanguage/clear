class Counter:
    n: int

    function add(self: *Counter, amount: int):
        self.n += amount

function main() -> int32:
    let c = Counter { 0 }
    c.add(1, 2)
    return 0

// expect-error
