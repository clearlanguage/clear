class Point:
    x: int = 1
    y: int = 2

class Account:
    owner: *int8
    balance: float64 = 100.0
    history: int

    function init(self: *Account, owner: *int8, deposit: float64):
        self.owner = owner
        self.balance += deposit
        self.history = 1

class Pair[T]:
    first: T
    second: T

function main() -> int32:
    print(Point { 5 }, Point(), Point(3, 4), Point { })
    let a = Account("ada", 25.0)
    print(a.owner, a.balance, a.history)
    print(Pair(1.5, 2.5), Pair(7, 8).second)
    return 0

// expect:
// Point(x=5, y=2) Point(x=1, y=2) Point(x=3, y=4) Point(x=1, y=2)
// ada 125.0 1
// Pair[float64](first=1.5, second=2.5) 8
