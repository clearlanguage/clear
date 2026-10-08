class Point:
    x: int
    y: int

    function length_squared(self: *Point) -> int:
        return self.x * self.x + self.y * self.y

class Account:
    owner: str
    balance: float64 = 0.0                 // a default

    function init(self: *Account, owner: str):
        self.owner = owner

    function deposit(self: *Account, amount: float64):
        self.balance += amount

function main() -> int32:
    let p = Point(3, 4)                    // fields in order
    let q = Point(y = 1, x = 2)            // or by name
    let r = Point { 5 }                    // struct literal: missing fields are 0 / their default
    print(p, q, r, p.length_squared())

    let account = Account("ada")           // runs __init__
    account.deposit(25.5)
    account.deposit(4.5)
    print(account.owner, account.balance)

    let ptr = &account                     // methods and fields work through pointers too
    ptr.deposit(10.0)
    print(account.balance)
    return 0

// expect:
// Point(x=3, y=4) Point(x=2, y=1) Point(x=5, y=0) 25
// ada 30.0
// 40.0
