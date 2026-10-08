// Every snippet in README.md comes from this file, so the README stays true.
import "math"
import "list"

enum Shape:
    Circle
    Square

class Vec2:
    x: float64
    y: float64

    operator add(self: *Vec2, other: Vec2) -> Vec2:
        return Vec2(self.x + other.x, self.y + other.y)

    function length(self: *Vec2) -> float64:
        return sqrt(self.x * self.x + self.y * self.y)

class Account:
    owner: *int8
    balance: float64 = 0.0

    function init(self: *Account, owner: *int8):
        self.owner = owner

    function deposit(self: *Account, amount: float64):
        self.balance += amount

function area(shape: Shape, size: float64) -> float64:
    switch shape:
        case Shape.Circle:
            return PI * size * size
        default:
            return size * size

function largest[T](values: *List[T]) -> T:
    let best = values[0]
    for value in values:
        best = max(best, value)
    return best

function main() -> int32:
    let total = 0
    for i in 1..=10:
        if i % 2 == 0:
            continue
        total += i
    print("odd sum:", total)

    let v = Vec2(3.0, 4.0) + Vec2(0.0, 0.0)
    print(v, v.length())

    let account = Account("ada")
    account.deposit(25.5)
    print(account.owner, account.balance)

    print(round(area(Shape.Circle, 1.0) * 100.0) / 100.0, area(Shape.Square, 3.0))

    let numbers = List[int]()
    defer numbers.free()
    for n in 0..5:
        numbers.push(n * n)
    print(largest(&numbers), numbers.length)

    let label = when total > 20 use "big" otherwise "small"
    print(label)
    return 0

// expect:
// odd sum: 25
// Vec2(x=3.0, y=4.0) 5.0
// ada 25.5
// 3.14 9.0
// 16 5
// big
