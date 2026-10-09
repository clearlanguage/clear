import "string"
import "list"

// The snippets of README.md's tour that are not in readme_tour.cl, so the README stays true.
import "math"

enum Shape:
    Circle(radius: float64)
    Rect(width: float64, height: float64)
    Empty

class Point:
    x: int
    y: int

class Temperature:
    celsius: float64

    property fahrenheit(self: *Temperature) -> float64:
        return self.celsius * 9.0 / 5.0 + 32.0

    property fahrenheit(self: *Temperature, value: float64):
        self.celsius = (value - 32.0) * 5.0 / 9.0

class Animal:
    name: str

    function sound(self: *Animal) -> str:
        return "..."

    function speak(self: *Animal):
        print(self.name, "says", self.sound())

class Dog(Animal):
    function sound(self: *Dog) -> str:
        return "woof"

    function speak(self: *Dog):
        super.speak()

class Grid:
    cells: [9; int]

    operator get(self, i: int64) -> *int:
        return &self.cells[i]

    operator len(self) -> int64:
        return 9

class Account:
    owner: str
    balance: float64 = 0.0

    function init(self, owner: str):
        self.owner = owner

    function deposit(self, amount: float64):
        self.balance += amount

    function snapshot(self: Account) -> float64:
        self.balance = -1.0
        return self.balance

variant Number:
    int
    float64

class Connection:
    name: str

    operator destruct(self):
        print("closing", self.name)

function greet(name: str, greeting: str = "hello") -> str:
    return greeting

function divmod(a: int, b: int) -> (int, int):
    return a / b, a % b

function fibonacci() -> Generator[int64]:
    let a: int64 = 0
    let b: int64 = 1
    while true:
        yield a
        a, b = b, a + b

async function add(a: int, b: int) -> int:
    return a + b

async function worker(steps: int) -> int:
    for i in 0..steps:
        await pause()
    return await add(steps, 1)

macro swap(a, b):
    let tmp = a
    a = b
    b = tmp

macro square(x):
    x * x

function main() -> int32:
    let x = 10
    if x > 10:
        print("big")
    else if x == 10:
        print("ten")
    else:
        print("small")

    let values: [3; int] = {1, 2, 3}
    print(3 in values, "ell" in "hello", len(values))
    print(greet("ada"), greet("ada", greeting = "hi"))
    let q, r = divmod(17, 5)
    print(q, r, Point(y = 4, x = 3))

    let scores: [3; int] = {7, 8, 9}
    print("total:", 42, 2.5, true, scores, Point(1, 2), (1, "a"), Shape.Circle(1.0))

    let t = Temperature(0.0)
    t.fahrenheit = 212.0
    print(t.celsius)

    let d = Dog("rex")
    d.speak()

    for f in fibonacci():
        if f > 20:
            break
        print(f)

    print(worker(3).run())

    let y = 20
    swap!(x, y)
    print(x, y, square!(7))

    let grid = Grid()
    grid[4] = 7
    grid[4] += 1
    let total = 0
    for cell in grid:
        total += cell
    print(grid[4], total)

    let account = Account("ada")
    account.deposit(5.0)
    print(account.snapshot(), account.balance)

    let n: Number = 2.5
    print(n is float64, n as float64)
    n = 7
    switch n:
        case int(i):
            print("int", i)
        case float64(f):
            print("float", f)

    if true:
        let c = Connection("db")

    let words = List[String]()
    words.push(String("hello"))
    let first = words[0]
    words[0].append(" world")
    print(first, "|", words[0])
    return 0

// expect:
// ten
// true true 3
// hello hi
// 3 2 Point(x=3, y=4)
// total: 42 2.5 true [7, 8, 9] Point(x=1, y=2) (1, a) Shape.Circle(radius=1.0)
// 100.0
// rex says woof
// 0
// 1
// 1
// 2
// 3
// 5
// 8
// 13
// 4
// 20 10 49
// 8 8
// -1.0 5.0
// true 2.5
// int 7
// closing db
// hello | hello world
