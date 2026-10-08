import "list"

class Point:
    x: int = 7
    y: int

class Bag:
    items: [4; int]

    operator contains(self: *Bag, value: int) -> bool:
        return value in self.items

function main() -> int32:
    print(2 ** 10, 3 ** 0, 2 ** -1, 2.0 ** 0.5, 10 ** 2 * 2)
    let x = 5
    if x == 1:
        print("one")
    else if x == 5:
        print("five")
    else:
        print("other")
    let a = "hi"
    let b: str = "hi"
    print(a == "hi", a != b, "apple" < "banana", a == null)
    let arr: [4; int] = {1, 2, 3, 4}
    print(3 in arr, 9 in arr, 9 not in arr, "lo" in "hello", "xyz" in "hello")
    print(len(arr), len("hello"), len(a))
    let numbers = List[int]()
    numbers.push(1)
    print(len(numbers))
    let bag = Bag({5, 6, 7, 8})
    print(6 in bag, 1 in bag, 1 not in bag)
    let z: int
    let p: Point
    print(z, p)
    assert x == 5, "x should be five"
    assert len(arr) == 4
    print("asserts passed")


    return 0

// expect:
// 1024 1 0 1.4142135623731 200
// five
// true false true false
// true false true true false
// 4 5 2
// 1
// true false true
// 0 Point(x=7, y=0)
// asserts passed
