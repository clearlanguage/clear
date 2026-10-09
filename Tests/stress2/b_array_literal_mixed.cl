// N5: an array literal mixing computed values and constants (built in registers) must be valid IR at every -O level
class Animal:
    name: str
    function sound(self) -> str:
        return "..."

class Dog(Animal):
    function sound(self) -> str:
        return "woof"

function main() -> int32:
    let x = 4
    let a: [2; int] = {x, 0}
    let b: [3; int] = {1, x, 2}
    let f = 1.5
    let c = [f, 0.0, 2.0]
    print(a[0], a[1], b, c)

    let dog = Dog("rex")
    let pets: [2; *Animal] = {&dog, null}
    print(pets[0].sound(), pets[1] == null)
    return 0

// expect:
// 4 0 [1, 4, 2] [1.5, 0.0, 2.0]
// woof true
