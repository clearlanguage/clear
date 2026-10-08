import "lib/animals"

class Cat(Animal):
    function sound(self: *Cat) -> str:
        return "meow"

function main() -> int32:
    let c = Cat("tom")
    c.speak()
    let a = Animal("thing")
    a.speak()
    return 0

// expect:
// tom says meow
// thing says ...
