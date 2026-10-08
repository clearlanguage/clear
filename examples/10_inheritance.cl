class Animal:
    name: str
    legs: int = 4

    // a subclass can replace a method; calls always run the object's own version
    function sound(self: *Animal) -> str:
        return "..."

    function speak(self: *Animal):
        print(self.name, "says", self.sound())

class Dog(Animal):
    tricks: int = 0                        // added after Animal's fields

    function sound(self: *Dog) -> str:     // replaces Animal's sound
        return "woof"

    function speak(self: *Dog):
        print("(wags tail)")
        super.speak()                      // Animal's version

class Bird(Animal):
    function sound(self: *Bird) -> str:
        return "tweet"

function introduce(a: *Animal):            // accepts any kind of Animal
    a.speak()

function main() -> int32:
    let d = Dog("rex")
    let b = Bird("tweety", 2)
    d.speak()
    print(d)

    introduce(&b)
    let zoo: [2; *Animal] = {&d, &b}
    for a in zoo:
        print(a.name, a.legs, a.sound())
    return 0

// expect:
// (wags tail)
// rex says woof
// Dog(name=rex, legs=4, tricks=0)
// tweety says tweet
// rex 4 woof
// tweety 2 tweet
