class Animal:
    name: str
    legs: int = 4

    function describe(self: *Animal):
        print(self.name, "has", self.legs, "legs")

    function sound(self: *Animal) -> str:
        return "..."

    function speak(self: *Animal):
        print(self.name, "says", self.sound())

class Dog(Animal):
    tricks: int = 0

    function sound(self: *Dog) -> str:
        return "woof"

    function learn(self: *Dog):
        self.tricks += 1

class Bird(Animal):
    function sound(self: *Bird) -> str:
        return "tweet"

function loudest(a: *Animal):
    a.speak()

function main() -> int32:
    let d = Dog("rex")
    d.learn()
    d.learn()
    d.describe()
    d.speak()
    print(d)

    let b = Bird("tweety", 2)
    b.describe()

    let animals: [3; *Animal] = {&d, &b, null}
    let generic = Animal("thing")
    animals[2] = &generic

    for a in animals:
        loudest(a)

    let named = Dog(name = "fido", tricks = 3)
    print(named.tricks, named.legs, named.sound())
    return 0

// expect:
// rex has 4 legs
// rex says woof
// Dog(name=rex, legs=4, tricks=2)
// tweety has 2 legs
// rex says woof
// tweety says tweet
// thing says ...
// 3 4 woof
