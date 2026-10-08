class Animal:
    legs: int

class Dog(Animal):
    tricks: int

function main() -> int32:
    let a = Animal(4)
    let d: *Dog = &a     // a *Animal is not necessarily a *Dog
    return 0

// expect-error
