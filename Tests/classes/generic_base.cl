class Base[T]:
    value: T

    virtual function show(self: *Base[T]):
        print("base", self.value)

class Child(Base[int]):
    extra: int

    function show(self: *Child):
        print("child", self.value, self.extra)

let global_child: Child

function main() -> int32:
    let c = Child(1, 2)
    let b: *Base[int] = &c
    b.show()
    global_child.show()
    let plain = Base[int](5)
    plain.show()
    return 0

// expect:
// child 1 2
// child 0 0
// base 5
