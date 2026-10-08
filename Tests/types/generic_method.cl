declare printf(fmt: *int8, args: ...) -> int32

class Box[T]:
    value: T

    function get(self: *Box[T]) -> T:
        return self.value

    function set(self: *Box[T], v: T):
        self.value = v

function main() -> int32:
    let b = Box { 2.5 }
    b.set(4.25)
    let i = Box { 3 }
    printf("%.2f %d\n", b.get(), i.get())
    return 0

// expect:
// 4.25 3
