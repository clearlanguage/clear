declare printf(fmt: *int8, args: ...) -> int32

class Counter:
    count: int

    function bump(*self):
        self.count += 1

    function get(*self) -> int:
        return self.count

function main() -> int32:
    let c = Counter { 0 }
    c.bump()
    c.bump()
    printf("%d\n", c.get())
    return 0

// expect:
// 2
