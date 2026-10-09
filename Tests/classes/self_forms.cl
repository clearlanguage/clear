class Counter:
    count: int

    function bump(self):
        self.count += 1

    function bump_copy(self: Counter) -> int:
        self.count += 100
        return self.count

    function get(self: *Counter) -> int:
        return self.count

class Box[T]:
    value: T

    function get(self) -> T:
        return self.value

function main() -> int32:
    let c = Counter(0)
    c.bump()
    c.bump()
    print(c.get(), c.bump_copy(), c.count, Box(5).get())
    return 0

// expect:
// 2 102 2 5
