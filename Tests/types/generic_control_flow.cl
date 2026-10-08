class Stack[T]:
    items: [8; T]
    count: int

    function push(self: *Stack[T], value: T):
        self.items[self.count] = value
        self.count += 1

    function total(self: *Stack[T]) -> T:
        let sum: T = self.items[0]
        for i in 1..self.count:
            sum += self.items[i]
        return sum

    function describe(self: *Stack[T]) -> int:
        switch self.count:
            case 0:
                return -1
            default:
                return when self.count > 2 use 2 otherwise 1

function main() -> int32:
    let s = Stack { {0, 0, 0, 0, 0, 0, 0, 0}, 0 }
    s.push(5)
    s.push(7)
    s.push(9)
    print(s.total(), s.describe())
    return 0

// expect:
// 21 2
