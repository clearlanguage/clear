class Vec:
    x: int
    y: int

    function init(self, x: int, y: int):
        self.x = x
        self.y = y

    operator add(self, other: Vec) -> Vec:
        return Vec(self.x + other.x, self.y + other.y)

    operator subtract(self, other: Vec) -> Vec:
        return Vec(self.x - other.x, self.y - other.y)

    operator multiply(self, k: int) -> Vec:
        return Vec(self.x * k, self.y * k)

    operator equals(self, other: Vec) -> bool:
        return self.x == other.x and self.y == other.y

    operator less(self, other: Vec) -> bool:
        return self.x < other.x

    operator str(self) -> str:
        return "Vec"

// a fixed-size collection: len + get make it indexable and iterable
class Triple:
    items: [3; int]

    operator get(self, i: int64) -> int:
        return self.items[i]

    operator set(self, i: int64, value: int):
        self.items[i] = value

    operator len(self) -> int64:
        return 3

    operator contains(self, value: int) -> bool:
        for v in self.items:
            if v == value:
                return true
        return false

// iteration through a generator
class Countdown:
    start: int

    operator iterate(self) -> Generator[int]:
        let i = self.start
        while i > 0:
            yield i
            i -= 1

    operator call(self, extra: int) -> int:
        return self.start + extra

function main() -> int32:
    let a = Vec(1, 2)
    let b = Vec(10, 20)
    let c = a + b * 2 - Vec(1, 1)
    print(c.x, c.y, a == Vec(1, 2), a != b, a < b, a)

    let t = Triple()
    t[0] = 5
    t[1] = 6
    t[2] += 7
    print(t[0], len(t), 6 in t, 9 in t)
    for v in t:
        print("item", v)

    let down = Countdown(3)
    for n in down:
        print("count", n)
    print(down(10))
    return 0

// expect:
// 20 41 true true true Vec
// 5 3 true false
// item 5
// item 6
// item 7
// count 3
// count 2
// count 1
// 13
