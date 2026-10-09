// N9: -v on a class calls its operator negate (and dispatches on the real type like the other operators)
class V:
    x: int

    operator negate(self) -> V:
        return V(0 - self.x)

    operator subtract(self, o: V) -> V:
        return V(self.x - o.x)

class Money:
    cents: String

    operator negate(self) -> Money:
        return Money("-" + self.cents)

function flip[T](value: T) -> T:
    return -value

function main() -> int32:
    let a = V(5)
    let b = -a
    print(b.x, (-(a - V(2))).x, flip(a).x, flip(3), flip(2.5))
    let m = Money(String("100"))
    print((-m).cents)
    return 0

// expect:
// -5 -3 -5 -3 -2.5
// -100
