// N9: -v on a class without operator negate is a compile error that says what to define
class V:
    x: int

    operator subtract(self, o: V) -> V:
        return V(self.x - o.x)

function main() -> int32:
    let a = V(1)
    let b = -a
    print(b.x)
    return 0

// expect-error: operator negate(self) -> V
