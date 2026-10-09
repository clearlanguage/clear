// stress test H8_deref_equals
// expect:
// true false


class Key:
    s: String
    operator equals(self, other: Key) -> bool:
        return self.s == other.s
function same(p: *Key, q: *Key) -> bool:
    return *p == *q
function main() -> int32:
    let a = Key(String("x"))
    let b = Key(String("x"))
    let c = Key(String("y"))
    print(same(&a, &b), same(&a, &c))
    return 0
