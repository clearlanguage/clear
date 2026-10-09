// stress test H4_self_op
// expect:
// 2
// true


class N:
    items: List[int]
    operator add(self, other: N) -> int64:
        return len(self.items) + len(other.items)
    operator equals(self, other: N) -> bool:
        return len(self.items) == len(other.items)
function main() -> int32:
    let a = N(List[int]())
    a.items.push(1)
    print(a + a)
    print(a == a)
    return 0
