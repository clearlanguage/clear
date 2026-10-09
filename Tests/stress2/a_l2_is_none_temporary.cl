// L2: `f() is not none` on a new ?String must clean the optional up (it leaked at -O0 / -O1)

function find(n: int) -> ?String:
    if n > 0:
        return String("x")
    return none

class Holder[V]:
    v: V

    function find(self, k: int) -> ?V:
        if k == 1:
            return self.v
        return none

    operator contains(self, k: int) -> bool:
        return self.find(k) is not none

function main() -> int32:
    print(find(1) is not none, find(0) is not none, find(2) is none)
    let h = Holder[String](String("x"))
    print(1 in h, 2 in h)
    let count = 0
    for i in 0..5:
        if find(i) is not none:
            count += 1
    print(count)
    return 0

// expect:
// true false false
// true false
// 4
