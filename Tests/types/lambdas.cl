function twice(x: int) -> int:
    return x * 2

function apply(f: function(int) -> int, x: int) -> int:
    return f(x)

function apply_all[F](f: F, values: *[4; int]):
    for i in 0..4:
        (*values)[i] = f((*values)[i])

class Handler:
    on_event: function(int) -> int

class Multiplier:
    factor: int

    operator call(self: *Multiplier, x: int) -> int:
        return x * self.factor

function main() -> int32:
    print(apply(twice, 4), apply(lambda x: x + 100, 1))
    let square: function(int) -> int = lambda n: n * n
    print(square(9))
    let f = twice
    print(f(21))
    let offset = 10
    let add_offset = lambda (x: int): x + offset
    print(add_offset(5))
    offset = 1000
    print(add_offset(5))
    let values: [4; int] = {1, 2, 3, 4}
    apply_all(lambda (v: int): v * offset, &values)
    print(values)
    let h = Handler(lambda x: x - 1)
    print(h.on_event(10))
    let triple = Multiplier(3)
    print(triple(7))
    let pick = lambda (a: int, b: int): when a > b use a otherwise b
    print(pick(3, 8))
    return 0

// expect:
// 8 101
// 81
// 42
// 15
// 15
// [1000, 2000, 3000, 4000]
// 9
// 21
// 8
