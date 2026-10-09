// an unknown type behind ?, *, [N; ...], a tuple, a function type or List[...] is E010, not a crash
class A:
    x: ?Nope

function f() -> ?Nope:
    return none

function g(a: ?Nope, b: *Nope) -> int:
    return 0

function h(k: function(?Nope) -> int) -> (Nope, int):
    return (1, 2)

function main() -> int32:
    let a: ?Nope = none
    let b: *Nope = null
    let c: [2; Nope] = {}
    let d: (Nope, int) = (1, 2)
    let e: function(Nope) -> int = none
    let l = List[?Nope]()
    let o: ?List[Nope] = none
    return 0

// expect-error: E010
