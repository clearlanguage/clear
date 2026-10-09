// everything of a module imported `as sh` works like it does without the alias
import "lib/shapes" as sh

function area(s: sh.Shape) -> int:
    switch s:
        case Circle(r):
            return r * r * 3
        case Empty:
            return 0

function main() -> int32:
    let b = sh.Box(5)                  // a generic class, its type argument inferred
    print(b.v, b.get())

    let a = sh.Shape.Circle(r = 2)     // enum cases with data, by name and by position
    let c = sh.Shape.Circle(3)
    let e = sh.Shape.Empty
    print(area(a), area(c), area(e), e is sh.Shape.Empty)

    let grid: [sh.N; int] = {}         // a const of the module as an array size
    print(len(grid), sh.N + 1, sh.NAME, sh.twice(sh.N), sh.Color.Blue as int)
    return 0

// expect:
// 5 5
// 12 27 0 true
// 4 5 shapes 8 10
