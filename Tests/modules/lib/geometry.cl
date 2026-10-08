class Rect:
    w: int
    h: int

    function area(self: *Rect) -> int:
        return self.w * self.h

function square(n: int) -> Rect:
    return Rect(n, n)

let shapes_made = 3

// a name that also exists in the importing file must not clash
function helper() -> int:
    return 1
