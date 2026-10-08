// a module: any .cl file. Everything at the top level can be imported.
let shapes_made = 0

class Square:
    side: int

    function area(self: *Square) -> int:
        return self.side * self.side

function square(side: int) -> Square:
    shapes_made += 1
    return Square(side)
