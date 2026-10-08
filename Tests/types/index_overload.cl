declare printf(fmt: *int8, args: ...) -> int32

class Grid:
    cells: [9; int]

    function __getitem__(self: *Grid, i: int) -> int:
        return self.cells[i]

    function __setitem__(self: *Grid, i: int, value: int):
        self.cells[i] = value

function main() -> int32:
    let g = Grid { {0, 0, 0, 0, 0, 0, 0, 0, 0} }
    g[4] = 7
    g[2] = g[4] + 1
    printf("%d %d %d\n", g[4], g[2], g[0])
    return 0

// expect:
// 7 8 0
