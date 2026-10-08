import "list"

function main() -> int32:
    let grid = List[List[int]]()
    for r in 0..3:
        let row = List[int]()
        for c in 0..4:
            row.push(r * 10 + c)
        grid.push(row)
    let total = 0
    for row in grid:
        for v in row:
            total += v
    print(len(grid), len(grid[2]), grid[2][3], total)
    grid[1][1] = 99
    print(grid[1][1])
    for row in grid:
        row.free()
    grid.free()
    return 0

// expect:
// 3 4 23 138
// 99
