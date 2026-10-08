import "lib/geometry"
import "lib/geometry.cl" as geo

function helper() -> int:
    return 2

function main() -> int32:
    let r = square(4)
    print(r.area(), geo.square(2).area(), shapes_made, geo.helper(), helper())
    return 0

// expect:
// 16 4 3 1 2
