// a clash between imports is fine while the name isn't used, and a definition in this file wins
import "lib/ma"
import "lib/mb"
import "lib/mb" as b

function helper() -> int:
    return 3

function main() -> int32:
    print(helper(), only_a(), b.helper())
    return 0

// expect:
// 3 10 2
