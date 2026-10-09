// a misspelled name through an import alias is E010 at that name, not a crash
import "lib/shapes" as sh

function main() -> int32:
    print(sh.NOPE)
    print(sh.nothere(1))
    let m = sh.Missing(2)
    let x: sh.Nope = 3
    return 0

// expect-error: E010
