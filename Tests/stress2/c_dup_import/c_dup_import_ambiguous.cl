// both imports define helper(): which one is meant must be said, the first one is not picked silently
import "lib/ma"
import "lib/mb"

function main() -> int32:
    print(helper())
    return 0

// expect-error: ‘helper’ is defined by both ‘ma.cl’ and ‘mb.cl’
