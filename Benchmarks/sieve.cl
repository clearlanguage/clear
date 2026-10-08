declare calloc(count: uint64, size: uint64) -> *int8
declare free(p: *int8)

const LIMIT = 50000000

function main() -> int32:
    let composite = calloc(LIMIT + 1, 1) as *bool
    let count = 0
    for i in 2..=LIMIT:
        if not composite[i]:
            count += 1
            let j = i as int64 * i
            while j <= LIMIT:
                composite[j] = true
                j += i
    print(count)
    free(composite as *int8)
    return 0
