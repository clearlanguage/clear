declare malloc(size: uint64) -> *int8
declare free(p: *int8)

const N = 700

function main() -> int32:
    let a = malloc(N * N * 8) as *float64
    let b = malloc(N * N * 8) as *float64
    let c = malloc(N * N * 8) as *float64
    for i in 0..N * N:
        a[i] = (i % 7) as float64 * 0.5
        b[i] = (i % 5) as float64 * 0.25
        c[i] = 0.0
    for i in 0..N:
        for k in 0..N:
            let aik = a[i * N + k]
            for j in 0..N:
                c[i * N + j] += aik * b[k * N + j]
    let total = 0.0
    for i in 0..N * N:
        total += c[i]
    print(total)
    free(a as *int8)
    free(b as *int8)
    free(c as *int8)
    return 0
