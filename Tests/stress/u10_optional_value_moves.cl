// stress test U10: opt.value on the optional's last use moves instead of copying
// flags: --copies
// expect:
// hello
// hello hello

function get() -> ?String:
    return String("hello")

function main() -> int32:
    let o = get()
    let s = o.value                 // last use of o: moved, no copy
    print(s)
    let p = get()
    let t = p.value                 // p is used again: copied
    print(t, p.value)
    return 0
