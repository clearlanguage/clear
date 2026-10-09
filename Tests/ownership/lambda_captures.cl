// a lambda borrows values that own memory; `move lambda` takes them over
import "string"
import "list"

function call_twice[F](f: F):
    f()
    f()

function main() -> int32:
    let s = String("captured")
    let count = 3
    let show = lambda: print(s, count)
    show()
    print("still mine:", s)

    let names = List[String]()
    names.push(String("ada"))
    let add = lambda (x: str): names.push(String(x))   // changes the list itself
    add("bob")
    print(len(names), names[1])

    let owned = String("moved in")
    let keep = move lambda: print(owned)
    call_twice(keep)
    return 0

// expect:
// captured 3
// still mine: captured
// 2 bob
// moved in
// moved in
