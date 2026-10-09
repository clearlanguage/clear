// no free() and no defer: everything is given back at the end of its scope (checked with valgrind)
import "string"
import "list"
import "map"

function shout(text: str) -> String:
    let s = String(text)
    return s.upper()                  // s is cleaned up here, the result is handed to the caller

function main() -> int32:
    let names = List[String]()
    names.push(String("ada"))
    let grace = String("grace")
    names.push(grace)                 // the list gets its own copy
    print(len(names), names[1], len(grace))

    let last = names.pop()            // the list hands the item over
    print(last, len(names))

    let copy = names[0].copy()        // a real, independent copy
    copy.append("!")
    print(names[0], copy)

    let groups = Map[String, List[int]]()
    groups[String("evens")] = List[int]()
    for i in 0..6:
        if i % 2 == 0:
            groups[String("evens")].push(i)
    print(len(groups[String("evens")]))
    groups.remove(String("evens"))   // removing cleans up the key and the list
    print(len(groups))

    for word in names:
        print("word", word)

    print(shout("loud"))

    let maybe: ?String = String("inside")
    switch maybe:
        case some(text):
            print("some", text)       // text names the string inside maybe, no copy
        case none:
            print("none")
    let taken = maybe.value           // a copy: maybe still holds its string
    print(taken, maybe is none)
    return 0

// expect:
// 2 grace 5
// grace 1
// ada ada!
// 3
// 0
// word ada
// LOUD
// some inside
// inside false
