// a lambda copies what it captures when it is made (like ints); what cannot be copied is borrowed;
// `move lambda` takes values over
import "string"
import "list"

class Handle:
    name: str

    operator destruct(self):
        self.name = ""

function call_twice[F](f: F):
    f()
    f()

function main() -> int32:
    let s = String("captured")
    let count = 3
    let show = lambda: print(s, count)
    s.append("!")                     // the lambda has its own copy: it still sees "captured"
    count = 4
    show()
    print("still mine:", s)

    let names = List[String]()
    names.push(String("ada"))
    let size = lambda: len(names)
    names.push(String("bob"))
    print(size(), len(names))         // 1: the copy was made before "bob"

    let h = Handle("file")
    let peek = lambda: print(h.name)  // a Handle cannot be copied: borrowed
    peek()

    let owned = String("moved in")
    let keep = move lambda: print(owned)
    call_twice(keep)
    return 0

// expect:
// captured 3
// still mine: captured!
// 1 2
// file
// moved in
// moved in
