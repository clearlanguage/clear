import "string"
import "list"

// operator destruct runs when a value's scope ends. Most code never writes one: String, List,
// Map and File already clean up after themselves, and a class holding them does too.
class Connection:
    name: str

    operator destruct(self):
        if self.name != null:
            print("closing", self.name)

class Session:                         // no destruct of its own: its fields are cleaned up
    user: String
    link: Connection

function open_session(user: str) -> Session:
    return Session(String(user), Connection("db"))

function main() -> int32:
    if true:
        let c = Connection("temporary")
        print("using", c.name)
    // "closing temporary" was printed when the if block ended

    let words = List[String]()         // no free(), no defer
    words.push(String("hello"))

    // reading copies: first has its own text
    let first = words[0]
    first.append("!")

    // writing goes to the element itself
    words[0].append(" world")
    print(first, "|", words[0])

    let w = String("tail")
    words.push(w)                      // the list gets a copy, w stays usable
    print(len(words), w)

    // a Connection cleans up something itself and has no operator copy, so it can't be
    // copied: assigning it (or a Session holding one) moves it and leaves the old variable empty
    let s = open_session("ada")
    let moved = s                      // moving hands everything over: s.user here would not compile
    print("one session:", moved.user)
    print("end of main")
    return 0
    // here: moved (its String and Connection), first, w, words... are all cleaned up

// expect:
// using temporary
// closing temporary
// hello! | hello world
// 2 tail
// one session: ada
// end of main
// closing db
