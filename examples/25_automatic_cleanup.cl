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
    let w = String("world")
    words.push(w)                      // moved into the list: w is now empty
    print(len(words), len(w))

    let first = words[0].copy()        // copying is explicit
    first.append("!")
    print(first, words[0])

    let s = open_session("ada")
    print("session for", s.user)

    let moved = s                      // moving hands everything over, nothing is copied
    print("still one session:", moved.user, len(s.user))
    print("end of main")
    return 0
    // here: moved (its String and Connection), first, w, words... are all cleaned up

// expect:
// using temporary
// closing temporary
// 2 0
// hello! hello
// session for ada
// still one session: ada 0
// end of main
// closing db
