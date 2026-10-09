// reading an owning value copies it; writing goes through the element itself
import "string"
import "list"
import "map"

class Person:
    name: String

    function get_name(self) -> String:
        return self.name              // a copy of the field

class Tag:
    text: String

    operator equals(self, other: Tag) -> bool:   // by value: the right side is copied
        return self.text == other.text

function shout(s: String) -> String:  // s is the caller's copy
    s.append("!")
    return s                          // handed back (moved), not copied again

function main() -> int32:
    let words = List[String]()
    words.push(String("hello"))

    let first = words[0]              // its own copy
    first.append(" there")
    print(first, "|", words[0])

    words[0].append(", world")        // writing changes the element itself
    print(words[0])

    let w = String("tail")
    words.push(w)                     // the list gets a copy, w is still usable
    print(len(words), w)

    let p = Person(String("ada"))
    let n = p.get_name()
    n.append(" lovelace")
    print(n, "|", p.name)

    print(shout(w), w)                // w is unchanged

    print(Tag(String("x")) == Tag(String("x")))

    let lists = List[List[int]]()
    let row = List[int]()
    row.push(1)
    lists.push(row)                   // copied, with its own items
    row.push(2)
    print(len(lists[0]), len(row))

    let ages = Map[String, int]()
    ages[String("ada")] = 36
    let backup = ages                 // a separate map
    ages[String("ada")] = 37
    print(ages[String("ada")], backup[String("ada")])

    let maybe: ?String = String("inside")
    let taken = maybe.value           // a copy; maybe still holds its string
    print(taken, maybe is none)
    return 0

// expect:
// hello there | hello
// hello, world
// 2 tail
// ada lovelace | ada
// tail! tail
// true
// 1 2
// 37 36
// inside false
