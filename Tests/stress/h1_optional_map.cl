// stress test H1_optional_map
// expect:
// 2 hall


class Room:
    name: String
    exit: ?String
function main() -> int32:
    let m = Map[String, Room]()
    m[String("a")] = Room(String("hall"), String("north"))
    m[String("b")] = Room(String("cave"), none)
    print(len(m), m[String("a")].name)
    return 0
