// + on text makes a new String: str + str, String + str, str + String, and +=
function greet(name: String) -> String:
    return "hi " + name + "!"
function main() -> int32:
    let first: str = "ada"
    let full = first + " " + "lovelace"
    print(full, len(full))
    full += ", countess"
    print(full)
    print(greet("bob"))
    let names = List[String]()
    names.push("x")
    names.push(first + "2")
    for i in 0..1000:
        let tmp = "n" + first
        names.push(tmp)
    print(len(names), names[0], names[1], names[2])
    let middle = full[4:12]
    print("[" + middle + "]")
    return 0

// expect:
// ada lovelace 12
// ada lovelace, countess
// hi bob!
// 1002 x ada2 nada
// [lovelace]
