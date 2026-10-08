class User:
    id: int
    name: str

function find(users: *[3; User], id: int) -> ?User:
    for u in *users:
        if u.id == id:
            return u
    return none

function main() -> int32:
    let users: [3; User] = {User(1, "ada"), User(2, "alan"), User(3, "grace")}
    let a = find(&users, 2)
    if a is not none:
        print(a.value.name)
    let b = find(&users, 9)
    print(b is none)
    return 0

// expect:
// alan
// true
