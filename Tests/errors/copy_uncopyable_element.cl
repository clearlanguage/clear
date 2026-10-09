import "list"

class Connection:                     // cleans up itself and has no operator copy
    id: int

    operator destruct(self):
        print("closing", self.id)

function main() -> int32:
    let links = List[Connection]()
    links.push(Connection(1))
    let second = links[0]             // two owners of one connection
    return 0

// expect-error
