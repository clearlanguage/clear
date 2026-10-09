class Connection:
    id: int

    operator destruct(self):
        print("closing", self.id)

class Client:
    link: Connection

    function get_link(self) -> Connection:
        return self.link

function main() -> int32:
    return 0

// expect-error
