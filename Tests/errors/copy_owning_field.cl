import "string"

class Person:
    name: String

    function get_name(self) -> String:
        return self.name     // would copy the field

function main() -> int32:
    return 0

// expect-error
