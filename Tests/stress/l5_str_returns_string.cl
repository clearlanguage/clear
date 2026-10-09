// stress test L5_str_returns_String
// expect:
// P!


class P:
    n: int
    operator str(self) -> String:
        return String("P!")
function main() -> int32:
    print(P(1))
    return 0
