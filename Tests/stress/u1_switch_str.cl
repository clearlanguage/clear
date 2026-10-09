// stress test U1_switch_str
// expect:
// two


function name(s: str) -> str:
    switch s:
        case "one":
            return "1"
        case "two":
            return "two"
        default:
            return "?"
function main() -> int32:
    print(name("two"))
    return 0
