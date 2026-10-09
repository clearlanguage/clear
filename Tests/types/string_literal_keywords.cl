// the strings "none", "null" and "true" are text, not the keywords
function main() -> int32:
    let s: str = "none"
    print(s, "null", "true")
    return 0

// expect:
// none null true
