// stress test M15_enum_as_string
// expect-error: E107


enum K:
    A
function take(s: String):
    print(s)
function main() -> int32:
    take(K.A as String)
    return 0
