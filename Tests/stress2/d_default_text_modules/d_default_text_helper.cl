// test-helper
class Badge:
    label: str = "none"

function greet(who: str = "world") -> int64:
    return len(who)

function from_helper() -> int64:
    return greet()

function helper_text() -> String:
    let out = String()
    out += "helper!"
    return out
