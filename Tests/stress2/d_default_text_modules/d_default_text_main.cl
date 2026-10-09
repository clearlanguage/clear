// a default text argument (and String(), whose text defaults to "") used from two modules:
// each module gets its own copy of the text
import "d_default_text_helper.cl"

function main() -> int32:
    let s = String()
    s += "main"
    print(greet(), from_helper(), s, helper_text(), Badge().label)
    return 0

// expect:
// 5 5 main helper! none
