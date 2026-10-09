// test-helper
// kv_text: escaping, splitting and hex helpers for kv_wal.cl

// escapes \, tab and newline so a value fits on one tab-separated line
function escape(s: str) -> String:
    let out = String()
    for c in s:
        switch c:
            case '\\':
                out.append("\\\\")
            case '\t':
                out.append("\\t")
            case '\n':
                out.append("\\n")
            default:
                out.push(c)
    return out

// none for a dangling backslash or an unknown escape
function unescape(s: str) -> ?String:
    let out = String()
    let i: int64 = 0
    while i < len(s):
        let c = s[i]
        if c == '\\':
            if i + 1 >= len(s):
                return none
            switch s[i + 1]:
                case '\\':
                    out.push('\\')
                case 't':
                    out.push('\t')
                case 'n':
                    out.push('\n')
                default:
                    return none
            i += 2
        else:
            out.push(c)
            i += 1
    return out

// splits on one separator byte, keeping empty fields
function fields(line: str, sep: int8) -> List[str]:
    let out = List[str]()
    let start: int64 = 0
    for i in 0..len(line):
        if line[i] == sep:
            out.push(line[start:i])
            start = i + 1
    out.push(line[start:])
    return out

function hex(v: uint64) -> String:
    let digits = "0123456789abcdef"
    let out = String()
    for i in 0..16:
        let shift = (15 - i) * 4
        out.push(digits[((v >> shift) & 15) as int64])
    return out
