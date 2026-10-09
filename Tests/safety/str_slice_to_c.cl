// flags: --checks
// part of a text has no zero byte after it, so C must not get it as a char*
declare puts(text: str) -> int32

function main() -> int32:
    let s: str = "hello world"
    puts(s[6:])          // the end of a text: fine
    puts(s[0:5])         // the middle: stopped
    return 0

// expect-exit: -6
