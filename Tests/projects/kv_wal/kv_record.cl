// test-helper
// kv_record: write-ahead-log records: one line per record, "body<TAB>checksum-of-body"
import "kv_text" as tx

const FORMAT = "kv1"

enum Record:
    Set(key: String, value: String)
    Del(key: String)
    Clear

// 64-bit FNV-1a
function checksum(s: str) -> uint64:
    let h: uint64 = 14695981039346656037
    for c in s:
        h = h ^ (c as uint8 as uint64)
        h = h * 1099511628211
    return h

function body_of(r: *Record) -> String:
    switch *r:
        case Set(key, value):
            return "S\t" + tx.escape(key) + "\t" + tx.escape(value)
        case Del(key):
            return "D\t" + tx.escape(key)
        case Clear:
            return "C"

function encode(r: *Record) -> String:
    let body = body_of(r)
    return body + "\t" + tx.hex(checksum(body)) + "\n"

// none when the line is damaged (torn write, bad checksum, unknown tag, bad escape)
function decode(line: str) -> ?Record:
    let parts = tx.fields(line, '\t')
    if len(parts) < 2:
        return none
    let body = line[0:len(line) - len(parts.last()) - 1]
    if tx.hex(checksum(body)) != parts.last():
        return none
    let tag = parts[0]
    if tag == "S" and len(parts) == 4:
        let k = tx.unescape(parts[1])
        let v = tx.unescape(parts[2])
        if not k or not v:
            return none
        return Record.Set(key = k, value = v)
    if tag == "D" and len(parts) == 3:
        let k = tx.unescape(parts[1])
        if not k:
            return none
        return Record.Del(key = k)
    if tag == "C" and len(parts) == 2:
        return Record.Clear
    return none
