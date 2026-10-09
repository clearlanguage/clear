// string: String, an owned growable string, plus helpers for C strings (*int8).
//
// str is text you look at (a literal, a String, part of either): a pointer and a length. A String turns into a
// str wherever one is expected, without copying; text[2:5] is a str looking at part of it.
//
//     import "string"
//     let greeting = String("hello")
//     defer greeting.free()
//     greeting.append(", world")
//     print(greeting, len(greeting))      // hello, world 12
//
// A String owns its bytes on the heap. Nothing copies or frees them behind your back: `copy()` makes an
// independent String, `+` builds a new one (both allocate), and `free()` gives the memory back.

import "memory"

declare strlen(text: *int8) -> uint64
declare strcmp(a: *int8, b: *int8) -> int32
declare strncmp(a: *int8, b: *int8, count: uint64) -> int32
declare strchr(text: *int8, character: int32) -> *int8
declare strstr(text: *int8, part: *int8) -> *int8
declare atoi(text: *int8) -> int32
declare atof(text: *int8) -> float64
declare toupper(character: int32) -> int32
declare snprintf(buffer: *int8, size: uint64, format: *int8, args: ...) -> int32
declare tolower(character: int32) -> int32

function length(text: *int8) -> int64:
    return strlen(text) as int64

function equals(a: *int8, b: *int8) -> bool:
    return strcmp(a, b) == 0

function starts_with(text: *int8, prefix: *int8) -> bool:
    return strncmp(text, prefix, strlen(prefix)) == 0

function contains(text: *int8, part: *int8) -> bool:
    return strstr(text, part) != null

function to_int(text: *int8) -> int32:
    return atoi(text)

function to_float(text: *int8) -> float64:
    return atof(text)

class String:
    data: *int8
    length: int64
    capacity: int64

    function init(self: *String, text: str):
        self.append(text)

    // room for at least `needed` bytes (plus the terminating zero)
    function reserve(self: *String, needed: int64):
        if needed + 1 <= self.capacity:
            return
        let grown = when self.capacity < 16 use 16 otherwise self.capacity * 2
        while grown < needed + 1:
            grown *= 2
        self.data = reallocate(self.data, grown)
        self.capacity = grown
        self.data[self.length] = 0

    function append_bytes(self: *String, bytes: *int8, count: int64):
        self.reserve(self.length + count)
        copy(&self.data[self.length], bytes, count)
        self.length += count
        self.data[self.length] = 0

    function append(self: *String, text: str):
        self.append_bytes(pointer(text), len(text))

    function append_string(self: *String, other: *String):
        self.append_bytes(other.data, other.length)

    function push(self: *String, character: int8):
        self.reserve(self.length + 1)
        self.data[self.length] = character
        self.length += 1
        self.data[self.length] = 0

    function append_int(self: *String, value: int64):
        let buffer: [32; int8] = {}
        let count = snprintf(&buffer[0], 32, "%lld", value)
        self.append_bytes(&buffer[0], count as int64)

    function append_float(self: *String, value: float64):
        let buffer: [64; int8] = {}
        let count = snprintf(&buffer[0], 64, "%g", value)
        self.append_bytes(&buffer[0], count as int64)

    // the text as a str (valid until the String changes or is freed); it ends with a zero byte, so C can use it
    function text(self: *String) -> str:
        if self.data == null:
            return ""
        return view(self.data, self.length)

    function c_str(self: *String) -> str:
        return self.text()

    // text[a:b]: a str looking at those bytes (no copy). It is also how a String becomes a str
    operator slice(self, start: int64, end: int64) -> str:
        assert start >= 0 and start <= end and end <= self.length, "String slice out of range"
        if self.data == null:
            return ""
        return view(self.data + start, end - start)

    operator str(self: *String) -> str:
        return self.text()

    operator len(self: *String) -> int64:
        return self.length

    operator get(self: *String, index: int64) -> int8:
        assert index >= 0 and index < self.length, "String index out of range"
        return self.data[index]

    operator set(self: *String, index: int64, character: int8):
        assert index >= 0 and index < self.length, "String index out of range"
        self.data[index] = character

    operator equals(self: *String, other: *String) -> bool:
        return self.text() == other.text()

    operator not_equals(self: *String, other: *String) -> bool:
        return self.text() != other.text()

    operator less(self: *String, other: *String) -> bool:
        return self.text() < other.text()

    operator hash(self: *String) -> uint64:
        return hash(self.text())

    // a new String holding both (allocates)
    operator add(self: *String, other: *String) -> String:
        let result = self.copy()
        result.append_string(other)
        return result

    operator contains(self: *String, part: str) -> bool:
        return part in self.text()

    function equals(self: *String, text: str) -> bool:
        return self.text() == text

    // let t = s (or reading a String out of a list or field) gives t its own copy of the text
    operator copy(self) -> String:
        return self.copy()

    function copy(self: *String) -> String:
        let result = String { }
        result.append_bytes(self.data, self.length)
        return result

    // the index of the first `part`, or -1
    function find(self: *String, part: str) -> int64:
        let text = self.text()
        let count = len(part)
        for i in 0..self.length - count + 1:
            if text[i:i + count] == part:
                return i
        return -1

    function starts_with(self: *String, prefix: str) -> bool:
        return len(prefix) <= self.length and self.text()[:len(prefix)] == prefix

    function ends_with(self: *String, suffix: str) -> bool:
        return len(suffix) <= self.length and self.text()[self.length - len(suffix):] == suffix

    // bytes [start, end) as a new String
    function slice(self: *String, start: int64, end: int64) -> String:
        assert start >= 0 and start <= end and end <= self.length, "String slice out of range"
        let result = String { }
        result.append_bytes(&self.data[start], end - start)
        return result

    function upper(self: *String) -> String:
        let result = self.copy()
        for i in 0..result.length:
            result.data[i] = toupper(result.data[i] as int32) as int8
        return result

    function lower(self: *String) -> String:
        let result = self.copy()
        for i in 0..result.length:
            result.data[i] = tolower(result.data[i] as int32) as int8
        return result

    // without spaces, tabs and line breaks at either end
    function strip(self: *String) -> String:
        let start: int64 = 0
        let end = self.length
        while start < end and is_space(self.data[start]):
            start += 1
        while end > start and is_space(self.data[end - 1]):
            end -= 1
        return self.slice(start, end)

    function to_int(self: *String) -> int32:
        return atoi(self.text())

    function to_float(self: *String) -> float64:
        return atof(self.text())

    function clear(self: *String):
        self.length = 0
        if self.data != null:
            self.data[0] = 0

    operator destruct(self):
        self.free()

    // gives the memory back now (it is also given back automatically at the end of the string's scope)
    function free(self: *String):
        if self.data != null:
            release(self.data)
        self.data = null
        self.length = 0
        self.capacity = 0

function is_space(character: int8) -> bool:
    return character == ' ' or character == '\t' or character == '\n' or character == '\r'

function from_int(value: int64) -> String:
    let result = String { }
    result.append_int(value)
    return result

function from_float(value: float64) -> String:
    let result = String { }
    result.append_float(value)
    return result
