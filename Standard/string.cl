// string: helpers for C strings (str, the type of "string literals"), and String, an owned growable string.
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

    function __init__(self: *String, text: str):
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
        self.append_bytes(text, strlen(text) as int64)

    function append_string(self: *String, other: String):
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

    // the text as a str (valid until the String changes or is freed)
    function __str__(self: *String) -> str:
        if self.data == null:
            return ""
        return self.data

    function c_str(self: *String) -> str:
        return self.__str__()

    function __len__(self: *String) -> int64:
        return self.length

    function __getitem__(self: *String, index: int64) -> int8:
        assert index >= 0 and index < self.length, "String index out of range"
        return self.data[index]

    function __setitem__(self: *String, index: int64, character: int8):
        assert index >= 0 and index < self.length, "String index out of range"
        self.data[index] = character

    function __eq__(self: *String, other: String) -> bool:
        return self.length == other.length and strcmp(self.__str__(), other.__str__()) == 0

    function __ne__(self: *String, other: String) -> bool:
        return not self.__eq__(other)

    function __lt__(self: *String, other: String) -> bool:
        return strcmp(self.__str__(), other.__str__()) < 0

    function __hash__(self: *String) -> uint64:
        return hash(self.__str__())

    // a new String holding both (allocates)
    function __add__(self: *String, other: String) -> String:
        let result = self.copy()
        result.append_string(other)
        return result

    function __contains__(self: *String, part: str) -> bool:
        return strstr(self.__str__(), part) != null

    function equals(self: *String, text: str) -> bool:
        return strcmp(self.__str__(), text) == 0

    function copy(self: *String) -> String:
        let result = String { }
        result.append_bytes(self.__str__(), self.length)
        return result

    // the index of the first `part`, or -1
    function find(self: *String, part: str) -> int64:
        let found = strstr(self.__str__(), part)
        if found == null:
            return -1
        return (found as int64) - (self.data as int64)

    function starts_with(self: *String, prefix: str) -> bool:
        return strncmp(self.__str__(), prefix, strlen(prefix)) == 0

    function ends_with(self: *String, suffix: str) -> bool:
        let count = strlen(suffix) as int64
        if count > self.length:
            return false
        return strcmp(&self.data[self.length - count], suffix) == 0

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
        return atoi(self.__str__())

    function to_float(self: *String) -> float64:
        return atof(self.__str__())

    function clear(self: *String):
        self.length = 0
        if self.data != null:
            self.data[0] = 0

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
