// string: helpers for C strings (*int8 terminated by a zero byte, the type of "string literals").

declare strlen(text: *int8) -> uint64
declare strcmp(a: *int8, b: *int8) -> int32
declare strncmp(a: *int8, b: *int8, count: uint64) -> int32
declare strchr(text: *int8, character: int32) -> *int8
declare strstr(text: *int8, part: *int8) -> *int8
declare atoi(text: *int8) -> int32
declare atof(text: *int8) -> float64
declare toupper(character: int32) -> int32
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
