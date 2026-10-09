// io: files and the terminal.
//
//     import "io"
//     let name = input("name? ")
//     write_file("hello.txt", "hi there\n")
//     let text = read_file("hello.txt")
//     if text is not none:
//         print(text.value)
//
//     let file = open("data.txt", "r")
//     if file is not none:
//         let f = file.value
//         defer f.close()
//         while true:
//             let line = f.read_line()
//             if line is none:
//                 break
//             print(line.value)
//
// Text comes back as String (see "string"); the caller frees it.

import "string"

declare fopen(path: *int8, mode: *int8) -> *int8
declare fdopen(descriptor: int32, mode: *int8) -> *int8
declare fclose(file: *int8) -> int32
declare fgetc(file: *int8) -> int32
declare fputs(text: *int8, file: *int8) -> int32
declare fwrite(data: *int8, size: uint64, count: uint64, file: *int8) -> uint64
declare fflush(file: *int8) -> int32
declare remove(path: *int8) -> int32

class File:
    handle: *int8

    function is_open(self: *File) -> bool:
        return self.handle != null

    // the next line without its line break, or none at the end of the file
    function read_line(self: *File) -> ?String:
        return read_line_from(self.handle)

    // everything from here to the end
    function read_all(self: *File) -> String:
        let text = String { }
        let character = fgetc(self.handle)
        while character >= 0:
            text.push(character as int8)
            character = fgetc(self.handle)
        return text

    // any text: a literal, a String, or part of one
    function write(self: *File, text: str):
        fwrite(pointer(text), 1, len(text) as uint64, self.handle)

    function write_string(self: *File, text: str):
        self.write(text)

    function write_line(self: *File, text: str):
        self.write(text)
        self.write("\n")

    function flush(self: *File):
        fflush(self.handle)

    // a File closes itself at the end of its scope (close() closes it sooner)
    operator destruct(self):
        self.close()

    function close(self: *File):
        if self.handle != null:
            fclose(self.handle)
        self.handle = null

// mode as in C: "r" read, "w" write (replace), "a" append, add "b" for binary
// C wants the path to end with a zero byte: a String's text always does (part of a text might not)
function open(path: str, mode: str) -> ?File:
    let terminated = String(path)
    let handle = fopen(terminated.c_str(), String(mode).c_str())
    if handle == null:
        return none
    return File(handle)

function read_file(path: str) -> ?String:
    let file = open(path, "rb")
    if file is none:
        return none
    let f = file.value
    let text = f.read_all()
    f.close()
    return text

function write_file(path: str, text: str) -> bool:
    let file = open(path, "wb")
    if file is none:
        return false
    let f = file.value
    f.write(text)
    f.close()
    return true

function append_file(path: str, text: str) -> bool:
    let file = open(path, "ab")
    if file is none:
        return false
    let f = file.value
    f.write(text)
    f.close()
    return true

function file_exists(path: str) -> bool:
    let terminated = String(path)
    let handle = fopen(terminated.c_str(), "r")
    if handle == null:
        return false
    fclose(handle)
    return true

function delete_file(path: str) -> bool:
    let terminated = String(path)
    return remove(terminated.c_str()) == 0

let standard_input: *int8 = null

function read_line_from(handle: *int8) -> ?String:
    let character = fgetc(handle)
    if character < 0:
        return none
    let line = String { }
    while character >= 0 and character != '\n':
        line.push(character as int8)
        character = fgetc(handle)
    return line

// a line typed by the user (without the line break), or none when input has ended
function read_line() -> ?String:
    if standard_input == null:
        standard_input = fdopen(0, "r")
    return read_line_from(standard_input)     // (a File here would close the terminal when it goes away)

// like Python's input(): shows the prompt, returns the line ("" at the end of input)
function input(prompt: str) -> String:
    print_text(prompt)
    let line = read_line()
    if line is none:
        return String { }
    return line.value

declare printf(format: *int8, args: ...) -> int32

function print_text(text: str):
    printf("%.*s", len(text) as int32, pointer(text))
    fflush(null)
