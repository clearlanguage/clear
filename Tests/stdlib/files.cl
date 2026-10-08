import "io"

function main() -> int32:
    let path = "/tmp/clear_io_test.txt"
    print(write_file(path, "first line\nsecond line\n"), append_file(path, "third\n"))

    let text = read_file(path)
    print(len(text.value), text.value.starts_with("first"))

    let file = open(path, "r")
    let f = file.value
    let count = 0
    while true:
        let line = f.read_line()
        if line is none:
            break
        count += 1
        print(count, line.value)
    f.close()

    print(file_exists(path), delete_file(path), file_exists(path))
    print(open("/nonexistent/dir/file", "r") is none, read_file("/nonexistent") is none)

    let name = input("name? ")
    print("hello", len(name))
    let after = read_line()
    print(after is none)
    return 0

// expect:
// true true
// 29 true
// 1 first line
// 2 second line
// 3 third
// true true false
// true true
// name? hello 0
// true
