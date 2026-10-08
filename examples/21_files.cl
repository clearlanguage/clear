import "io"

function main() -> int32:
    let path = "/tmp/clear_example.txt"
    write_file(path, "first line\nsecond line\n")
    append_file(path, "third line\n")

    let file = open(path, "r")
    if file is none:
        print("could not open", path)
        return 1

    let f = file.value
    defer f.close()
    while true:
        let line = f.read_line()
        if line is none:
            break
        print("read:", line.value)

    let all = read_file(path)
    print(len(all.value), "bytes")
    delete_file(path)
    print(file_exists(path))

    // input("name? ") would read a line from the keyboard
    return 0

// expect:
// read: first line
// read: second line
// read: third line
// 34 bytes
// false
