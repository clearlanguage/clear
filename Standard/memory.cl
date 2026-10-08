// memory: explicit heap allocation. Nothing in Clear allocates behind your back, this is how you ask for it.
//
//     import "memory"
//     let numbers = allocate[int](100)
//     defer release(numbers)

declare malloc(size: uint64) -> *int8
declare calloc(count: uint64, size: uint64) -> *int8
declare realloc(pointer: *int8, size: uint64) -> *int8
declare free(pointer: *int8)
declare memcpy(destination: *int8, source: *int8, size: uint64) -> *int8
declare memmove(destination: *int8, source: *int8, size: uint64) -> *int8
declare memset(destination: *int8, value: int32, size: uint64) -> *int8
declare abort()

// `count` zero-initialized values of type T; stops the program if memory runs out
function allocate[T](count: int64) -> *T:
    let memory = calloc(count as uint64, sizeof T)
    if memory == null:
        abort()
    return memory as *T

// grows (or shrinks) an allocation, keeping its contents
function reallocate[T](pointer: *T, count: int64) -> *T:
    let memory = realloc(pointer as *int8, count as uint64 * sizeof T)
    if memory == null:
        abort()
    return memory as *T

function release[T](pointer: *T):
    free(pointer as *int8)

function copy[T](destination: *T, source: *T, count: int64):
    memmove(destination as *int8, source as *int8, count as uint64 * sizeof T)
