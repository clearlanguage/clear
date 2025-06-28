declare malloc(int size) -> int8*
declare free(int8* ptr)
declare printf(string, ...) -> int

class allocator:
    uint type_size
    int8* pointer

    function __construct__(uint typesize):
        this.type_size = typesize

