class allocator:
    uint type_size
    int8* pointer

    function __construct__(uint typesize):
        this.type_size = typesize
        this.pointer = null

    function __destruct__():
        this.deallocate()

    function allocate(uint n):
        let size = n * this.type_size
        this.pointer = malloc(size)
        if this.pointer == null:
            print("Allocation failed")

    function deallocate():
        if this.pointer == null:
            print("Freeing null")
            return
        free(this.pointer)
        this.pointer = null


