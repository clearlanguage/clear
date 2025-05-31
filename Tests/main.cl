declare printf(int8* fmt, ...) -> int
declare scanf(int8* scan, ...) -> int

function main():
    int n
    scanf("%li", &n)

    if n < 0:
        printf("cannot count down with negative!\n")

    while n > 0:
        printf("count down: %i\n", n)
        n--

    int[3] arr = {1, 2, 3}
    int* ptr = &arr[0]
