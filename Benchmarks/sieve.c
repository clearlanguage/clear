#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define LIMIT 50000000
int main(void) {
    bool* composite = calloc(LIMIT + 1, 1);
    int count = 0;
    for (int i = 2; i <= LIMIT; i++) {
        if (!composite[i]) {
            count++;
            for (long long j = (long long)i * i; j <= LIMIT; j += i) composite[j] = true;
        }
    }
    printf("%d\n", count);
    free(composite);
    return 0;
}
