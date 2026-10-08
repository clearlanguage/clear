#include <stdio.h>
#include <stdlib.h>
#define N 700
int main(void) {
    double* a = malloc(N * N * 8); double* b = malloc(N * N * 8); double* c = malloc(N * N * 8);
    for (int i = 0; i < N * N; i++) { a[i] = (i % 7) * 0.5; b[i] = (i % 5) * 0.25; c[i] = 0.0; }
    for (int i = 0; i < N; i++)
        for (int k = 0; k < N; k++) {
            double aik = a[i * N + k];
            for (int j = 0; j < N; j++) c[i * N + j] += aik * b[k * N + j];
        }
    double total = 0.0;
    for (int i = 0; i < N * N; i++) total += c[i];
    printf("%.1f\n", total);
    free(a); free(b); free(c);
    return 0;
}
