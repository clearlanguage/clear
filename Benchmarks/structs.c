#include <stdio.h>
typedef struct { double x, y, z; } Vec3;
static Vec3 add(Vec3 a, Vec3 b) { return (Vec3){ a.x + b.x, a.y + b.y, a.z + b.z }; }
static Vec3 scale(Vec3 a, double k) { return (Vec3){ a.x * k, a.y * k, a.z * k }; }
int main(void) {
    Vec3 p = {0, 0, 0}, v = {1, 2, 3};
    for (int i = 0; i < 100000000; i++) p = add(p, scale(v, 0.000001));
    printf("%.15g %.15g %.15g\n", p.x, p.y, p.z);
    return 0;
}
