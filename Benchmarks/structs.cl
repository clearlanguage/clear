class Vec3:
    x: float64
    y: float64
    z: float64

function add(a: Vec3, b: Vec3) -> Vec3:
    return Vec3 { a.x + b.x, a.y + b.y, a.z + b.z }

function scale(a: Vec3, k: float64) -> Vec3:
    return Vec3 { a.x * k, a.y * k, a.z * k }

function main() -> int32:
    let p = Vec3 { 0.0, 0.0, 0.0 }
    let v = Vec3 { 1.0, 2.0, 3.0 }
    for i in 0..100000000:
        p = add(p, scale(v, 0.000001))
    print(p.x, p.y, p.z)
    return 0
