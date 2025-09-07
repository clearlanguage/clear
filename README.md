---

# Clear Programming Language

Clear is a modern **compiled programming language** that combines the **simplicity of Python** with the **performance and control of low-level languages**. Designed with clarity, safety, and flexibility in mind, Clear is ideal for building high-performance applications—ranging from games and simulations to real-time trading systems and beyond.

---

## 📦 Build Instructions

### Requirements

* Clang compiler with C++23 support (Clang 16 or later recommended)
* CMake (version ≥ 3.20.0 recommended)
* LLVM (built from source with CMake installed) → See the [LLVM CMake build guide](https://llvm.org/docs/CMake.html) for setup instructions.
* Linux or macOS (currently only tested on these platforms)

### Building

* First clone the repository

```
git clone https://github.com/clearlanguage/clear.git
```

* Create a build directory

```
mkdir build
cd build
```

* Use IDE CMake integration or call cmake directly

```
cmake path/to/clearc
cmake --build .
```

---

## 🌍 Open Source

Clear is open source and welcomes contributions!

---

## Roadmap

### ✅ Completed Features

* Variables

```
let a: int = 5
let y: float = 2.0
let name: const *char = "alice"
```

* Functions

```
function add(a: int, b: int) -> int:
    return a + b

function main():
    let result: int = add(5, 10)
```

* Operators

```
let sum: int = 5 + 3
let product: int = 4 * 2
let isEqual: bool = (sum == product)
```

* Pointers / Dereferencing

```
let x: int = 42
let ptr: *int = &x    // pointer to x
let y: int = *ptr     // dereference ptr
```

* Arrays

```
let nums: [5; int] = {1, 2, 3, 4, 5}
let first: int = nums[0]
```

* If / ElseIf / Else

```
if x > 10:
    print("Greater than 10")
elseif x == 10:
    print("Exactly 10")
else:
    print("Less than 10")
```

* While loops

```
let i: int = 0
while i < 5:
    print(i)
    i++
```

* Type Inference

```
let i = 0          // inferred int
let const number = 2.0 // inferred const float
```

* Structs

```
struct Point:
    x: float
    y: float
    z: float

let p: Point = { 1.0, 2.0 }
```

* Classes

```
class Person:
    name: string   // no `let` needed inside class

    function greet():
        print("Hello ", this.name)

let p: Person = { "Bob" }
p.greet() // outputs "Hello Bob"
```

* Traits

```
trait Drawable:
    function draw()

class Circle:
    function draw():
        print("Drawing Circle")

function draw_object(drawable: Drawable):
    drawable.draw()

let c: Circle
draw_object(c)
```

* Function Overloading

```
function _print(a: int64):
    printf("%i", a)

function _print(a: float64):
    printf("%d", a)
```

* Variadic Arguments

```
function println(args...):
    for arg in args:
        _print(arg)
    printf("\n")
```

* Defer

```
function test():
    defer print("Cleaning up!")
    print("Doing work...")
    // "Cleaning up!" will print when leaving this scope
```

* Named Blocks

```
outer:
    defer print("leaving outer")
    print("Inside outer")

    inner:
        defer print("leaving inner")
        print("Inside inner")
```

* Switch Statements

```
let a: int = ...
switch a:
    case 20, 30, 40:
        print("a was either 20, 30, or 40")
    default:
        print("not a valid input")
```

* Enums

```
enum Direction:
    Left, Right, Up, Down 

print(Direction.Left)   // 0
print(Direction.Right)  // 1
print(Direction.Up)     // 2
```

* Optionals

```
let x: ?int

if x != null:
    print(?x)   // unwrap optional
```

---

## 🚧 Planned Features

* Unions
* Variants
* Generics
* Modules
* Inheritance
* Advanced Enums
* Unpack Operator
* Operator Overloading
* Properties
* Async/await
* Package Manager
* Lambdas
* Compile-time safety checks (indexing, dereferencing, etc.)
* Optional runtime safety checks

---
