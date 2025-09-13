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
let sum = 5 + 3
let product = 4 * 2
let isEqual = (sum == product)
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
let first = nums[0]
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
```

* Classes

```
class Person:
    name: string   // no `let` needed inside class

    function greet(*self):
        print("Hello ", self.name)

let p = Person { "Bob" }
p.greet() // outputs "Hello Bob"
```

* Generic Classes

```
class Person[T]:
    name: T

let p1 = Person { "Bob" } //produced Person[string]
let p2 = Person { 5 }  //produced Person[int]
```

* Ternary Expressions

```
let message = when a >= 0 use "positive" otherwise "negative"
```

* Casting

```
let x = 2.4 as int // 2 
```

* Imports

```
import "file.cl" // import all symbols into this file
import "file.cl" as file // import all symbols into this file under the file namespace
```

---

## 🚧 Planned Features

* Switch Statements
* Optionals
* Macros
* Generics
* Unions
* Variants
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
