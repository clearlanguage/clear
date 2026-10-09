// A stack VM split over two modules and used through an alias: an enum, a class with
// operator str returning a String, List[Instr] / List[String] built in the helper and
// read here (last(), slices, printing), a generator over lines, Map[str, int64] labels,
// tuples in a List, and switch with break out of the loop.
import "vm_core" as vm

const FIB = "
    push 0
    store 0      ; a
    push 1
    store 1      ; b
    push 15
    store 2      ; n
loop:
    load 2
    jz done
    load 0
    print
    load 0
    load 1
    add
    load 1
    store 0
    store 1
    load 2
    push 1
    sub
    store 2
    jmp loop
done:
    halt
"

const PRIMES = "
    push 2
    store 0          ; candidate
outer:
    load 0
    push 60
    lt
    jz end
    load 0
    call isprime
    jz next
    load 0
    print
next:
    load 0
    push 1
    add
    store 0
    jmp outer
end:
    halt
; isprime(n) -> 0/1 ; uses mem 1 (n) and 2 (d)
isprime:
    store 1
    push 2
    store 2
check:
    load 2
    load 2
    mul
    load 1
    swap
    lt               ; n < d*d ?
    jnz yes
    load 1
    load 2
    mod
    jz no
    load 2
    push 1
    add
    store 2
    jmp check
yes:
    push 1
    ret
no:
    push 0
    ret
"

// 10! with a counting loop, then code after halt that must not run
const FACT = "
    push 1
    store 0
    push 10
    store 1
again:
    load 0
    load 1
    mul
    store 0
    load 1
    push 1
    sub
    dup
    store 1
    jnz again
    load 0
    print
    halt
    push 99
    print
"

const BROKEN = "
    push 1
    frobnicate 3
    jmp nowhere
"

function show(name: str, source: str):
    let prog = vm.assemble(source)
    if not prog.errors.is_empty():
        print(name, "failed:", prog.errors)
        return
    print(name, "first:", prog.code[0], "last:", prog.code.last())
    print(prog.code[0:2], len(prog.code))
    let out = vm.execute(&prog, trace = true)
    print(name, out)

function main() -> int32:
    show("fib", FIB)
    show("primes", PRIMES)
    show("fact", FACT)
    show("broken", BROKEN)
    let ins = vm.Instr(vm.Op.Jmp, 7)
    print(ins, ins.op == vm.Op.Jmp, vm.needs_label(ins.op), vm.op_from("swap") ?? vm.Op.Halt)
    let toks = vm.tokens("  push   -12 ; comment")
    print(toks, vm.is_number(toks[1]), vm.parse_int(toks[1]), vm.is_number("-"), vm.is_number("4x"))
    return 0

// expect:
// fib first: push 0 last: halt 0
// [push 0, store 0] 22
// steps: 234 stack depth: 0
// fib [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
// primes first: push 2 last: ret 0
// [push 2, store 0] 40
// steps: 2845 stack depth: 0
// primes [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59]
// fact first: push 1 last: print 0
// [push 1, store 0] 19
// steps: 107 stack depth: 0
// fact [3628800]
// broken failed: [line 3: unknown op frobnicate, line 4: unknown label nowhere]
// jmp 7 true true Op.Swap
// [push, -12] true -12 false false
