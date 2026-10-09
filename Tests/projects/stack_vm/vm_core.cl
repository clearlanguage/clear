// test-helper
// vm_core: a tiny stack VM (text assembly -> List[Instr] -> execute), used by stack_vm.cl

enum Op:
    Push
    Pop
    Dup
    Swap
    Over
    Add
    Sub
    Mul
    Div
    Mod
    Lt
    Eq
    Jmp
    Jz
    Jnz
    Load
    Store
    Print
    Call
    Ret
    Halt

class Instr:
    op: Op
    arg: int64 = 0

    operator str(self) -> String:
        return OP_NAMES[self.op as int] + " " + from_int(self.arg)

class Program:
    code: List[Instr]
    errors: List[String]

const OP_NAMES: [21; str] = {"push", "pop", "dup", "swap", "over", "add", "sub", "mul", "div", "mod", "lt", "eq", "jmp", "jz", "jnz", "load", "store", "print", "call", "ret", "halt"}

function op_from(name: str) -> ?Op:
    for i in 0..len(OP_NAMES):
        if OP_NAMES[i] == name:
            return i as Op
    return none

function needs_label(op: Op) -> bool:
    return op == Op.Jmp or op == Op.Jz or op == Op.Jnz or op == Op.Call

// split a line into tokens, dropping ';' comments
function tokens(line: str) -> List[str]:
    let out = List[str]()
    let start: int64 = -1
    for i in 0..len(line):
        let c = line[i]
        if c == ';':
            if start >= 0:
                out.push(line[start:i])
            return out
        let space = c == ' ' or c == '\t' or c == '\r'
        if space and start >= 0:
            out.push(line[start:i])
            start = -1
        else if not space and start < 0:
            start = i
    if start >= 0:
        out.push(line[start:])
    return out

function lines(text: str) -> Generator[str]:
    let start: int64 = 0
    for i in 0..len(text):
        if text[i] == '\n':
            yield text[start:i]
            start = i + 1
    if start < len(text):
        yield text[start:]

function is_number(t: str) -> bool:
    if len(t) == 0:
        return false
    let first: int64 = when t[0] == '-' use 1 otherwise 0
    if first == len(t):
        return false
    for i in first..len(t):
        if t[i] < '0' or t[i] > '9':
            return false
    return true

function parse_int(t: str) -> int64:
    let neg = t[0] == '-'
    let v: int64 = 0
    for i in (when neg use 1 otherwise 0)..len(t):
        v = v * 10 + (t[i] - '0') as int64
    return when neg use -v otherwise v

function assemble(source: str) -> Program:
    let prog = Program { }
    let labels = Map[str, int64]()
    let fixups = List[(int64, str, int)]()
    let line_no = 0
    for line in lines(source):
        line_no++
        let toks = tokens(line)
        if len(toks) == 0:
            continue
        let head = toks[0]
        if head[len(head) - 1] == ':':
            labels[head[:len(head) - 1]] = len(prog.code)
            continue
        let op = op_from(head)
        if op is none:
            prog.errors.push("line " + from_int(line_no) + ": unknown op " + head)
            continue
        let ins = Instr(op.value)
        if len(toks) > 1:
            if is_number(toks[1]):
                ins.arg = parse_int(toks[1])
            else if needs_label(op.value):
                fixups.push((len(prog.code), toks[1], line_no))
            else:
                ins.arg = 0
        prog.code.push(ins)
    for f in fixups:
        let at, name, ln = f
        let target = labels.get(name)
        if target:
            prog.code[at].arg = target
        else:
            prog.errors.push("line " + from_int(ln) + ": unknown label " + name)
    return prog

function execute(prog: *Program, trace: bool = false) -> List[int64]:
    let out = List[int64]()
    let stack = List[int64]()
    let calls = List[int64]()
    let memory: [16; int64] = {}
    let pc: int64 = 0
    let steps = 0
    while pc < len(prog.code):
        let ins = prog.code[pc]
        pc++
        steps++
        switch ins.op:
            case Op.Push:
                stack.push(ins.arg)
            case Op.Pop:
                stack.pop()
            case Op.Dup:
                stack.push(stack.last())
            case Op.Swap:
                let b = stack.pop()
                let a = stack.pop()
                stack.push(b)
                stack.push(a)
            case Op.Over:
                stack.push(stack[len(stack) - 2])
            case Op.Add, Op.Sub, Op.Mul, Op.Div, Op.Mod, Op.Lt, Op.Eq:
                let b = stack.pop()
                let a = stack.pop()
                let r: int64 = 0
                switch ins.op:
                    case Op.Add:
                        r = a + b
                    case Op.Sub:
                        r = a - b
                    case Op.Mul:
                        r = a * b
                    case Op.Div:
                        r = a / b
                    case Op.Mod:
                        r = a % b
                    case Op.Lt:
                        r = when a < b use 1 otherwise 0
                    default:
                        r = when a == b use 1 otherwise 0
                stack.push(r)
            case Op.Jmp:
                pc = ins.arg
            case Op.Jz:
                if stack.pop() == 0:
                    pc = ins.arg
            case Op.Jnz:
                if stack.pop() != 0:
                    pc = ins.arg
            case Op.Load:
                stack.push(memory[ins.arg])
            case Op.Store:
                memory[ins.arg] = stack.pop()
            case Op.Print:
                out.push(stack.pop())
            case Op.Call:
                calls.push(pc)
                pc = ins.arg
            case Op.Ret:
                pc = calls.pop()
            case Op.Halt:
                break
    if trace:
        print("steps:", steps, "stack depth:", len(stack))
    return out

