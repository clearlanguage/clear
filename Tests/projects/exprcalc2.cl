// Tokenizer + recursive-descent expression evaluator, written the natural way:
// a recursive AST enum (children held in Lists), String == str, += on Strings, switch on str.
import "math"

enum TokKind:
    Number
    Ident
    Op
    LParen
    RParen
    Comma
    End

class Token:
    text: String
    num: float64
    pos: int64
    kind: TokKind

enum Expr:
    Num(value: float64)
    Var(name: String)
    Neg(inner: List[Expr])
    Bin(op: str, sides: List[Expr])
    Call(name: String, args: List[Expr])

function is_digit(c: int8) -> bool:
    return c >= '0' and c <= '9'

function is_alpha(c: int8) -> bool:
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_'

function tokenize(src: str) -> ?List[Token]:
    let toks = List[Token]()
    let i: int64 = 0
    let n = len(src)
    while i < n:
        let c = src[i]
        if c == ' ':
            i += 1
            continue
        if is_digit(c) or c == '.':
            let start = i
            while i < n and (is_digit(src[i]) or src[i] == '.'):
                i += 1
            let t = String(src[start:i])
            toks.push(Token { t, t.to_float(), start, TokKind.Number })
        else if is_alpha(c):
            let start = i
            while i < n and (is_alpha(src[i]) or is_digit(src[i])):
                i += 1
            toks.push(Token { String(src[start:i]), 0.0, start, TokKind.Ident })
        else if c == '*' and i + 1 < n and src[i + 1] == '*':
            toks.push(Token { "**", 0.0, i, TokKind.Op })
            i += 2
        else if c == '+' or c == '-' or c == '*' or c == '/':
            toks.push(Token { String(src[i:i + 1]), 0.0, i, TokKind.Op })
            i += 1
        else if c == '(':
            toks.push(Token { "(", 0.0, i, TokKind.LParen })
            i += 1
        else if c == ')':
            toks.push(Token { ")", 0.0, i, TokKind.RParen })
            i += 1
        else if c == ',':
            toks.push(Token { ",", 0.0, i, TokKind.Comma })
            i += 1
        else:
            print("error: unexpected character at", i)
            return none
    toks.push(Token { "<end>", 0.0, n, TokKind.End })
    return toks

function pair(a: Expr, b: Expr) -> List[Expr]:
    let l = List[Expr]()
    l.push(a)
    l.push(b)
    return l

function one(a: Expr) -> List[Expr]:
    let l = List[Expr]()
    l.push(a)
    return l

class Parser:
    toks: List[Token]
    pos: int64 = 0
    error: String

    function peek(self) -> *Token:
        return &self.toks[self.pos]

    function fail(self, msg: str) -> ?Expr:
        if len(self.error) == 0:
            self.error = msg + " at column " + from_int(self.peek().pos)
        return none

    function is_op(self, op: str) -> bool:
        let t = self.peek()
        return t.kind == TokKind.Op and t.text == op

    // expr := term (('+'|'-') term)*
    function expr(self) -> ?Expr:
        let left = self.term()
        if not left:
            return none
        while self.is_op("+") or self.is_op("-"):
            let op: str = when self.is_op("+") use "+" otherwise "-"
            self.pos += 1
            let right = self.term()
            if not right:
                return none
            left = Expr.Bin(op, pair(left, right))
        return left

    function term(self) -> ?Expr:
        let left = self.unary()
        if not left:
            return none
        while self.is_op("*") or self.is_op("/"):
            let op: str = when self.is_op("*") use "*" otherwise "/"
            self.pos += 1
            let right = self.unary()
            if not right:
                return none
            left = Expr.Bin(op, pair(left, right))
        return left

    function unary(self) -> ?Expr:
        if self.is_op("-"):
            self.pos += 1
            let inner = self.unary()
            if not inner:
                return none
            return Expr.Neg(one(inner))
        return self.power()

    function power(self) -> ?Expr:
        let base = self.primary()
        if not base:
            return none
        if self.is_op("**"):
            self.pos += 1
            let exp = self.unary()
            if not exp:
                return none
            return Expr.Bin("**", pair(base, exp))
        return base

    function primary(self) -> ?Expr:
        let t = self.peek()
        switch t.kind:
            case TokKind.Number:
                self.pos += 1
                return Expr.Num(t.num)
            case TokKind.Ident:
                let name = t.text
                self.pos += 1
                if self.peek().kind != TokKind.LParen:
                    return Expr.Var(name)
                self.pos += 1
                let args = List[Expr]()
                if self.peek().kind != TokKind.RParen:
                    while true:
                        let a = self.expr()
                        if not a:
                            return none
                        args.push(a)
                        if self.peek().kind != TokKind.Comma:
                            break
                        self.pos += 1
                if self.peek().kind != TokKind.RParen:
                    return self.fail("expected ')'")
                self.pos += 1
                return Expr.Call(name, args)
            case TokKind.LParen:
                self.pos += 1
                let inner = self.expr()
                if not inner:
                    return none
                if self.peek().kind != TokKind.RParen:
                    return self.fail("expected ')'")
                self.pos += 1
                return inner
            default:
                return self.fail("unexpected token")

function eval(e: *Expr, env: *Map[String, float64]) -> ?float64:
    switch *e:
        case Num(v):
            return v
        case Var(name):
            return env.get(name)
        case Neg(inner):
            let v = eval(&inner[0], env)
            if not v:
                return none
            return -v
        case Bin(op, sides):
            let a = eval(&sides[0], env)
            let b = eval(&sides[1], env)
            if not a or not b:
                return none
            switch op:
                case "+":
                    return a + b
                case "-":
                    return a - b
                case "*":
                    return a * b
                case "/":
                    if b == 0.0:
                        return none
                    return a / b
                default:
                    return a ** b
        case Call(name, args):
            let vals = List[float64]()
            for a in args:
                let v = eval(&a, env)
                if not v:
                    return none
                vals.push(v)
            switch name:
                case "max":
                    if len(vals) == 2:
                        return max(vals[0], vals[1])
                case "min":
                    if len(vals) == 2:
                        return min(vals[0], vals[1])
                case "sqrt":
                    if len(vals) == 1:
                        return sqrt(vals[0])
                case "abs":
                    if len(vals) == 1:
                        return abs(vals[0])
                default:
                    pass
            return none

function show(e: *Expr) -> String:
    switch *e:
        case Num(v):
            return from_float(v)
        case Var(name):
            return name
        case Neg(inner):
            return "(-" + show(&inner[0]) + ")"
        case Bin(op, sides):
            return "(" + show(&sides[0]) + " " + op + " " + show(&sides[1]) + ")"
        case Call(name, args):
            let s = name + "("
            for i in 0..len(args):
                if i > 0:
                    s += ", "
                s += show(&args[i])
            s += ")"
            return s

function evaluate(src: str, env: *Map[String, float64]) -> ?float64:
    let toks = tokenize(src)
    if not toks:
        return none
    let p = Parser { toks }
    let root = p.expr()
    if root and p.peek().kind != TokKind.End:
        p.fail("trailing input")
        root = none
    if not root:
        print("parse error:", p.error)
        return none
    print("  tree:", show(&root))
    return eval(&root, env)

variant Outcome:
    float64
    str

function lines_of(script: str) -> Generator[str]:
    let start: int64 = 0
    for i in 0..len(script):
        if script[i] == ';':
            yield script[start:i]
            start = i + 1
    if start < len(script):
        yield script[start:]

function main() -> int32:
    let env = Map[String, float64]()
    env["x"] = 3.0
    env["y"] = 4.0
    env["pi"] = PI
    let cases = ["1 + 2 * 3", "(1 + 2) * 3", "-2 ** 2", "2 ** 3 ** 2", "x * x + y * y", "sqrt(x*x + y*y)", "max(x, y) - min(x, -y)", "10 / 4", "1 / 0", "2 * (3 + ", "foo + 1", "abs(-7.5) + 1.5", "1 - 2 - 3", "min(1)"]
    for c in cases:
        let r = evaluate(c, &env)
        if r:
            print(c, "=", r)
        else:
            print(c, "=> error")
    let results = Map[String, Outcome]()
    let order = List[String]()
    for line in lines_of("a = 2; b = a ** 10; c = b / (a - 2); d = b - 24"):
        let eq = String(line).find("=")
        let name = String(line[:eq]).strip()
        let r = evaluate(line[eq + 1:], &env)
        if r:
            env[name] = r
            results[name] = r
        else:
            results[name] = "error"
        order.push(name)
    let toks = tokenize("-x * f(2)")
    if toks:
        let p = Parser { toks }
        print(p.expr())
    for k in order:
        switch results[k]:
            case float64(v):
                print(k, "->", v)
            case str(msg):
                print(k, "->", msg)
    return 0

// expect:
//   tree: (1.0 + (2.0 * 3.0))
// 1 + 2 * 3 = 7.0
//   tree: ((1.0 + 2.0) * 3.0)
// (1 + 2) * 3 = 9.0
//   tree: (-(2.0 ** 2.0))
// -2 ** 2 = -4.0
//   tree: (2.0 ** (3.0 ** 2.0))
// 2 ** 3 ** 2 = 512.0
//   tree: ((x * x) + (y * y))
// x * x + y * y = 25.0
//   tree: sqrt(((x * x) + (y * y)))
// sqrt(x*x + y*y) = 5.0
//   tree: (max(x, y) - min(x, (-y)))
// max(x, y) - min(x, -y) = 8.0
//   tree: (10.0 / 4.0)
// 10 / 4 = 2.5
//   tree: (1.0 / 0.0)
// 1 / 0 => error
// parse error: unexpected token at column 9
// 2 * (3 +  => error
//   tree: (foo + 1.0)
// foo + 1 => error
//   tree: (abs((-7.5)) + 1.5)
// abs(-7.5) + 1.5 = 9.0
//   tree: ((1.0 - 2.0) - 3.0)
// 1 - 2 - 3 = -4.0
//   tree: min(1.0)
// min(1) => error
//   tree: 2.0
//   tree: (a ** 10.0)
//   tree: (b / (a - 2.0))
//   tree: (b - 24.0)
// Expr.Bin(op=*, sides=[Expr.Neg(inner=[Expr.Var(name=x)]), Expr.Call(name=f, args=[Expr.Num(value=2.0)])])
// a -> 2.0
// b -> 1024.0
// c -> error
// d -> 1000.0
