import { test } from "node:test";
import * as assert from "node:assert/strict";
import * as fs from "fs";
import * as path from "path";
import * as oniguruma from "vscode-oniguruma";
import * as textmate from "vscode-textmate";

const ROOT = path.resolve(__dirname, "..", "..");
const REPO = path.resolve(ROOT, "..", "..");

async function loadGrammar(): Promise<textmate.IGrammar> {
    const wasm = fs.readFileSync(require.resolve("vscode-oniguruma/release/onig.wasm")).buffer;
    await oniguruma.loadWASM(wasm);

    const registry = new textmate.Registry({
        onigLib: Promise.resolve({
            createOnigScanner: (patterns: string[]) => new oniguruma.OnigScanner(patterns),
            createOnigString: (s: string) => new oniguruma.OnigString(s),
        }),
        loadGrammar: async (scopeName) => {
            if (scopeName !== "source.clear") return null;
            const file = path.join(ROOT, "syntaxes", "clear.tmLanguage.json");
            return textmate.parseRawGrammar(fs.readFileSync(file, "utf8"), file);
        },
    });

    return (await registry.loadGrammar("source.clear"))!;
}

/** token text -> its innermost scope, for one or more lines */
async function tokens(source: string): Promise<[string, string][]> {
    const grammar = await loadGrammar();
    let state = textmate.INITIAL;
    const result: [string, string][] = [];

    for (const line of source.split("\n")) {
        const lineTokens = grammar.tokenizeLine(line, state);
        for (const token of lineTokens.tokens) {
            const text = line.slice(token.startIndex, token.endIndex);
            if (text.trim() !== "") result.push([text.trim(), token.scopes[token.scopes.length - 1]]);
        }
        state = lineTokens.ruleStack;
    }

    return result;
}

function scopeOf(all: [string, string][], text: string, nth = 0): string | undefined {
    return all.filter(([t]) => t === text)[nth]?.[1];
}

test("declarations", async () => {
    const t = await tokens(`async function fetch[T: Shape](url: str, retries: int = 3) -> Task[T]:
class Dog(Animal, Named):
operator add(self, other: Vec2) -> Vec2:
property name(self) -> str:
declare printf(format: *int8, args: ...) -> int32
macro square(x):
enum Color:
variant Number:`);

    assert.equal(scopeOf(t, "async"), "storage.modifier.async.clear");
    assert.equal(scopeOf(t, "fetch"), "entity.name.function.clear");
    assert.equal(scopeOf(t, "T"), "entity.name.type.parameter.clear");
    assert.equal(scopeOf(t, "Shape"), "entity.name.type.trait.clear");
    assert.equal(scopeOf(t, "url"), "variable.parameter.clear");
    assert.equal(scopeOf(t, "str"), "support.type.primitive.clear");
    assert.equal(scopeOf(t, "3"), "constant.numeric.integer.clear");
    assert.equal(scopeOf(t, "Task"), "support.type.clear");
    assert.equal(scopeOf(t, "Dog"), "entity.name.type.class.clear");
    assert.equal(scopeOf(t, "Animal"), "entity.other.inherited-class.clear");
    assert.equal(scopeOf(t, "add"), "entity.name.function.operator.clear");
    assert.equal(scopeOf(t, "self"), "variable.language.self.clear");
    assert.equal(scopeOf(t, "Vec2", 1), "entity.name.type.clear");
    assert.equal(scopeOf(t, "name"), "entity.name.function.property.clear");
    assert.equal(scopeOf(t, "printf"), "entity.name.function.extern.clear");
    assert.equal(scopeOf(t, "*"), "keyword.operator.type.clear");
    assert.equal(scopeOf(t, "square"), "entity.name.function.macro.clear");
    assert.equal(scopeOf(t, "Color"), "entity.name.type.enum.clear");
    assert.equal(scopeOf(t, "Number"), "entity.name.type.variant.clear");
});

test("statements and expressions", async () => {
    const t = await tokens(`    let label = when v > 3 use "large\\n" otherwise 'x'
    for i in 0..=10:
    if found := find(data, 9):
    print(square!(7), numbers.length, user?.name, 0x1F, 2.5e-3)
    const LIMIT = 4  // TODO: more
    /* block
       comment */
    x as float64
    n is not none`);

    assert.equal(scopeOf(t, "let"), "storage.type.variable.clear");
    assert.equal(scopeOf(t, "when"), "keyword.control.conditional.clear");
    assert.equal(scopeOf(t, "otherwise"), "keyword.control.conditional.clear");
    assert.equal(scopeOf(t, "\\n"), "constant.character.escape.clear");
    assert.equal(scopeOf(t, "'x'"), "string.quoted.single.clear");
    assert.equal(scopeOf(t, "..="), "keyword.operator.range.clear");
    assert.equal(scopeOf(t, ":="), "keyword.operator.assignment.walrus.clear");
    assert.equal(scopeOf(t, "find"), "entity.name.function.call.clear");
    assert.equal(scopeOf(t, "print"), "support.function.builtin.clear");
    assert.equal(scopeOf(t, "square"), "entity.name.function.macro.clear");
    assert.equal(scopeOf(t, "length"), "variable.other.property.clear");
    assert.equal(scopeOf(t, "?."), "punctuation.accessor.clear");
    assert.equal(scopeOf(t, "0x1F"), "constant.numeric.hex.clear");
    assert.equal(scopeOf(t, "2.5e-3"), "constant.numeric.float.clear");
    assert.equal(scopeOf(t, "LIMIT"), "constant.other.clear");
    assert.equal(scopeOf(t, "TODO"), "keyword.other.todo.clear");
    assert.equal(scopeOf(t, "comment"), "comment.block.clear");
    assert.equal(scopeOf(t, "float64"), "support.type.primitive.clear");
    assert.equal(scopeOf(t, "none"), "constant.language.null.clear");
});

test("class bodies: fields and methods", async () => {
    const t = await tokens(`class Account:
    owner: str
    balance: float64 = 0.0
    function deposit(self, amount: float64):
        self.balance += amount
        else:
        default:`);

    assert.equal(scopeOf(t, "owner"), "variable.other.member.declaration.clear");
    assert.equal(scopeOf(t, "balance"), "variable.other.member.declaration.clear");
    assert.equal(scopeOf(t, "deposit"), "entity.name.function.clear");
    assert.equal(scopeOf(t, "+="), "keyword.operator.assignment.compound.clear");
    assert.equal(scopeOf(t, "else"), "keyword.control.conditional.clear");
    assert.equal(scopeOf(t, "default"), "keyword.control.conditional.clear");
});

test("every example tokenizes with nothing left open at the end of a line", async () => {
    const grammar = await loadGrammar();
    const examples = fs.readdirSync(path.join(REPO, "examples")).filter((f) => f.endsWith(".cl"));
    assert.ok(examples.length > 20);

    for (const example of examples) {
        let state = textmate.INITIAL;
        let inBlockComment = false;
        fs.readFileSync(path.join(REPO, "examples", example), "utf8").split("\n").forEach((line, index) => {
            state = grammar.tokenizeLine(line, state).ruleStack;
            inBlockComment = (inBlockComment || line.includes("/*")) && !line.includes("*/");
            if (inBlockComment) return;
            const probe = grammar.tokenizeLine("x", state).tokens[0].scopes;
            assert.deepEqual(probe, ["source.clear", "variable.other.clear"], `${example}:${index + 1} leaves ${probe.join(" ")} open`);
        });
    }
});
