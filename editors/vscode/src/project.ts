// What a file can see: its own declarations, the modules it imports (resolved the way clearc
// resolves them), the standard library's String/List/Map, and the types of expressions.

import * as fs from "fs";
import * as path from "path";
import {
    ClearSymbol, ParsedFile, ImportInfo, parse, baseType, substitute, splitTopLevel, localsAt,
    enclosingType, isCallable, isTypeKind, isPrimitive,
} from "./analyzer";

export interface ProjectOptions {
    /** the standard library folder, if one was found */
    standardLibrary(): string | undefined;
    /** the text of a file that is open in the editor (it may be unsaved) */
    openDocument(file: string): { text: string; version: number } | undefined;
}

export interface Located {
    symbol: ClearSymbol;
    file: string;
}

export interface ModuleRef {
    info?: ImportInfo;
    file: string;
    parsed: ParsedFile;
}

/** Members every value of these types has, without a declaration in a .cl file. */
const SYNTHETIC_MEMBERS: Record<string, { name: string; detail: string; doc: string; returnType?: string }[]> = {
    optional: [
        { name: "value", detail: "value: T", doc: "The value inside. Reading it when the optional is `none` stops the program.", returnType: "T" },
        { name: "value_or", detail: "function value_or(default: T) -> T", doc: "The value inside, or `default` when it is `none`. Same as `x ?? default`.", returnType: "T" },
    ],
    Generator: [
        { name: "advance", detail: "function advance() -> bool", doc: "Runs the generator to its next `yield`. True when a new value is ready.", returnType: "bool" },
        { name: "value", detail: "function value() -> T", doc: "A copy of the value the generator last yielded.", returnType: "T" },
        { name: "done", detail: "function done() -> bool", doc: "True when the generator has finished.", returnType: "bool" },
        { name: "free", detail: "function free()", doc: "Frees the generator early (it is also cleaned up at the end of its scope)." },
    ],
    Task: [
        { name: "run", detail: "function run() -> T", doc: "Runs the task to the end and returns its result.", returnType: "T" },
        { name: "resume", detail: "function resume()", doc: "Runs the task until it next pauses." },
        { name: "done", detail: "function done() -> bool", doc: "True when the task has finished.", returnType: "bool" },
        { name: "result", detail: "function result() -> T", doc: "The result of a finished task.", returnType: "T" },
        { name: "free", detail: "function free()", doc: "Frees the task early (it is also cleaned up at the end of its scope)." },
    ],
};

export function syntheticMembers(kind: string): { name: string; detail: string; doc: string; returnType?: string }[] {
    return SYNTHETIC_MEMBERS[kind] ?? [];
}

const PRELUDE = ["string", "list", "map"];

export class Project {
    private cache = new Map<string, { key: string; parsed: ParsedFile }>();

    constructor(private options: ProjectOptions) {}

    invalidate(file?: string) {
        if (file) this.cache.delete(path.resolve(file));
        else this.cache.clear();
    }

    parseText(file: string, text: string, version: number): ParsedFile {
        const key = `open:${version}`;
        const cached = this.cache.get(path.resolve(file));
        if (cached && cached.key === key) return cached.parsed;
        const parsed = parse(text);
        this.cache.set(path.resolve(file), { key, parsed });
        return parsed;
    }

    parseFile(file: string): ParsedFile | undefined {
        file = path.resolve(file);
        const open = this.options.openDocument(file);
        if (open) return this.parseText(file, open.text, open.version);

        let stat: fs.Stats;
        try {
            stat = fs.statSync(file);
        } catch {
            return undefined;
        }

        const key = `disk:${stat.mtimeMs}:${stat.size}`;
        const cached = this.cache.get(file);
        if (cached && cached.key === key) return cached.parsed;

        try {
            const parsed = parse(fs.readFileSync(file, "utf8"));
            this.cache.set(file, { key, parsed });
            return parsed;
        } catch {
            return undefined;
        }
    }

    // ---------------------------------------------------------------- imports

    /** The closest folder (this one or a parent) with a clear.toml. */
    projectRoot(file: string): string | undefined {
        let dir = path.dirname(path.resolve(file));
        for (;;) {
            if (fs.existsSync(path.join(dir, "clear.toml"))) return dir;
            const parent = path.dirname(dir);
            if (parent === dir) return undefined;
            dir = parent;
        }
    }

    /** name -> { directory, entry } for the dependencies in clear.toml */
    packages(root: string): Map<string, { directory: string; entry: string }> {
        const result = new Map<string, { directory: string; entry: string }>();
        let manifest: string;
        try {
            manifest = fs.readFileSync(path.join(root, "clear.toml"), "utf8");
        } catch {
            return result;
        }

        let section = "";
        for (const raw of manifest.split(/\r?\n/)) {
            const line = raw.replace(/#.*$/, "").trim();
            const header = /^\[([^\]]+)\]$/.exec(line);
            if (header) {
                section = header[1].trim();
                continue;
            }
            if (section !== "dependencies") continue;

            const dependency = /^([A-Za-z0-9_-]+)\s*=\s*\{(.*)\}$/.exec(line);
            if (!dependency) continue;

            const localPath = /path\s*=\s*"([^"]*)"/.exec(dependency[2]);
            const directory = localPath ? path.resolve(root, localPath[1]) : path.join(root, ".clear", "packages", dependency[1]);
            result.set(dependency[1], { directory, entry: this.libraryFile(directory, dependency[1]) });
        }

        return result;
    }

    private libraryFile(directory: string, name: string): string {
        let lib: string | undefined;
        let main = "main.cl";
        try {
            const manifest = fs.readFileSync(path.join(directory, "clear.toml"), "utf8");
            lib = /^\s*lib\s*=\s*"([^"]*)"/m.exec(manifest)?.[1];
            main = /^\s*main\s*=\s*"([^"]*)"/m.exec(manifest)?.[1] ?? main;
        } catch {
            // no manifest: the defaults below
        }

        if (lib) return path.join(directory, lib);

        for (const candidate of [`${name}.cl`, "lib.cl", main]) {
            if (fs.existsSync(path.join(directory, candidate))) return path.join(directory, candidate);
        }

        return path.join(directory, `${name}.cl`);
    }

    /** Where `import "<written>"` in `fromFile` leads, in the same order clearc looks. */
    resolveImport(fromFile: string, written: string): string | undefined {
        const withExtension = (p: string) => (path.extname(p) === "" ? `${p}.cl` : p);
        const standard = this.options.standardLibrary();
        const parts = written.split("/");

        if (parts[0] === "std" && parts.length > 1)
            return standard ? existing(path.join(standard, withExtension(parts.slice(1).join("/")))) : undefined;

        const candidates = [path.join(path.dirname(fromFile), withExtension(written))];

        const root = this.projectRoot(fromFile);
        if (root) {
            const pkg = this.packages(root).get(parts[0]);
            if (pkg) candidates.push(parts.length === 1 ? pkg.entry : path.join(pkg.directory, withExtension(parts.slice(1).join("/"))));
        }

        if (standard) candidates.push(path.join(standard, withExtension(written)));

        const self = path.resolve(fromFile);
        for (const candidate of candidates) {
            const found = existing(candidate);
            if (found && found !== self) return found;
        }

        return undefined;
    }

    /** The modules `file` imports, plus the String/List/Map prelude the compiler adds itself. */
    modules(file: string, parsed: ParsedFile): ModuleRef[] {
        const result: ModuleRef[] = [];
        const seen = new Set<string>([path.resolve(file)]);

        for (const info of parsed.imports) {
            const resolved = this.resolveImport(file, info.path);
            if (!resolved || (seen.has(resolved) && !info.alias)) continue;
            const module = this.parseFile(resolved);
            if (!module) continue;
            seen.add(resolved);
            result.push({ info, file: resolved, parsed: module });
        }

        const standard = this.options.standardLibrary();
        if (standard) {
            const defined = new Set(parsed.symbols.map((s) => s.name));
            for (const name of PRELUDE) {
                const resolved = existing(path.join(standard, `${name}.cl`));
                if (!resolved || seen.has(resolved)) continue;
                const module = this.parseFile(resolved);
                if (!module) continue;
                // only the class itself comes for free (String, List, Map), not the module's helpers
                const symbols = module.symbols.filter((s) => isTypeKind(s.kind) && !defined.has(s.name));
                result.push({ file: resolved, parsed: { ...module, symbols } });
            }
        }

        return result;
    }

    /** Top-level declarations `file` can use without a prefix. */
    visibleSymbols(file: string, parsed: ParsedFile): Located[] {
        const result: Located[] = parsed.symbols.map((symbol) => ({ symbol, file }));

        for (const module of this.modules(file, parsed)) {
            if (module.info?.alias) continue;
            for (const symbol of module.parsed.symbols) result.push({ symbol, file: module.file });
        }

        return result;
    }

    lookupTopLevel(file: string, parsed: ParsedFile, name: string): Located | undefined {
        return this.visibleSymbols(file, parsed).find((l) => l.symbol.name === name);
    }

    moduleAlias(file: string, parsed: ParsedFile, alias: string): ModuleRef | undefined {
        return this.modules(file, parsed).find((m) => m.info?.alias === alias);
    }

    // ---------------------------------------------------------------- members

    /**
     * Fields, methods and properties of a type (written like `*List[int]`), with type parameters
     * replaced by the arguments. Base classes are included, the derived class's version first.
     */
    members(file: string, parsed: ParsedFile, type: string, seen = new Set<string>()): Located[] {
        const { name, args } = baseType(type);
        if (seen.has(name)) return [];
        seen.add(name);

        const owner = this.lookupType(file, parsed, name);
        if (!owner) return [];

        const result: Located[] = [];
        const typeParams = owner.symbol.typeParams;
        const specialise = (s: ClearSymbol): ClearSymbol =>
            args.length === 0 ? s : {
                ...s,
                type: s.type && substitute(s.type, typeParams, args),
                returnType: s.returnType && substitute(s.returnType, typeParams, args),
                detail: substitute(s.detail, typeParams, args),
                params: s.params?.map((p) => ({ ...p, type: p.type && substitute(p.type, typeParams, args) })),
            };

        for (const child of owner.symbol.children) result.push({ symbol: specialise(child), file: owner.file });

        // properties and functions written outside the class, with `self: *Type`
        const ownerParsed = this.parseFile(owner.file);
        if (ownerParsed) {
            for (const symbol of ownerParsed.symbols) {
                const self = symbol.params?.[0];
                if (self?.name === "self" && self.type && baseType(self.type).name === name && (symbol.kind === "property" || symbol.kind === "function"))
                    result.push({ symbol: specialise(symbol), file: owner.file });
            }
        }

        for (const base of owner.symbol.bases ?? []) {
            for (const inherited of this.members(owner.file, ownerParsed ?? parsed, base, seen)) {
                if (!result.some((r) => r.symbol.name === inherited.symbol.name)) result.push(inherited);
            }
        }

        return result;
    }

    lookupType(file: string, parsed: ParsedFile, name: string): Located | undefined {
        const found = this.visibleSymbols(file, parsed).find((l) => l.symbol.name === name && isTypeKind(l.symbol.kind));
        if (found) return found;

        // module.Type
        const dot = name.indexOf(".");
        if (dot > 0) {
            const module = this.moduleAlias(file, parsed, name.slice(0, dot));
            const symbol = module?.parsed.symbols.find((s) => s.name === name.slice(dot + 1) && isTypeKind(s.kind));
            if (module && symbol) return { symbol, file: module.file };
        }

        return undefined;
    }

    // ---------------------------------------------------------------- types of expressions

    /** The type of a local variable, from its annotation, its initializer or the loop it belongs to. */
    typeOfLocal(file: string, parsed: ParsedFile, local: ClearSymbol, depth = 0): string | undefined {
        if (local.type) return local.type;
        if (depth > 8) return undefined;
        if (local.initializer) return this.typeOf(file, parsed, local.initializer, local.line, depth + 1);
        if (local.iterates) {
            const [iterated, index] = local.iterates.split("#");
            return this.elementType(file, parsed, iterated, local.line, index === undefined ? undefined : Number(index), depth + 1);
        }
        return undefined;
    }

    /** What `for x in <expression>` gives x. */
    elementType(file: string, parsed: ParsedFile, expression: string, line: number, tupleIndex?: number, depth = 0): string | undefined {
        if (/\.\.=?/.test(stripStrings(expression)) && !/^\s*\[/.test(expression)) return "int";

        const type = this.typeOf(file, parsed, expression, line, depth);
        if (!type) return undefined;

        const { name, args, element } = baseType(type);
        if (element) return element;
        if (name === "str" || name === "String") return "int8";
        if (name === "Map") return tupleIndex === 1 ? args[1] : args[0];
        if (name === "Generator") return args[0];

        // a class with operator iterate (a generator) or operator get
        const members = this.members(file, parsed, type);
        const iterate = members.find((m) => m.symbol.kind === "operator" && m.symbol.name === "iterate");
        if (iterate?.symbol.returnType) return baseType(iterate.symbol.returnType).args[0];
        const get = members.find((m) => m.symbol.kind === "operator" && m.symbol.name === "get");
        if (get?.symbol.returnType) return stripPointer(get.symbol.returnType);

        return undefined;
    }

    /** A best guess at the type of an expression written at `line`. */
    typeOf(file: string, parsed: ParsedFile, expression: string, line: number, depth = 0): string | undefined {
        let text = expression.trim();
        if (text === "" || depth > 12) return undefined;

        // when c use a otherwise b
        const when = /^when\s+.+?\s+use\s+(.+?)\s+otherwise\s+(.+)$/.exec(text);
        if (when) return this.typeOf(file, parsed, when[1], line, depth + 1) ?? this.typeOf(file, parsed, when[2], line, depth + 1);

        // move lambda / lambda
        if (/^(move\s+)?lambda\b/.test(text)) return "function";

        // x as T
        const cast = /\s+as\s+([A-Za-z_*?\[\]][A-Za-z0-9_*?\[\];, ]*)$/.exec(text);
        if (cast) return cast[1].trim();

        // binary operators: comparisons are bool, text + text is a String, otherwise the left side's type
        const masked = stripStrings(text);
        const flat = topLevelOnly(masked);
        if (/(==|!=|<=|>=|\bin\b|\bis\b|\band\b|\bor\b|^not\b)/.test(flat) || /(?<![-<>])[<>](?![<>=])/.test(flat)) return "bool";
        const plus = topLevelOperator(masked, ["??"]);
        if (plus >= 0) {
            const left = this.typeOf(file, parsed, text.slice(0, plus), line, depth + 1);
            return left ? stripOptional(left) : this.typeOf(file, parsed, text.slice(plus + 2), line, depth + 1);
        }
        const arithmetic = topLevelOperator(masked, ["+", "-", "*", "/", "%", "&", "|", "^"]);
        if (arithmetic > 0) {
            const left = this.typeOf(file, parsed, text.slice(0, arithmetic), line, depth + 1);
            if (masked[arithmetic] === "+" && (left === "str" || left === "String")) return "String";
            return left;
        }

        // literals
        if (/^".*"$/.test(text)) return "str";
        if (/^'.*'$/.test(text)) return "int8";
        if (/^(true|false)$/.test(text)) return "bool";
        if (/^-?\d+(\.\d+|[eE][+-]?\d+)/.test(text)) return "float64";
        if (/^-?(0[xXbB])?[0-9A-Fa-f_]+$/.test(text)) return "int";
        if (text === "none") return undefined;
        if (text.startsWith("(")) {
            const close = matchClose(text, 0);
            if (close === text.length - 1) {
                const items = splitTopLevel(text.slice(1, -1));
                if (items.length === 1) return this.typeOf(file, parsed, items[0], line, depth + 1);
                return `(${items.map((i) => this.typeOf(file, parsed, i, line, depth + 1) ?? "?").join(", ")})`;
            }
        }

        // &x and *x
        if (text.startsWith("&")) {
            const inner = this.typeOf(file, parsed, text.slice(1), line, depth + 1);
            return inner ? `*${inner}` : undefined;
        }
        if (text.startsWith("*")) {
            const inner = this.typeOf(file, parsed, text.slice(1), line, depth + 1);
            return inner ? stripPointer(inner) : undefined;
        }
        if (text.startsWith("await ")) {
            const task = this.typeOf(file, parsed, text.slice(6), line, depth + 1);
            return task ? baseType(task).args[0] : undefined;
        }

        // a chain: head(.member | ?.member | (args) | [index])*
        const chain = parseChain(text);
        if (!chain) return undefined;

        let current: string | undefined;
        let module: ModuleRef | undefined;
        let pendingCallable: ClearSymbol | undefined;
        let pendingTypeArgs: string[] = [];
        let typeName: Located | undefined;

        const head = chain.head;
        const local = localsAt(parsed, line).find((l) => l.name === head);

        if (head === "self") {
            const owner = local?.type ?? (enclosingType(parsed, line) ? `*${enclosingType(parsed, line)!.name}` : undefined);
            current = owner;
        } else if (local) {
            current = this.typeOfLocal(file, parsed, local, depth + 1);
        } else if (isPrimitive(head)) {
            typeName = undefined;
            current = undefined;
            pendingCallable = { name: head, kind: "function", returnType: head } as ClearSymbol;
        } else {
            const alias = this.moduleAlias(file, parsed, head);
            if (alias) module = alias;
            else {
                const top = this.lookupTopLevel(file, parsed, head);
                if (top && isTypeKind(top.symbol.kind)) typeName = top;
                else if (top && isCallable(top.symbol.kind)) pendingCallable = top.symbol;
                else if (top) current = top.symbol.type ?? (top.symbol.initializer ? this.typeOf(top.file, this.parseFile(top.file) ?? parsed, top.symbol.initializer, top.symbol.line, depth + 1) : undefined);
                else return undefined;
            }
        }

        for (const step of chain.steps) {
            if (step.kind === "member") {
                if (module) {
                    const symbol = module.parsed.symbols.find((s) => s.name === step.text);
                    module = undefined;
                    if (!symbol) return undefined;
                    if (isTypeKind(symbol.kind)) typeName = { symbol, file: "" };
                    else if (isCallable(symbol.kind)) pendingCallable = symbol;
                    else current = symbol.type;
                    continue;
                }

                if (typeName) {
                    // Shape.Circle(...) is a Shape
                    const caseSymbol = typeName.symbol.children.find((c) => c.name === step.text);
                    if (caseSymbol?.kind === "enumCase") {
                        current = typeName.symbol.name;
                        typeName = undefined;
                        continue;
                    }
                    return undefined;
                }

                if (current === undefined) return undefined;

                if (step.optional || current.startsWith("?")) {
                    if (step.text === "value" || step.text === "value_or") {
                        current = stripOptional(current);
                        if (step.text === "value_or") pendingCallable = { name: "value_or", kind: "method", returnType: current } as ClearSymbol;
                        continue;
                    }
                }

                const { name, args } = baseType(current);
                const synthetic = syntheticMembers(name).find((m) => m.name === step.text);
                if (synthetic) {
                    const result = synthetic.returnType === "T" ? args[0] : synthetic.returnType;
                    if (synthetic.detail.startsWith("function")) pendingCallable = { name: step.text, kind: "method", returnType: result } as ClearSymbol;
                    else current = result;
                    continue;
                }

                const member = this.members(file, parsed, stripOptional(current)).find((m) => m.symbol.name === step.text);
                if (!member) return undefined;
                const optional = step.optional;

                if (member.symbol.kind === "method" || member.symbol.kind === "function") {
                    pendingCallable = member.symbol;
                    current = undefined;
                } else {
                    const memberType = member.symbol.kind === "property" ? member.symbol.returnType : member.symbol.type;
                    current = memberType && optional && !memberType.startsWith("?") ? `?${memberType}` : memberType;
                }
                continue;
            }

            if (step.kind === "index") {
                if (typeName) {
                    pendingTypeArgs = splitTopLevel(step.text);
                    continue;
                }
                if (pendingCallable) {
                    // generic call: largest[float64](...)
                    pendingTypeArgs = splitTopLevel(step.text);
                    continue;
                }
                if (current === undefined) return undefined;
                current = this.indexType(file, parsed, current, step.text);
                continue;
            }

            // a call
            if (typeName) {
                const args = pendingTypeArgs.length > 0 ? `[${pendingTypeArgs.join(", ")}]` : "";
                current = `${typeName.symbol.name}${args}`;
                typeName = undefined;
                pendingTypeArgs = [];
                continue;
            }

            if (pendingCallable) {
                let result = pendingCallable.returnType;
                if (result && pendingCallable.typeParams && pendingTypeArgs.length > 0)
                    result = substitute(result, pendingCallable.typeParams, pendingTypeArgs);
                else if (result && pendingCallable.typeParams && pendingCallable.params) {
                    // infer T from the first argument whose parameter is plainly T
                    const argTexts = splitTopLevel(step.text);
                    const inferred = pendingCallable.typeParams.map((tp) => {
                        const index = pendingCallable!.params!.findIndex((p) => p.type?.trim() === tp);
                        return index >= 0 && argTexts[index] ? this.typeOf(file, parsed, argTexts[index], line, depth + 1) ?? tp : tp;
                    });
                    result = substitute(result, pendingCallable.typeParams, inferred);
                }
                if (pendingCallable.isAsync && result) result = `Task[${result}]`;
                current = result;
                pendingCallable = undefined;
                pendingTypeArgs = [];
                continue;
            }

            if (current?.startsWith("function")) {
                const arrow = current.lastIndexOf("->");
                current = arrow >= 0 ? current.slice(arrow + 2).trim() : undefined;
                continue;
            }

            if (current) {
                const call = this.members(file, parsed, current).find((m) => m.symbol.kind === "operator" && m.symbol.name === "call");
                current = call?.symbol.returnType;
                continue;
            }

            return undefined;
        }

        if (typeName) return typeName.symbol.name;
        return current;
    }

    private indexType(file: string, parsed: ParsedFile, type: string, index: string): string | undefined {
        const { name, args, element } = baseType(type);
        const isSlice = findSliceColon(index);

        if (element) return isSlice ? `[]${element}` : element;
        if (name === "str" || name === "String") return isSlice ? "str" : "int8";
        if (name === "Map") return args[1];
        if (type.trim().startsWith("(")) {
            const items = splitTopLevel(type.trim().slice(1, -1));
            const position = Number(index.trim());
            return Number.isInteger(position) ? items[position] : undefined;
        }

        const members = this.members(file, parsed, type);
        const operator = members.find((m) => m.symbol.kind === "operator" && m.symbol.name === (isSlice ? "slice" : "get"));
        return operator?.symbol.returnType ? stripPointer(operator.symbol.returnType) : undefined;
    }
}

function existing(candidate: string): string | undefined {
    try {
        return fs.statSync(candidate).isFile() ? path.resolve(candidate) : undefined;
    } catch {
        return undefined;
    }
}

export function stripPointer(type: string): string {
    const t = type.trim();
    return t.startsWith("*") ? t.slice(1).trim() : t;
}

export function stripOptional(type: string): string {
    const t = type.trim();
    return t.startsWith("?") ? t.slice(1).trim() : t;
}

function stripStrings(text: string): string {
    return text.replace(/"(?:\\.|[^"\\])*"/g, (s) => `"${" ".repeat(Math.max(0, s.length - 2))}"`).replace(/'(?:\\.|[^'\\])*'/g, (s) => `'${" ".repeat(Math.max(0, s.length - 2))}'`);
}

function topLevelOnly(text: string): string {
    let depth = 0;
    let out = "";
    for (const c of text) {
        if (c === "(" || c === "[" || c === "{") depth++;
        if (depth === 0) out += c;
        else out += " ";
        if (c === ")" || c === "]" || c === "}") depth--;
    }
    return out;
}

/** The last top-level binary operator from the list (so `a - b - c` splits at the last `-`). */
function topLevelOperator(text: string, operators: string[]): number {
    const flat = topLevelOnly(text);
    for (let i = flat.length - 1; i > 0; i--) {
        for (const op of operators) {
            if (!flat.startsWith(op, i)) continue;
            const before = flat.slice(0, i).trimEnd();
            const prev = before[before.length - 1];
            // unary (-x, *p), ->, ++, ** and compound forms are not binary operators here
            if (!prev || "=+-*/%<>&|^!(,?:".includes(prev)) continue;
            if (op === "-" && flat[i + 1] === ">") continue;
            if (op.length === 1 && (flat[i + 1] === op || flat[i - 1] === op || flat[i + 1] === "=")) continue;
            if (op === "?" || op === "??") return i;
            return i;
        }
    }
    return -1;
}

function matchClose(text: string, open: number): number {
    let depth = 0;
    for (let i = open; i < text.length; i++) {
        if ("([{".includes(text[i])) depth++;
        else if (")]}".includes(text[i])) {
            depth--;
            if (depth === 0) return i;
        }
    }
    return -1;
}

function findSliceColon(index: string): boolean {
    let depth = 0;
    for (const c of index) {
        if ("([{".includes(c)) depth++;
        else if (")]}".includes(c)) depth--;
        else if (c === ":" && depth === 0) return true;
    }
    return false;
}

export interface Chain {
    head: string;
    steps: { kind: "member" | "call" | "index"; text: string; optional?: boolean }[];
}

/** `a.b(1)[2]?.c` -> head a, steps .b (1) [2] ?.c ; undefined for anything that isn't a plain chain. */
export function parseChain(text: string): Chain | undefined {
    const headMatch = /^[A-Za-z_][A-Za-z0-9_]*/.exec(text);
    if (!headMatch) return undefined;

    const steps: Chain["steps"] = [];
    let i = headMatch[0].length;

    while (i < text.length) {
        const c = text[i];

        if (c === " " || c === "\t") {
            i++;
            continue;
        }

        if (c === "." || (c === "?" && text[i + 1] === ".")) {
            const optional = c === "?";
            i += optional ? 2 : 1;
            while (text[i] === " ") i++;
            const name = /^[A-Za-z_][A-Za-z0-9_]*/.exec(text.slice(i));
            if (!name) return { head: headMatch[0], steps };
            steps.push({ kind: "member", text: name[0], optional });
            i += name[0].length;
            continue;
        }

        if (c === "!" && text[i + 1] === "(") {
            // a macro use: its type is unknown
            return undefined;
        }

        if (c === "(" || c === "[" || c === "{") {
            const close = matchClose(text, i);
            if (close < 0) return undefined;
            steps.push({ kind: c === "[" ? "index" : "call", text: text.slice(i + 1, close) });
            i = close + 1;
            continue;
        }

        return undefined;
    }

    return { head: headMatch[0], steps };
}

/**
 * The expression right before `column` on a line, e.g. for `print(list[0].name.` it is `list[0].name`.
 * Used to complete members after a dot.
 */
export function receiverBefore(line: string, column: number): string | undefined {
    let i = column - 1;
    while (i >= 0 && line[i] === " ") i--;

    const end = i + 1;
    let depth = 0;

    while (i >= 0) {
        const c = line[i];
        if (")]}".includes(c)) {
            depth++;
            i--;
            continue;
        }
        if ("([{".includes(c)) {
            if (depth === 0) break;
            depth--;
            i--;
            continue;
        }
        if (depth > 0) {
            i--;
            continue;
        }
        if (/[A-Za-z0-9_.]/.test(c) || (c === "?" && line[i + 1] === ".")) {
            i--;
            continue;
        }
        if (c === "&" || c === "*") {
            // &x.y binds tighter on the right; stop here
            break;
        }
        break;
    }

    const text = line.slice(i + 1, end).trim();
    return text === "" ? undefined : text;
}
