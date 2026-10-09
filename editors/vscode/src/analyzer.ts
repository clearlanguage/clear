// A small, forgiving reader for Clear source. It does not replace the compiler: it finds
// declarations, imports, locals and their types well enough for completion, hover,
// go to definition and the outline, and it keeps working on half-written code.

export type SymbolKind =
    | "function" | "method" | "operator" | "property" | "declare" | "macro"
    | "class" | "trait" | "enum" | "variant" | "union"
    | "field" | "enumCase" | "const" | "variable" | "parameter" | "typeParameter";

export interface Parameter {
    name: string;
    type?: string;
    defaultValue?: string;
}

export interface Range {
    line: number;
    character: number;
    endLine: number;
    endCharacter: number;
}

export interface ClearSymbol {
    name: string;
    kind: SymbolKind;
    /** where the name is written */
    line: number;
    character: number;
    /** the last line that belongs to the declaration (its block, for classes and functions) */
    endLine: number;
    /** the declaration as written, e.g. `function push(self: *List[T], value: T)` */
    detail: string;
    doc?: string;
    type?: string;
    params?: Parameter[];
    returnType?: string;
    typeParams?: string[];
    bases?: string[];
    isAsync?: boolean;
    children: ClearSymbol[];
    /** the class, enum... a member belongs to */
    container?: string;
    /** for locals: the lines where the name can be used */
    scopeStart?: number;
    scopeEnd?: number;
    /** for locals: the expression it was initialised from, used to work out its type */
    initializer?: string;
    /** for `for x in <expr>` locals */
    iterates?: string;
}

export interface ImportInfo {
    path: string;
    alias?: string;
    line: number;
    /** column range of the quoted path, without the quotes */
    start: number;
    end: number;
}

export interface ParsedFile {
    symbols: ClearSymbol[];
    imports: ImportInfo[];
    locals: ClearSymbol[];
    lines: string[];
}

const IDENT = "[A-Za-z_][A-Za-z0-9_]*";

export const KEYWORDS = new Set([
    "if", "else", "elseif", "while", "for", "in", "return", "break", "continue", "switch", "case", "default",
    "when", "use", "otherwise", "pass", "defer", "assert", "yield", "await", "import", "as", "is", "and", "or",
    "not", "true", "false", "null", "none", "let", "const", "function", "operator", "property", "class", "trait",
    "enum", "variant", "union", "macro", "declare", "lambda", "async", "move", "sizeof", "self", "super",
]);

interface LogicalLine {
    /** the first physical line */
    line: number;
    /** the last physical line (a statement continues while brackets are open) */
    endLine: number;
    indent: number;
    /** code without comments; strings keep their length but lose their contents */
    code: string;
    /** code without comments, strings intact (for initializers) */
    text: string;
    /** the trailing `// comment` on the first line, if any */
    trailingComment?: string;
}

function indentOf(line: string): number {
    let width = 0;
    for (const c of line) {
        if (c === " ") width++;
        else if (c === "\t") width += 4;
        else break;
    }
    return width;
}

/**
 * Removes comments from every line. `masked` also blanks out string and char contents so brackets
 * and keywords inside them are not seen; both keep every column where it was.
 */
export function stripComments(lines: string[]): { plain: string[]; masked: string[]; comments: (string | undefined)[] } {
    const plain: string[] = [];
    const masked: string[] = [];
    const comments: (string | undefined)[] = [];
    let inBlock = false;

    for (const line of lines) {
        let p = "";
        let m = "";
        let comment: string | undefined;
        let i = 0;

        while (i < line.length) {
            const c = line[i];

            if (inBlock) {
                if (c === "*" && line[i + 1] === "/") {
                    inBlock = false;
                    p += "  ";
                    m += "  ";
                    i += 2;
                } else {
                    p += " ";
                    m += " ";
                    i++;
                }
                continue;
            }

            if (c === "/" && line[i + 1] === "/") {
                comment = line.slice(i + 2).trim();
                break;
            }

            if (c === "/" && line[i + 1] === "*") {
                inBlock = true;
                p += "  ";
                m += "  ";
                i += 2;
                continue;
            }

            if (c === "\"" || c === "'") {
                const quote = c;
                let j = i + 1;
                while (j < line.length && line[j] !== quote) {
                    if (line[j] === "\\") j++;
                    j++;
                }
                const end = Math.min(j + 1, line.length);
                p += line.slice(i, end);
                m += quote + " ".repeat(Math.max(0, end - i - 2)) + (end - i >= 2 ? quote : "");
                i = end;
                continue;
            }

            p += c;
            m += c;
            i++;
        }

        plain.push(p.replace(/\s+$/, ""));
        masked.push(m.replace(/\s+$/, ""));
        comments.push(comment);
    }

    return { plain, masked, comments };
}

function bracketBalance(code: string): number {
    let depth = 0;
    for (const c of code) {
        if (c === "(" || c === "[" || c === "{") depth++;
        else if (c === ")" || c === "]" || c === "}") depth--;
    }
    return depth;
}

function logicalLines(lines: string[]): { logical: LogicalLine[]; comments: (string | undefined)[] } {
    const { plain, masked, comments } = stripComments(lines);
    const logical: LogicalLine[] = [];

    for (let i = 0; i < lines.length; i++) {
        if (masked[i].trim() === "") continue;

        let code = masked[i];
        let text = plain[i];
        let end = i;
        let depth = bracketBalance(code);

        // a continuation is indented deeper (or closes the bracket); anything else means the
        // bracket was left open while typing, and the next statement must not be swallowed
        const continues = (next: number) =>
            masked[next].trim() === "" || indentOf(lines[next]) > indentOf(lines[i]) || /^[)\]}]/.test(masked[next].trim());

        while (depth > 0 && end + 1 < lines.length && continues(end + 1)) {
            end++;
            code += " " + masked[end].trim();
            text += " " + plain[end].trim();
            depth += bracketBalance(masked[end]);
        }

        logical.push({ line: i, endLine: end, indent: indentOf(lines[i]), code, text, trailingComment: comments[i] });
        i = end;
    }

    return { logical, comments };
}

/** Splits on commas that are not inside brackets. */
export function splitTopLevel(text: string, separator = ","): string[] {
    const parts: string[] = [];
    let depth = 0;
    let current = "";

    for (let i = 0; i < text.length; i++) {
        const c = text[i];
        if (c === "(" || c === "[" || c === "{") depth++;
        else if (c === ")" || c === "]" || c === "}") depth--;

        if (c === separator && depth === 0) {
            parts.push(current);
            current = "";
        } else {
            current += c;
        }
    }

    if (current.trim() !== "" || parts.length > 0) parts.push(current);
    return parts.map((p) => p.trim()).filter((p) => p !== "");
}

/** The text between the bracket at `open` and its partner; -1 if it never closes. */
function matchBracket(text: string, open: number): number {
    const pairs: Record<string, string> = { "(": ")", "[": "]", "{": "}" };
    const close = pairs[text[open]];
    let depth = 0;

    for (let i = open; i < text.length; i++) {
        if (text[i] === text[open]) depth++;
        else if (text[i] === close) {
            depth--;
            if (depth === 0) return i;
        }
    }

    return -1;
}

function parseParameters(text: string): Parameter[] {
    return splitTopLevel(text).map((part) => {
        let rest = part;
        let defaultValue: string | undefined;

        const equals = findTopLevel(rest, "=");
        if (equals >= 0) {
            defaultValue = rest.slice(equals + 1).trim();
            rest = rest.slice(0, equals).trim();
        }

        const colon = rest.indexOf(":");
        if (colon >= 0)
            return { name: rest.slice(0, colon).trim(), type: rest.slice(colon + 1).trim(), defaultValue };

        return { name: rest.trim(), defaultValue };
    });
}

/** Index of `needle` outside brackets (and not part of ==, <=, >=, !=, :=, ->). */
function findTopLevel(text: string, needle: string): number {
    let depth = 0;
    for (let i = 0; i < text.length; i++) {
        const c = text[i];
        if (c === "(" || c === "[" || c === "{") depth++;
        else if (c === ")" || c === "]" || c === "}") depth--;
        else if (depth === 0 && text.startsWith(needle, i)) {
            if (needle === "=" && ("=<>!:+-*/%&|^".includes(text[i - 1] ?? "") || text[i + 1] === "=")) continue;
            return i;
        }
    }
    return -1;
}

interface Signature {
    typeParams?: string[];
    params: Parameter[];
    returnType?: string;
    /** the signature as written, up to the closing `:` */
    written: string;
}

/** Reads `[T, U](a: int, b: T) -> R` starting at `from` (just after the name). */
function parseSignature(code: string, text: string, from: number): Signature {
    let i = from;
    let typeParams: string[] | undefined;

    while (i < code.length && code[i] === " ") i++;

    if (code[i] === "[") {
        const close = matchBracket(code, i);
        if (close < 0) return { params: [], written: text.slice(0, code.length).trim() };
        typeParams = splitTopLevel(code.slice(i + 1, close)).map((t) => t.split(":")[0].trim());
        i = close + 1;
    }

    while (i < code.length && code[i] === " ") i++;

    let params: Parameter[] = [];
    if (code[i] === "(") {
        const close = matchBracket(code, i);
        const end = close < 0 ? code.length : close;
        params = parseParameters(text.slice(i + 1, end));
        i = close < 0 ? code.length : close + 1;
    }

    let returnType: string | undefined;
    const rest = code.slice(i);
    const arrow = rest.indexOf("->");
    if (arrow >= 0) {
        returnType = rest.slice(arrow + 2).replace(/:\s*$/, "").trim();
        if (returnType === "") returnType = undefined;
    }

    return { typeParams, params, returnType, written: text.replace(/:\s*$/, "").trim() };
}

function docAbove(lines: string[], comments: (string | undefined)[], line: number): string | undefined {
    const doc: string[] = [];

    for (let i = line - 1; i >= 0; i--) {
        const trimmed = lines[i].trim();
        if (!trimmed.startsWith("//")) break;
        doc.unshift(comments[i] ?? "");
    }

    return doc.length > 0 ? doc.join("\n").trim() : undefined;
}

/** Blank lines and comments after a block do not belong to it. */
function blockEnd(logical: LogicalLine[], index: number): number {
    const indent = logical[index].indent;
    let end = logical[index].endLine;

    for (let j = index + 1; j < logical.length; j++) {
        if (logical[j].indent <= indent) break;
        end = logical[j].endLine;
    }

    return end;
}

const DECLARATION = new RegExp(
    `^(async\\s+)?(function|operator|property|declare|macro|class|trait|enum|variant|union)\\s+(${IDENT})`,
);

export function parse(source: string): ParsedFile {
    const lines = source.split(/\r?\n/);
    const { logical, comments } = logicalLines(lines);
    const symbols: ClearSymbol[] = [];
    const imports: ImportInfo[] = [];
    const locals: ClearSymbol[] = [];

    // containers: the declarations whose blocks we are inside, innermost last
    const stack: { symbol: ClearSymbol; indent: number; end: number }[] = [];

    for (let index = 0; index < logical.length; index++) {
        const entry = logical[index];
        const code = entry.code.trim();
        const text = entry.text.trim();
        const offset = entry.code.length - entry.code.trimStart().length;

        while (stack.length > 0 && (entry.indent <= stack[stack.length - 1].indent || entry.line > stack[stack.length - 1].end))
            stack.pop();

        const parent = stack.length > 0 ? stack[stack.length - 1].symbol : undefined;
        const inFunction = stack.some((s) => isCallable(s.symbol.kind));
        const doc = docAbove(lines, comments, entry.line) ?? entry.trailingComment;

        // import "path" [as alias]
        const importMatch = /^import\s+"([^"]*)"(?:\s+as\s+([A-Za-z_][A-Za-z0-9_]*))?/.exec(text);
        if (importMatch) {
            const start = lines[entry.line].indexOf("\"") + 1;
            imports.push({ path: importMatch[1], alias: importMatch[2], line: entry.line, start, end: start + importMatch[1].length });
            continue;
        }

        const declaration = DECLARATION.exec(code);
        if (declaration && !(inFunction && declaration[2] === "declare")) {
            const keyword = declaration[2];
            const name = declaration[3];
            const nameColumn = offset + declaration[0].length - name.length;
            const end = blockEnd(logical, index);
            const signature = parseSignature(code, text, declaration[0].length);

            let kind: SymbolKind = keyword as SymbolKind;
            const isMember = parent !== undefined && isTypeKind(parent.kind);
            if (keyword === "function" && isMember) kind = "method";

            const symbol: ClearSymbol = {
                name,
                kind,
                line: entry.line,
                character: nameColumn,
                endLine: keyword === "declare" ? entry.endLine : end,
                detail: signature.written,
                doc,
                params: isCallable(kind) ? signature.params : undefined,
                returnType: signature.returnType,
                typeParams: signature.typeParams,
                isAsync: declaration[1] !== undefined,
                container: isMember ? parent!.name : undefined,
                children: [],
            };

            if (isTypeKind(kind)) {
                symbol.params = undefined;
                if (kind === "class" || kind === "enum" || kind === "trait") {
                    // class Dog(Animal, Shape)
                    const afterName = code.slice(declaration[0].length);
                    const open = afterName.indexOf("(");
                    if (open >= 0 && (afterName.indexOf("[") < 0 || afterName.indexOf("[") > open || matchBracket(afterName, afterName.indexOf("[")) < open)) {
                        const close = matchBracket(afterName, open);
                        symbol.bases = splitTopLevel(afterName.slice(open + 1, close < 0 ? undefined : close));
                    }
                }
            }

            if (parent) parent.children.push(symbol);
            else symbols.push(symbol);

            if (isCallable(kind) && kind !== "declare") {
                // parameters are locals of the function body
                for (const param of signature.params) {
                    if (param.name === "" || param.name === "...") continue;
                    const column = lines[entry.line].indexOf(param.name, nameColumn + name.length);
                    locals.push({
                        name: param.name,
                        kind: "parameter",
                        line: entry.line,
                        character: column >= 0 ? column : nameColumn,
                        endLine: entry.line,
                        detail: param.type ? `${param.name}: ${param.type}` : param.name,
                        type: param.name === "self" && !param.type && symbol.container ? `*${symbol.container}` : param.type,
                        container: symbol.container,
                        scopeStart: entry.line,
                        scopeEnd: end,
                        children: [],
                    });
                }

                for (const typeParam of signature.typeParams ?? []) {
                    locals.push({
                        name: typeParam, kind: "typeParameter", line: entry.line, character: nameColumn, endLine: entry.line,
                        detail: typeParam, scopeStart: entry.line, scopeEnd: end, children: [],
                    });
                }
            }

            if (keyword !== "declare" && end > entry.endLine)
                stack.push({ symbol, indent: entry.indent, end });

            continue;
        }

        // members of classes, enums, variants, unions and traits
        if (parent && isTypeKind(parent.kind) && !inFunction) {
            const member = memberDeclaration(parent, entry, code, text, offset, doc);
            if (member) {
                parent.children.push(member);
                continue;
            }
        }

        if (inFunction) {
            const functionEntry = [...stack].reverse().find((s) => isCallable(s.symbol.kind))!;
            readLocals(entry, code, text, offset, blockEndFrom(logical, index, functionEntry.end), locals, lines);
            continue;
        }

        // top level (or inside a class body): let / const
        const variable = new RegExp(`^(let|const)\\s+(${IDENT})`).exec(code);
        if (variable) {
            const declared = readVariable(code, text, variable[0].length - variable[2].length);
            symbols.push({
                name: variable[2],
                kind: variable[1] === "const" ? "const" : "variable",
                line: entry.line,
                character: offset + variable[0].length - variable[2].length,
                endLine: entry.endLine,
                detail: text,
                doc,
                type: declared.type,
                initializer: declared.initializer,
                children: [],
            });
        }
    }

    return { symbols, imports, locals, lines };
}

/** For a local declared at `index`: it can be used until its enclosing block ends. */
function blockEndFrom(logical: LogicalLine[], index: number, functionEnd: number): number {
    const indent = logical[index].indent;
    let end = logical[index].endLine;

    for (let j = index + 1; j < logical.length; j++) {
        if (logical[j].indent < indent || logical[j].line > functionEnd) break;
        end = logical[j].endLine;
    }

    return end;
}

function readVariable(code: string, text: string, nameStart: number): { type?: string; initializer?: string } {
    const rest = text.slice(nameStart);
    const equals = findTopLevel(rest, "=");
    const colon = rest.indexOf(":");
    let type: string | undefined;

    if (colon >= 0 && (equals < 0 || colon < equals)) type = rest.slice(colon + 1, equals < 0 ? undefined : equals).trim();

    return { type, initializer: equals >= 0 ? rest.slice(equals + 1).trim() : undefined };
}

function memberDeclaration(parent: ClearSymbol, entry: LogicalLine, code: string, text: string, offset: number, doc?: string): ClearSymbol | undefined {
    const base = { line: entry.line, endLine: entry.endLine, doc, container: parent.name, children: [] as ClearSymbol[] };

    if (parent.kind === "enum") {
        // Red | Blue = 10 | Circle(radius: float64)
        const match = new RegExp(`^(${IDENT})\\s*(\\(.*\\))?\\s*(=\\s*.+)?$`).exec(code);
        if (match && !KEYWORDS.has(match[1])) {
            const params = match[2] ? parseParameters(text.slice(text.indexOf("(") + 1, text.lastIndexOf(")"))) : undefined;
            return { ...base, name: match[1], kind: "enumCase", character: offset, detail: `${parent.name}.${text}`, params };
        }
    }

    if (parent.kind === "variant") {
        // each line is a type
        if (/^[A-Za-z_*?\[(]/.test(code) && !code.includes(":"))
            return { ...base, name: text, kind: "enumCase", character: offset, detail: text, type: text };
    }

    // field: name: type [= default]
    const field = new RegExp(`^(${IDENT})\\s*:(?!=)\\s*(.+)$`).exec(code);
    if (field && !KEYWORDS.has(field[1])) {
        const declared = readVariable(code, text, 0);
        return {
            ...base,
            name: field[1],
            kind: "field",
            character: offset,
            detail: text,
            type: declared.type,
            initializer: declared.initializer,
        };
    }

    return undefined;
}

function readLocals(entry: LogicalLine, code: string, text: string, offset: number, scopeEnd: number, locals: ClearSymbol[], lines: string[]) {
    const add = (name: string, extra: Partial<ClearSymbol>, scopeStart = entry.line) => {
        if (name === "_" || KEYWORDS.has(name)) return;
        const column = lines[entry.line].search(new RegExp(`\\b${name}\\b`));
        locals.push({
            name,
            kind: "variable",
            line: entry.line,
            character: column >= 0 ? column : offset,
            endLine: entry.endLine,
            detail: name,
            scopeStart,
            scopeEnd,
            children: [],
            ...extra,
        });
    };

    // let a = ..., let a: T, let a, b = ..., const A = ...
    const declaration = /^(let|const)\s+(.*)$/.exec(code);
    if (declaration) {
        const afterKeyword = text.slice(declaration[1].length).trim();
        const equals = findTopLevel(afterKeyword, "=");
        const left = equals >= 0 ? afterKeyword.slice(0, equals) : afterKeyword;
        const names = splitTopLevel(left);

        if (names.length === 1) {
            const declared = readVariable(afterKeyword, afterKeyword, 0);
            const name = names[0].split(":")[0].trim();
            add(name, {
                kind: declaration[1] === "const" ? "const" : "variable",
                detail: `${declaration[1]} ${afterKeyword}`,
                type: declared.type,
                initializer: declared.initializer,
            });
        } else {
            names.forEach((n, i) => add(n.split(":")[0].trim(), { detail: `${declaration[1]} ${n}`, initializer: equals >= 0 ? `${afterKeyword.slice(equals + 1).trim()}[${i}]` : undefined }));
        }
        return;
    }

    // for x in expr / for k, v in expr: the names belong to the loop body
    const loop = /^for\s+(.+?)\s+in\s+(.+?):?\s*$/.exec(code);
    if (loop) {
        const iterates = text.slice(text.indexOf(" in ") + 4).replace(/:\s*$/, "").trim();
        const names = splitTopLevel(loop[1]);
        names.forEach((n, i) => add(n, { detail: `for ${loop[1]} in ${iterates}`, iterates: names.length === 1 ? iterates : `${iterates}#${i}` }));
        return;
    }

    // if x := expr / while x := expr / else if x := expr
    const walrus = new RegExp(`^(?:if|while|elseif|else\\s+if)\\s+(${IDENT})\\s*:=\\s*(.+?):?\\s*$`).exec(code);
    if (walrus) {
        const expression = text.slice(text.indexOf(":=") + 2).replace(/:\s*$/, "").trim();
        add(walrus[1], { detail: `${walrus[1]} := ${expression}`, initializer: `${expression}.value` });
        return;
    }

    // case Circle(r, h): / case some(x): / case int(i):
    const pattern = new RegExp(`^case\\s+(${IDENT}(?:\\.${IDENT})*)\\s*\\((.*)\\)\\s*:?\\s*$`).exec(code);
    if (pattern) {
        for (const name of splitTopLevel(pattern[2])) {
            if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(name))
                add(name, { detail: `case ${pattern[1]}(${pattern[2]})`, type: isPrimitive(pattern[1]) ? pattern[1] : undefined });
        }
        return;
    }

    // a, b = ...  (assignment to new names is not a declaration in Clear, nothing to do)
}

export function isCallable(kind: SymbolKind): boolean {
    return kind === "function" || kind === "method" || kind === "operator" || kind === "property" || kind === "declare" || kind === "macro";
}

export function isTypeKind(kind: SymbolKind): boolean {
    return kind === "class" || kind === "trait" || kind === "enum" || kind === "variant" || kind === "union";
}

export const PRIMITIVE_TYPES = [
    "bool", "int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64",
    "float", "float32", "float64", "str",
];

export function isPrimitive(name: string): boolean {
    return PRIMITIVE_TYPES.includes(name);
}

/** `*List[int]` -> { name: "List", args: ["int"] }; `?Foo` -> Foo; `[4; int]` / `[]int` -> array of int */
export function baseType(type: string): { name: string; args: string[]; element?: string } {
    let t = type.trim();

    while (t.startsWith("*") || t.startsWith("?") || t.startsWith("&")) t = t.slice(1).trim();

    if (t.startsWith("[")) {
        const close = matchBracket(t, 0);
        const inside = t.slice(1, close < 0 ? undefined : close);
        const semicolon = inside.indexOf(";");
        const element = semicolon >= 0 ? inside.slice(semicolon + 1).trim() : t.slice((close < 0 ? t.length : close) + 1).trim();
        return { name: "[]", args: [], element };
    }

    const open = t.indexOf("[");
    if (open > 0) {
        const close = matchBracket(t, open);
        return { name: t.slice(0, open).trim(), args: splitTopLevel(t.slice(open + 1, close < 0 ? undefined : close)) };
    }

    return { name: t, args: [] };
}

/** Replaces type parameters (T -> int) in a type written in a generic declaration. */
export function substitute(type: string, typeParams: string[] | undefined, args: string[]): string {
    if (!typeParams || typeParams.length === 0 || args.length === 0) return type;
    return type.replace(new RegExp(`\\b(${typeParams.join("|")})\\b`, "g"), (name) => args[typeParams.indexOf(name)] ?? name);
}

/** The symbol (top-level or member) whose name is at this position, by declaration. */
export function findDeclarationAt(file: ParsedFile, line: number, character: number): ClearSymbol | undefined {
    const visit = (symbols: ClearSymbol[]): ClearSymbol | undefined => {
        for (const symbol of symbols) {
            if (symbol.line === line && character >= symbol.character && character <= symbol.character + symbol.name.length)
                return symbol;
            const inner = visit(symbol.children);
            if (inner) return inner;
        }
        return undefined;
    };

    return visit(file.symbols) ?? file.locals.find((l) => l.line === line && character >= l.character && character <= l.character + l.name.length);
}

/** Locals (variables, parameters) usable at this line, innermost declaration first. */
export function localsAt(file: ParsedFile, line: number): ClearSymbol[] {
    return file.locals
        .filter((l) => l.scopeStart !== undefined && l.scopeEnd !== undefined && line >= l.scopeStart && line <= l.scopeEnd && (l.kind === "parameter" || l.kind === "typeParameter" || line >= l.line))
        .sort((a, b) => b.line - a.line);
}

/** The class, enum... whose body contains this line. */
export function enclosingType(file: ParsedFile, line: number): ClearSymbol | undefined {
    return file.symbols.find((s) => isTypeKind(s.kind) && line > s.line && line <= s.endLine);
}

/** The function or method whose body contains this line. */
export function enclosingFunction(file: ParsedFile, line: number): ClearSymbol | undefined {
    for (const symbol of file.symbols) {
        if (isCallable(symbol.kind) && line >= symbol.line && line <= symbol.endLine) return symbol;
        if (isTypeKind(symbol.kind)) {
            const member = symbol.children.find((c) => isCallable(c.kind) && line >= c.line && line <= c.endLine);
            if (member) return member;
        }
    }
    return undefined;
}
