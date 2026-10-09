// Editor features over the Project model, in plain data (no VS Code types), so they can be tested
// with node alone. extension.ts turns these results into VS Code objects.

import * as fs from "fs";
import * as path from "path";
import {
    ClearSymbol, ParsedFile, SymbolKind, KEYWORDS, PRIMITIVE_TYPES, findDeclarationAt, localsAt, enclosingType,
    enclosingFunction, isCallable, isTypeKind, splitTopLevel, stripComments, baseType,
} from "./analyzer";
import { Project, Located, receiverBefore, syntheticMembers, stripOptional } from "./project";
import { BUILTIN_FUNCTIONS, KEYWORD_DOCS, PRIMITIVE_DOCS } from "./builtins";

export type EntryKind = SymbolKind | "module" | "keyword" | "builtin" | "primitive" | "file" | "folder" | "snippet";

export interface Target {
    name: string;
    kind: EntryKind;
    /** where it is declared, when it comes from a .cl file */
    file?: string;
    line?: number;
    character?: number;
    symbol?: ClearSymbol;
    /** what the hover shows as code */
    code?: string;
    doc?: string;
}

export interface CompletionEntry {
    label: string;
    kind: EntryKind;
    detail?: string;
    doc?: string;
    insertText?: string;
    /** a snippet (with $1 placeholders) rather than plain text */
    isSnippet?: boolean;
    sortText?: string;
    /** an import the completion adds, e.g. "math" */
    addImport?: string;
    /** for completions inside an import string: the text to replace starts here */
    replaceFrom?: number;
}

export interface SignatureResult {
    label: string;
    params: string[];
    activeParameter: number;
    doc?: string;
}

export interface Occurrence {
    file: string;
    line: number;
    character: number;
    length: number;
    isDeclaration: boolean;
}

const WORD = /[A-Za-z_][A-Za-z0-9_]*/g;

export function wordAt(line: string, character: number): { word: string; start: number; end: number } | undefined {
    WORD.lastIndex = 0;
    let match: RegExpExecArray | null;
    while ((match = WORD.exec(line))) {
        if (character >= match.index && character <= match.index + match[0].length)
            return { word: match[0], start: match.index, end: match.index + match[0].length };
    }
    return undefined;
}

/** True when the position is inside a comment or a string literal. */
export function inCommentOrString(parsed: ParsedFile, line: number, character: number): boolean {
    const { masked } = stripComments(parsed.lines.slice(0, line + 1));
    const code = masked[line] ?? "";
    const raw = parsed.lines[line] ?? "";

    // a comment: the masked line ends before the cursor although the raw line goes on
    if (character > code.length && raw.slice(code.length).trimStart().startsWith("/")) return true;
    if (code.slice(0, character).trim() === "" && raw.trim() !== "" && code.trim() === "") return true;

    // inside quotes: an odd number of quotes before the cursor
    let quotes = 0;
    for (let i = 0; i < Math.min(character, code.length); i++) if (code[i] === "\"") quotes++;
    return quotes % 2 === 1;
}

function hoverCode(symbol: ClearSymbol): string {
    switch (symbol.kind) {
        case "field":
            return `${symbol.container ? `(field) ${symbol.container}.` : ""}${symbol.detail}`;
        case "enumCase":
            return symbol.detail;
        case "parameter":
            return `(parameter) ${symbol.detail}`;
        case "typeParameter":
            return `(type parameter) ${symbol.name}`;
        case "variable":
        case "const":
            return symbol.detail;
        default:
            return symbol.detail;
    }
}

export class LanguageService {
    constructor(public project: Project, private standardLibrary: () => string | undefined) {}

    private parsed(file: string): ParsedFile | undefined {
        return this.project.parseFile(file);
    }

    // ---------------------------------------------------------------- what a name refers to

    resolve(file: string, line: number, character: number): Target | undefined {
        const parsed = this.parsed(file);
        if (!parsed) return undefined;
        const text = parsed.lines[line] ?? "";

        // the path in an import
        const importInfo = parsed.imports.find((i) => i.line === line && character >= i.start && character <= i.end);
        if (importInfo) {
            const resolved = this.project.resolveImport(file, importInfo.path);
            return {
                name: importInfo.path,
                kind: "module",
                file: resolved,
                line: 0,
                character: 0,
                code: `import "${importInfo.path}"${importInfo.alias ? ` as ${importInfo.alias}` : ""}`,
                doc: resolved ? `\`${resolved}\`${moduleDoc(resolved)}` : "Not found next to this file, in the project's packages or in the standard library.",
            };
        }

        const word = wordAt(text, character);
        if (!word) return undefined;

        // the declaration itself
        const declared = findDeclarationAt(parsed, line, word.start);
        if (declared && declared.name === word.word) return this.target(declared, file, parsed);

        // after a dot: a member of whatever is on the left
        const before = text.slice(0, word.start).trimEnd();
        if (before.endsWith(".") && !before.endsWith("..")) {
            const receiver = receiverBefore(before.replace(/\??\.$/, ""), before.replace(/\??\.$/, "").length);
            if (receiver) return this.memberTarget(file, parsed, receiver, word.word, line);
            return undefined;
        }

        return this.nameTarget(file, parsed, word.word, line);
    }

    private target(symbol: ClearSymbol, file: string, parsed: ParsedFile): Target {
        let code = hoverCode(symbol);
        if ((symbol.kind === "variable" || symbol.kind === "parameter" || symbol.kind === "const") && !symbol.type) {
            const inferred = this.project.typeOfLocal(file, parsed, symbol);
            if (inferred) code = `${symbol.kind === "parameter" ? "(parameter) " : symbol.kind === "const" ? "const " : "let "}${symbol.name}: ${inferred}`;
        }
        return { name: symbol.name, kind: symbol.kind, file, line: symbol.line, character: symbol.character, symbol, code, doc: symbol.doc };
    }

    private located(found: Located): Target {
        const parsed = this.parsed(found.file);
        return parsed ? this.target(found.symbol, found.file, parsed) : { name: found.symbol.name, kind: found.symbol.kind, symbol: found.symbol, code: found.symbol.detail, doc: found.symbol.doc };
    }

    private nameTarget(file: string, parsed: ParsedFile, name: string, line: number): Target | undefined {
        if (name === "self") {
            const owner = enclosingType(parsed, line);
            const local = localsAt(parsed, line).find((l) => l.name === "self");
            if (local) return { ...this.target(local, file, parsed), doc: KEYWORD_DOCS.self };
            if (owner) return { name, kind: "keyword", code: `self: *${owner.name}`, doc: KEYWORD_DOCS.self };
        }

        const local = localsAt(parsed, line).find((l) => l.name === name);
        if (local) return this.target(local, file, parsed);

        const alias = this.project.moduleAlias(file, parsed, name);
        if (alias) return { name, kind: "module", file: alias.file, line: 0, character: 0, code: `import "${alias.info!.path}" as ${name}`, doc: `\`${alias.file}\`${moduleDoc(alias.file)}` };

        // a field or method used without self. (inside a class body, e.g. in a field default)
        const top = this.project.lookupTopLevel(file, parsed, name);
        if (top) return this.located(top);

        const builtin = BUILTIN_FUNCTIONS.find((b) => b.name === name);
        if (builtin) return { name, kind: "builtin", code: builtin.signature, doc: builtin.doc };

        if (PRIMITIVE_DOCS[name] !== undefined) return { name, kind: "primitive", code: name, doc: PRIMITIVE_DOCS[name] };
        if (KEYWORD_DOCS[name] !== undefined) return { name, kind: "keyword", code: name, doc: KEYWORD_DOCS[name] };

        return undefined;
    }

    private memberTarget(file: string, parsed: ParsedFile, receiver: string, name: string, line: number): Target | undefined {
        for (const member of this.membersOf(file, parsed, receiver, line)) {
            if (member.label !== name) continue;
            if (member.located) return this.located(member.located);
            return { name, kind: member.kind, code: member.detail, doc: member.doc };
        }
        return undefined;
    }

    /** Everything `receiver.` can be followed by. */
    private membersOf(file: string, parsed: ParsedFile, receiver: string, line: number): (CompletionEntry & { located?: Located })[] {
        const fromLocated = (l: Located): CompletionEntry & { located?: Located } => ({
            label: l.symbol.name,
            kind: l.symbol.kind,
            detail: l.symbol.detail,
            doc: l.symbol.doc,
            located: l,
        });

        // super.method()
        if (receiver === "super") {
            const owner = enclosingType(parsed, line);
            return (owner?.bases ?? []).flatMap((base) => this.project.members(file, parsed, base)).filter(isUsableMember).map(fromLocated);
        }

        // module alias: geo.square
        const alias = /^[A-Za-z_][A-Za-z0-9_]*$/.test(receiver) && !localsAt(parsed, line).some((l) => l.name === receiver) ? this.project.moduleAlias(file, parsed, receiver) : undefined;
        if (alias) return alias.parsed.symbols.filter((s) => s.kind !== "operator").map((symbol) => fromLocated({ symbol, file: alias.file }));

        // a type name: Shape.Circle, Color.Red
        if (/^[A-Za-z_][A-Za-z0-9_.]*$/.test(receiver) && !localsAt(parsed, line).some((l) => l.name === receiver)) {
            const type = this.project.lookupType(file, parsed, receiver);
            if (type) return type.symbol.children.filter((c) => c.kind === "enumCase").map((symbol) => fromLocated({ symbol, file: type.file }));
        }

        const type = this.project.typeOf(file, parsed, receiver, line);
        if (!type) return [];

        const result: (CompletionEntry & { located?: Located })[] = [];
        const { name, args } = baseType(type);

        if (type.trim().startsWith("?")) {
            for (const m of syntheticMembers("optional")) result.push({ label: m.name, kind: m.detail.startsWith("function") ? "method" : "field", detail: m.detail.replace(/\bT\b/g, stripOptional(type)), doc: m.doc });
        }

        for (const m of syntheticMembers(name)) result.push({ label: m.name, kind: "method", detail: m.detail.replace(/\bT\b/g, args[0] ?? "T"), doc: m.doc });

        for (const member of this.project.members(file, parsed, stripOptional(type))) {
            if (!isUsableMember(member)) continue;
            if (result.some((r) => r.label === member.symbol.name)) continue;
            result.push(fromLocated(member));
        }

        return result;
    }

    // ---------------------------------------------------------------- hover

    hover(file: string, line: number, character: number): { code?: string; doc?: string } | undefined {
        const target = this.resolve(file, line, character);
        if (!target) return undefined;

        let doc = target.doc;
        if (target.symbol && target.file && target.kind !== "variable" && target.kind !== "parameter" && target.kind !== "typeParameter") {
            const where = this.describeLocation(file, target.file);
            if (where) doc = doc ? `${doc}\n\n${where}` : where;
        }

        return { code: target.code, doc };
    }

    private describeLocation(fromFile: string, declaredIn: string): string | undefined {
        if (path.resolve(fromFile) === path.resolve(declaredIn)) return undefined;
        const standard = this.standardLibrary();
        if (standard && path.resolve(declaredIn).startsWith(path.resolve(standard) + path.sep))
            return `*from the standard library: \`import "${path.basename(declaredIn, ".cl")}"\`*`;
        return `*from \`${path.relative(path.dirname(fromFile), declaredIn)}\`*`;
    }

    // ---------------------------------------------------------------- go to definition

    definition(file: string, line: number, character: number): { file: string; line: number; character: number; length: number } | undefined {
        const target = this.resolve(file, line, character);
        if (!target?.file || target.line === undefined) return undefined;
        return { file: target.file, line: target.line, character: target.character ?? 0, length: target.kind === "module" ? 0 : target.name.length };
    }

    // ---------------------------------------------------------------- completion

    completions(file: string, line: number, character: number): CompletionEntry[] {
        const parsed = this.parsed(file);
        if (!parsed) return [];
        const text = parsed.lines[line] ?? "";
        const before = text.slice(0, character);

        // import "...
        const importPrefix = /^\s*import\s+"([^"]*)$/.exec(before);
        if (importPrefix) return this.importCompletions(file, importPrefix[1], character - importPrefix[1].length);

        if (inCommentOrString(parsed, line, character)) return [];

        // after a dot
        const member = /([A-Za-z0-9_\])}]\s*)(\??\.)\s*([A-Za-z_][A-Za-z0-9_]*)?$/.exec(before);
        if (member && !/\.\.\s*[A-Za-z0-9_]*$/.test(before) && !/\d\.$/.test(before.replace(/[A-Za-z_][A-Za-z0-9_]*$/, ""))) {
            const dotAt = before.length - (member[3]?.length ?? 0) - member[2].length;
            const receiver = receiverBefore(text.slice(0, dotAt), dotAt);
            if (!receiver) return [];
            return this.membersOf(file, parsed, receiver, line).map(({ located, ...entry }) => ({
                ...entry,
                sortText: entry.kind === "field" || entry.kind === "enumCase" ? `0${entry.label}` : `1${entry.label}`,
            }));
        }

        return this.scopeCompletions(file, parsed, line, before);
    }

    private scopeCompletions(file: string, parsed: ParsedFile, line: number, before: string): CompletionEntry[] {
        const entries: CompletionEntry[] = [];
        const seen = new Set<string>();
        const add = (entry: CompletionEntry) => {
            if (seen.has(entry.label)) return;
            seen.add(entry.label);
            entries.push(entry);
        };

        const typeContext = /(:\s*[*?&\[\]0-9; ]*|->\s*[*?&\[\]0-9; ]*|\bas\s+|\bis\s+(not\s+)?|\[\s*|,\s*)[A-Za-z_]*$/.test(before) && !/^\s*(let|const)\s+[A-Za-z_]*$/.test(before);
        const statementStart = /^\s*[A-Za-z_]*$/.test(before);

        for (const local of localsAt(parsed, line)) {
            add({ label: local.name, kind: local.kind, detail: local.type ? `${local.name}: ${local.type}` : local.detail, sortText: `0${local.name}` });
        }

        // fields and methods of the class we are in, used through self
        const owner = enclosingType(parsed, line);
        const inMethod = enclosingFunction(parsed, line);
        if (owner && inMethod) add({ label: "self", kind: "keyword", detail: `self: *${owner.name}`, sortText: "0self" });

        for (const located of this.project.visibleSymbols(file, parsed)) {
            const s = located.symbol;
            if (s.kind === "operator") continue;
            const where = path.resolve(located.file) === path.resolve(file) ? undefined : path.basename(located.file);
            add({
                label: s.name,
                kind: s.kind,
                detail: s.detail,
                doc: [s.doc, where ? `*from ${where}*` : undefined].filter(Boolean).join("\n\n") || undefined,
                insertText: s.kind === "macro" ? `${s.name}!` : undefined,
                sortText: `${typeContext && isTypeKind(s.kind) ? "0" : "1"}${s.name}`,
            });
        }

        for (const module of this.project.modules(file, parsed)) {
            if (module.info?.alias) add({ label: module.info.alias, kind: "module", detail: `import "${module.info.path}" as ${module.info.alias}`, sortText: `1${module.info.alias}` });
        }

        for (const type of PRIMITIVE_TYPES) add({ label: type, kind: "primitive", detail: type, doc: PRIMITIVE_DOCS[type], sortText: `${typeContext ? "0" : "3"}${type}` });

        if (!typeContext) {
            for (const builtin of BUILTIN_FUNCTIONS) add({ label: builtin.name, kind: "builtin", detail: builtin.signature, doc: builtin.doc, sortText: `2${builtin.name}` });

            for (const keyword of KEYWORDS) {
                if (keyword === "self" || keyword === "super") continue;
                add({ label: keyword, kind: "keyword", doc: KEYWORD_DOCS[keyword], sortText: `${statementStart ? "2" : "4"}${keyword}` });
            }
            if (owner && inMethod && (owner.bases?.length ?? 0) > 0) add({ label: "super", kind: "keyword", doc: KEYWORD_DOCS.super, sortText: "2super" });

            // standard library functions that need an import: completing one adds it
            for (const entry of this.importableSymbols(file, parsed)) add(entry);
        } else {
            for (const name of ["Generator", "Task"]) add({ label: name, kind: "class", detail: name, doc: PRIMITIVE_DOCS[name], sortText: `0${name}` });
            add({ label: "function", kind: "keyword", doc: "A function type: `function(int) -> int`.", sortText: "1function" });
        }

        return entries;
    }

    /** Top-level declarations of standard modules this file does not import yet. */
    private importableSymbols(file: string, parsed: ParsedFile): CompletionEntry[] {
        const standard = this.standardLibrary();
        if (!standard) return [];

        const imported = new Set(this.project.modules(file, parsed).filter((m) => m.info).map((m) => path.resolve(m.file)));
        const result: CompletionEntry[] = [];

        let names: string[];
        try {
            names = fs.readdirSync(standard).filter((n) => n.endsWith(".cl"));
        } catch {
            return [];
        }

        for (const name of names) {
            const module = path.join(standard, name);
            if (imported.has(path.resolve(module)) || path.resolve(module) === path.resolve(file)) continue;
            const moduleParsed = this.project.parseFile(module);
            if (!moduleParsed) continue;
            const moduleName = name.replace(/\.cl$/, "");

            for (const symbol of moduleParsed.symbols) {
                // C declarations (malloc, fopen...) are the module's plumbing, not what people look for
                if (symbol.kind === "declare" && moduleName !== "math") continue;
                if (symbol.kind === "operator") continue;
                result.push({
                    label: symbol.name,
                    kind: symbol.kind,
                    detail: `${symbol.detail}  (import "${moduleName}")`,
                    doc: symbol.doc,
                    addImport: moduleName,
                    sortText: `5${symbol.name}`,
                });
            }
        }

        return result;
    }

    private importCompletions(file: string, typed: string, replaceFrom: number): CompletionEntry[] {
        const entries: CompletionEntry[] = [];
        const slash = typed.lastIndexOf("/");
        const folderTyped = slash >= 0 ? typed.slice(0, slash) : "";
        const listFolder = (folder: string, prefix: string, kind: "file" | "module") => {
            let names: fs.Dirent[];
            try {
                names = fs.readdirSync(folder, { withFileTypes: true });
            } catch {
                return;
            }
            for (const entry of names) {
                if (entry.name.startsWith(".") || entry.name === "node_modules" || entry.name === "build") continue;
                if (entry.isDirectory()) entries.push({ label: `${prefix}${entry.name}/`, kind: "folder", insertText: `${prefix}${entry.name}/`, replaceFrom, sortText: `1${entry.name}` });
                else if (entry.name.endsWith(".cl") && path.join(folder, entry.name) !== path.resolve(file)) {
                    const module = entry.name.slice(0, -3);
                    entries.push({ label: `${prefix}${module}`, kind, detail: path.join(folder, entry.name), doc: moduleDoc(path.join(folder, entry.name)) || undefined, replaceFrom, sortText: `${kind === "module" ? "2" : "0"}${module}` });
                }
            }
        };

        const prefix = folderTyped === "" ? "" : `${folderTyped}/`;
        listFolder(path.join(path.dirname(file), folderTyped), prefix, "file");

        const standard = this.standardLibrary();
        if (standard) {
            if (folderTyped === "") listFolder(standard, "", "module");
            if (folderTyped === "std") listFolder(standard, "std/", "module");
        }

        const root = this.project.projectRoot(file);
        if (root) {
            for (const [name, pkg] of this.project.packages(root)) {
                if (folderTyped === "") entries.push({ label: name, kind: "module", detail: `package ${name}`, replaceFrom, sortText: `1${name}` });
                else if (folderTyped.split("/")[0] === name) listFolder(path.join(pkg.directory, folderTyped.split("/").slice(1).join("/")), prefix, "module");
            }
        }

        const unique = new Map<string, CompletionEntry>();
        for (const entry of entries) if (!unique.has(entry.label)) unique.set(entry.label, entry);
        return [...unique.values()];
    }

    // ---------------------------------------------------------------- signature help

    signature(file: string, line: number, character: number): SignatureResult | undefined {
        const parsed = this.parsed(file);
        if (!parsed) return undefined;

        // join a few lines above so calls that span lines still work
        const first = Math.max(0, line - 6);
        const { masked } = stripComments(parsed.lines.slice(0, line + 1));
        const lines = masked.slice(first, line + 1);
        lines[lines.length - 1] = lines[lines.length - 1].slice(0, character);
        const text = lines.join(" ");
        const raw = parsed.lines.slice(first, line + 1).map((l, i) => (i === lines.length - 1 ? l.slice(0, character) : l)).join(" ");

        let depth = 0;
        let commas = 0;
        for (let i = text.length - 1; i >= 0; i--) {
            const c = text[i];
            if (c === ")" || c === "]" || c === "}") depth++;
            else if (c === "[" || c === "{") {
                if (depth === 0) return undefined;
                depth--;
            } else if (c === "(") {
                if (depth > 0) {
                    depth--;
                    continue;
                }
                let calleeEnd = i;
                const isMacro = raw[i - 1] === "!";
                if (isMacro) calleeEnd--;
                const callee = receiverBefore(raw.slice(0, calleeEnd), calleeEnd);
                if (!callee) return undefined;
                return this.signatureOf(file, parsed, callee, line, commas, isMacro);
            } else if (c === "," && depth === 0) commas++;
        }

        return undefined;
    }

    private signatureOf(file: string, parsed: ParsedFile, callee: string, line: number, active: number, isMacro: boolean): SignatureResult | undefined {
        // generic arguments: largest[float64](
        const bare = callee.replace(/\[[^\]]*\]$/, "");
        const dot = bare.lastIndexOf(".");
        let target: Target | undefined;

        if (dot > 0) {
            const receiver = bare.slice(0, dot).replace(/\?$/, "");
            target = this.memberTarget(file, parsed, receiver, bare.slice(dot + 1), line);
        } else {
            target = this.nameTarget(file, parsed, bare, line);
        }

        if (!target) return undefined;

        if (target.kind === "builtin") {
            const builtin = BUILTIN_FUNCTIONS.find((b) => b.name === target!.name)!;
            return { label: builtin.signature, params: builtin.params, activeParameter: Math.min(active, Math.max(0, builtin.params.length - 1)), doc: builtin.doc };
        }

        const symbol = target.symbol;
        if (!symbol) return undefined;

        const format = (p: { name: string; type?: string; defaultValue?: string }) => `${p.name}${p.type ? `: ${p.type}` : ""}${p.defaultValue ? ` = ${p.defaultValue}` : ""}`;

        // a constructor: init's parameters, or the fields in order
        if (symbol.kind === "class" || symbol.kind === "union") {
            const init = symbol.children.find((c) => c.kind === "method" && c.name === "init");
            const params = init
                ? (init.params ?? []).filter((p) => p.name !== "self").map(format)
                : symbol.children.filter((c) => c.kind === "field").map((f) => format({ name: f.name, type: f.type, defaultValue: f.initializer }));
            return { label: `${symbol.name}(${params.join(", ")})`, params, activeParameter: active, doc: init?.doc ?? symbol.doc };
        }

        // a local holding a function: let f: function(int, int) -> int
        if (symbol.kind === "variable" || symbol.kind === "parameter") {
            const type = symbol.type ?? (target.file ? this.project.typeOfLocal(target.file, this.parsed(target.file) ?? parsed, symbol) : undefined);
            const functionType = type && /^function\s*\((.*)\)\s*(->.*)?$/.exec(type.trim());
            if (!functionType) return undefined;
            const params = splitTopLevel(functionType[1]);
            return { label: `${symbol.name}(${params.join(", ")})${functionType[2] ? ` ${functionType[2]}` : ""}`, params, activeParameter: active };
        }

        if (!symbol.params) return undefined;

        let params = symbol.params;
        if ((symbol.kind === "method" || dot > 0) && params[0]?.name === "self") params = params.slice(1);

        const formatted = params.map(format);
        const label = `${isMacro ? `${symbol.name}!` : symbol.name}(${formatted.join(", ")})${symbol.returnType ? ` -> ${symbol.returnType}` : ""}`;
        const variadic = formatted.length > 0 && /\.\.\./.test(formatted[formatted.length - 1]);
        return { label, params: formatted, activeParameter: variadic ? Math.min(active, formatted.length - 1) : active, doc: symbol.doc };
    }

    // ---------------------------------------------------------------- references and rename

    /** Every use of the name at the position, in the given files (whole words outside comments and strings). */
    references(file: string, line: number, character: number, files: string[]): Occurrence[] {
        const target = this.resolve(file, line, character);
        const parsed = this.parsed(file);
        if (!target || !parsed || !target.symbol || target.kind === "module") return [];

        const symbol = target.symbol;
        const name = symbol.name;
        const isLocal = symbol.kind === "variable" && symbol.scopeStart !== undefined || symbol.kind === "parameter" || symbol.kind === "typeParameter" || (symbol.kind === "const" && symbol.scopeStart !== undefined);
        const isMember = symbol.container !== undefined && symbol.kind !== "parameter" || symbol.kind === "enumCase";

        const searchIn = isLocal ? [file] : unique([file, ...files, ...(target.file ? [target.file] : [])]);
        const result: Occurrence[] = [];

        for (const candidate of searchIn) {
            const candidateParsed = this.parsed(candidate);
            if (!candidateParsed) continue;
            const { masked } = stripComments(candidateParsed.lines);

            for (let l = 0; l < masked.length; l++) {
                if (isLocal && (l < (symbol.scopeStart ?? 0) || l > (symbol.scopeEnd ?? masked.length))) continue;

                const pattern = new RegExp(`\\b${name}\\b`, "g");
                let match: RegExpExecArray | null;
                while ((match = pattern.exec(masked[l]))) {
                    const prefix = masked[l].slice(0, match.index).trimEnd();
                    const afterDot = prefix.endsWith(".") && !prefix.endsWith("..");
                    const isDeclaration = path.resolve(candidate) === path.resolve(target.file ?? file) && l === symbol.line && match.index === symbol.character;

                    if (!isDeclaration) {
                        if (isMember && !afterDot && !(symbol.kind === "enumCase" && /\bcase\s+$/.test(prefix))) {
                            // a member used without a dot: only inside its own class (fields in methods via `self.` are dotted)
                            continue;
                        }
                        if (!isMember && afterDot) continue;

                        // make sure this occurrence means the same thing
                        const resolved = this.resolve(candidate, l, match.index);
                        if (resolved?.symbol && (resolved.file !== undefined && target.file !== undefined) &&
                            (path.resolve(resolved.file) !== path.resolve(target.file) || resolved.line !== target.line || resolved.character !== target.character))
                            continue;
                        if (!resolved?.symbol && isLocal) continue;
                    }

                    result.push({ file: candidate, line: l, character: match.index, length: name.length, isDeclaration });
                }
            }
        }

        return result;
    }

    /** Whether a rename at the position is safe: the name must be declared in one of the user's files. */
    canRename(file: string, line: number, character: number): { name: string; start: number } | string {
        const parsed = this.parsed(file);
        const target = this.resolve(file, line, character);
        if (!parsed || !target?.symbol || !target.file) return "Only names declared in Clear files can be renamed.";
        if (target.kind === "operator") return "Operators have fixed names.";
        if (target.symbol.name === "init" || target.symbol.name === "self") return `'${target.symbol.name}' has a fixed meaning.`;

        const standard = this.standardLibrary();
        if (standard && path.resolve(target.file).startsWith(path.resolve(standard) + path.sep)) return "Names from the standard library can't be renamed.";
        if (target.file.includes(`${path.sep}.clear${path.sep}packages${path.sep}`)) return "Names from a package can't be renamed here.";

        const word = wordAt(parsed.lines[line] ?? "", character);
        return word ? { name: word.word, start: word.start } : "Nothing to rename here.";
    }

    // ---------------------------------------------------------------- inferred types

    /** `let` variables without a written type, with the type the code gives them. */
    inferredTypes(file: string, startLine: number, endLine: number): { line: number; character: number; type: string }[] {
        const parsed = this.parsed(file);
        if (!parsed) return [];
        const result: { line: number; character: number; type: string }[] = [];

        for (const local of parsed.locals) {
            if (local.line < startLine || local.line > endLine) continue;
            if (local.kind !== "variable" || local.type) continue;
            if (!/^(let|const|for)\b/.test(local.detail) && !local.detail.includes(":=")) continue;
            if (local.detail.startsWith("for") && /\.\.=?/.test(local.iterates ?? "")) continue;
            const type = this.project.typeOfLocal(file, parsed, local);
            if (!type || type === "function") continue;
            // a constructor call already says the type: let list = List[int]()
            if (local.initializer && /^[(\[{]/.test(local.initializer.replace(/\s/g, "").slice(type.replace(/\s/g, "").length)) && local.initializer.replace(/\s/g, "").startsWith(type.replace(/\s/g, ""))) continue;
            result.push({ line: local.line, character: local.character + local.name.length, type });
        }

        for (const symbol of parsed.symbols) {
            if ((symbol.kind !== "variable" && symbol.kind !== "const") || symbol.type || !symbol.initializer) continue;
            if (symbol.line < startLine || symbol.line > endLine) continue;
            const type = this.project.typeOf(file, parsed, symbol.initializer, symbol.line);
            if (type && type !== "function") result.push({ line: symbol.line, character: symbol.character + symbol.name.length, type });
        }

        return result;
    }

    // ---------------------------------------------------------------- quick fixes

    /** Standard modules that declare `name` at the top level. */
    modulesDeclaring(name: string): string[] {
        const standard = this.standardLibrary();
        if (!standard) return [];
        let names: string[];
        try {
            names = fs.readdirSync(standard).filter((n) => n.endsWith(".cl"));
        } catch {
            return [];
        }
        return names.filter((n) => this.project.parseFile(path.join(standard, n))?.symbols.some((s) => s.name === name)).map((n) => n.replace(/\.cl$/, ""));
    }
}

function isUsableMember(l: Located): boolean {
    return l.symbol.kind !== "operator" && l.symbol.name !== "init";
}

function unique(files: string[]): string[] {
    return [...new Set(files.map((f) => path.resolve(f)))];
}

/** The opening comment of a module, shown when hovering its import. */
function moduleDoc(file: string): string {
    try {
        const lines = fs.readFileSync(file, "utf8").split(/\r?\n/);
        const doc: string[] = [];
        for (const line of lines) {
            const trimmed = line.trim();
            if (!trimmed.startsWith("//")) break;
            doc.push(trimmed.replace(/^\/\/ ?/, ""));
        }
        return doc.length > 0 ? `\n\n${doc.join("\n")}` : "";
    } catch {
        return "";
    }
}
