import * as vscode from "vscode";
import * as cp from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { ClearSymbol, SymbolKind } from "./analyzer";
import { Project } from "./project";
import { LanguageService, EntryKind } from "./service";
import { parseDiagnostics, parseLooseError } from "./diagnostics";

const LANGUAGE = "clear";
const SELECTOR: vscode.DocumentSelector = [{ language: LANGUAGE, scheme: "file" }, { language: LANGUAGE, scheme: "untitled" }];

let extensionRoot = "";

function config() {
    return vscode.workspace.getConfiguration("clear");
}

// ---------------------------------------------------------------- finding clearc and the standard library

function isFile(p: string): boolean {
    try {
        return fs.statSync(p).isFile();
    } catch {
        return false;
    }
}

function isDirectory(p: string): boolean {
    try {
        return fs.statSync(p).isDirectory();
    } catch {
        return false;
    }
}

function expandHome(p: string): string {
    return p.startsWith("~") ? path.join(process.env.HOME ?? process.env.USERPROFILE ?? "", p.slice(1)) : p;
}

function workspaceRoots(): string[] {
    return (vscode.workspace.workspaceFolders ?? []).map((f) => f.uri.fsPath);
}

/** clearc: the setting, then build/clearc in the workspace (working on the compiler), then the PATH. */
function compilerPath(): string {
    const configured = config().get<string>("compilerPath", "").trim();
    if (configured) {
        const expanded = expandHome(configured);
        if (path.isAbsolute(expanded) || !expanded.includes(path.sep)) return expanded;
        for (const root of workspaceRoots()) if (isFile(path.join(root, expanded))) return path.join(root, expanded);
        return expanded;
    }

    const exe = process.platform === "win32" ? "clearc.exe" : "clearc";
    for (const root of workspaceRoots()) {
        for (const candidate of [path.join(root, "build", exe), path.join(root, "build", "Release", exe), path.join(root, "out", "build", exe)])
            if (isFile(candidate)) return candidate;
    }

    return "clearc";
}

function standardLibrary(): string | undefined {
    const configured = config().get<string>("standardLibraryPath", "").trim();
    const candidates = [
        configured ? expandHome(configured) : "",
        process.env.CLEAR_STANDARD_DIR ?? "",
        ...workspaceRoots().map((root) => path.join(root, "Standard")),
        path.join(extensionRoot, "standard"),
    ];
    return candidates.find((c) => c !== "" && isFile(path.join(c, "list.cl")));
}

// ---------------------------------------------------------------- conversions

function completionKind(kind: EntryKind): vscode.CompletionItemKind {
    switch (kind) {
        case "function": case "declare": case "builtin": return vscode.CompletionItemKind.Function;
        case "method": case "operator": return vscode.CompletionItemKind.Method;
        case "property": return vscode.CompletionItemKind.Property;
        case "macro": return vscode.CompletionItemKind.Snippet;
        case "class": case "union": return vscode.CompletionItemKind.Class;
        case "trait": return vscode.CompletionItemKind.Interface;
        case "enum": case "variant": return vscode.CompletionItemKind.Enum;
        case "enumCase": return vscode.CompletionItemKind.EnumMember;
        case "field": return vscode.CompletionItemKind.Field;
        case "const": return vscode.CompletionItemKind.Constant;
        case "variable": return vscode.CompletionItemKind.Variable;
        case "parameter": return vscode.CompletionItemKind.Variable;
        case "typeParameter": return vscode.CompletionItemKind.TypeParameter;
        case "module": return vscode.CompletionItemKind.Module;
        case "keyword": return vscode.CompletionItemKind.Keyword;
        case "primitive": return vscode.CompletionItemKind.Struct;
        case "file": return vscode.CompletionItemKind.File;
        case "folder": return vscode.CompletionItemKind.Folder;
        default: return vscode.CompletionItemKind.Text;
    }
}

function symbolKind(kind: SymbolKind): vscode.SymbolKind {
    switch (kind) {
        case "function": case "declare": case "macro": return vscode.SymbolKind.Function;
        case "method": return vscode.SymbolKind.Method;
        case "operator": return vscode.SymbolKind.Operator;
        case "property": return vscode.SymbolKind.Property;
        case "class": case "union": return vscode.SymbolKind.Class;
        case "trait": return vscode.SymbolKind.Interface;
        case "enum": case "variant": return vscode.SymbolKind.Enum;
        case "enumCase": return vscode.SymbolKind.EnumMember;
        case "field": return vscode.SymbolKind.Field;
        case "const": return vscode.SymbolKind.Constant;
        case "typeParameter": return vscode.SymbolKind.TypeParameter;
        default: return vscode.SymbolKind.Variable;
    }
}

function documentSymbol(document: vscode.TextDocument, symbol: ClearSymbol): vscode.DocumentSymbol {
    const last = Math.min(symbol.endLine, document.lineCount - 1);
    const range = new vscode.Range(symbol.line, 0, last, document.lineAt(last).text.length);
    const selection = new vscode.Range(symbol.line, symbol.character, symbol.line, symbol.character + symbol.name.length);
    const detail = symbol.kind === "field" ? symbol.type ?? "" : symbol.returnType ? `-> ${symbol.returnType}` : "";
    const result = new vscode.DocumentSymbol(symbol.name, detail, symbolKind(symbol.kind), range.contains(selection) ? range : selection, selection);
    result.children = symbol.children.map((child) => documentSymbol(document, child));
    return result;
}

function markdown(code: string | undefined, doc: string | undefined): vscode.MarkdownString {
    const result = new vscode.MarkdownString();
    if (code) result.appendCodeblock(code, LANGUAGE);
    if (doc) result.appendMarkdown(doc);
    return result;
}

/**
 * Where an `import` line goes: after the last import; else after the file's opening comment when a
 * blank line separates it from the code (a comment touching a declaration documents that declaration).
 */
function importInsertLine(document: vscode.TextDocument): number {
    const lines = document.getText().split(/\r?\n/);
    let lastImport = -1;
    lines.forEach((line, i) => {
        if (/^\s*import\s+"/.test(line)) lastImport = i;
    });
    if (lastImport >= 0) return lastImport + 1;

    let i = 0;
    while (i < lines.length && lines[i].trim().startsWith("//")) i++;
    return i > 0 && i < lines.length && lines[i].trim() === "" ? i + 1 : 0;
}

function importEdit(document: vscode.TextDocument, module: string): vscode.TextEdit {
    const line = importInsertLine(document);
    const hasImports = document.getText().split(/\r?\n/).some((l) => /^\s*import\s+"/.test(l));
    const next = line < document.lineCount ? document.lineAt(line).text.trim() : "";
    const text = `import "${module}"\n${!hasImports && next !== "" ? "\n" : ""}`;
    return vscode.TextEdit.insert(new vscode.Position(line, 0), text);
}

// ---------------------------------------------------------------- diagnostics from clearc check

class Checker implements vscode.Disposable {
    private collection = vscode.languages.createDiagnosticCollection(LANGUAGE);
    /** the files each checked file reported problems in (its imports can have errors too) */
    private reported = new Map<string, Set<string>>();
    private timers = new Map<string, NodeJS.Timeout>();
    private running = new Map<string, cp.ChildProcess>();
    private warnedMissing = false;

    constructor(private output: vscode.OutputChannel) {}

    dispose() {
        this.collection.dispose();
        for (const timer of this.timers.values()) clearTimeout(timer);
        for (const child of this.running.values()) child.kill();
    }

    clear(document: vscode.TextDocument) {
        const key = document.uri.fsPath;
        for (const file of this.reported.get(key) ?? []) this.collection.delete(vscode.Uri.file(file));
        this.reported.delete(key);
    }

    schedule(document: vscode.TextDocument, delay: number) {
        if (document.languageId !== LANGUAGE || document.uri.scheme !== "file" || !config().get<boolean>("diagnostics.enable", true)) return;
        const key = document.uri.fsPath;
        clearTimeout(this.timers.get(key));
        this.timers.set(key, setTimeout(() => {
            this.timers.delete(key);
            this.check(document);
        }, delay));
    }

    check(document: vscode.TextDocument) {
        const file = document.uri.fsPath;
        this.running.get(file)?.kill();

        // unsaved text is checked from a hidden copy next to the file, so its imports still resolve
        let checked = file;
        let temporary: string | undefined;
        if (document.isDirty) {
            temporary = path.join(path.dirname(file), `.${path.basename(file, ".cl")}.clear-check.cl`);
            try {
                fs.writeFileSync(temporary, document.getText());
                checked = temporary;
            } catch {
                temporary = undefined;
            }
        }

        const compiler = compilerPath();
        const child = cp.execFile(compiler, ["check", checked], { cwd: path.dirname(file), timeout: 60000, maxBuffer: 16 * 1024 * 1024 }, (error, stdout, stderr) => {
            if (temporary) fs.rm(temporary, () => undefined);
            if (this.running.get(file) !== child) return;
            this.running.delete(file);

            if (error && (error as NodeJS.ErrnoException).code === "ENOENT") {
                this.reportMissingCompiler(compiler);
                return;
            }

            const output = `${stdout}\n${stderr}`;
            const found = parseDiagnostics(output);
            const byFile = new Map<string, vscode.Diagnostic[]>();

            for (const d of found) {
                let target = d.file ? path.resolve(path.dirname(file), d.file) : file;
                if (temporary && path.resolve(target) === path.resolve(temporary)) target = file;

                const range = new vscode.Range(d.line, d.character, d.line, d.character + Math.max(1, d.length));
                const severity = d.severity === "error" ? vscode.DiagnosticSeverity.Error : d.severity === "warning" ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Information;
                const diagnostic = new vscode.Diagnostic(range, d.help ? `${d.message}\n${d.help}` : d.message, severity);
                diagnostic.source = "clearc";
                diagnostic.code = d.code;
                if (!byFile.has(target)) byFile.set(target, []);
                byFile.get(target)!.push(diagnostic);
            }

            // something went wrong without the usual error layout (a crash, a broken clear.toml)
            if (found.length === 0 && error && typeof error.code === "number") {
                const message = parseLooseError(output) ?? output.trim().split(/\r?\n/)[0] ?? "clearc check failed";
                const diagnostic = new vscode.Diagnostic(new vscode.Range(0, 0, 0, 0), message, vscode.DiagnosticSeverity.Error);
                diagnostic.source = "clearc";
                byFile.set(file, [diagnostic]);
                this.output.appendLine(`clearc check ${file}:\n${output.trim()}`);
            }

            for (const previous of this.reported.get(file) ?? []) if (!byFile.has(previous)) this.collection.delete(vscode.Uri.file(previous));
            for (const [target, diagnostics] of byFile) this.collection.set(vscode.Uri.file(target), diagnostics);
            if (!byFile.has(file)) this.collection.delete(document.uri);
            this.reported.set(file, new Set(byFile.keys()));
        });

        this.running.set(file, child);
    }

    private reportMissingCompiler(compiler: string) {
        if (this.warnedMissing) return;
        this.warnedMissing = true;
        vscode.window.showWarningMessage(
            `Clear: could not run '${compiler}', so errors are not shown. Build clearc or set "clear.compilerPath".`,
            "Open Settings",
        ).then((choice) => {
            if (choice) vscode.commands.executeCommand("workbench.action.openSettings", "clear.compilerPath");
        });
    }

    resetWarning() {
        this.warnedMissing = false;
    }
}

// ---------------------------------------------------------------- running clearc in a terminal

function quote(arg: string): string {
    if (/^[A-Za-z0-9_./:=+-]+$/.test(arg)) return arg;
    if (process.platform === "win32") return `"${arg.replace(/"/g, '""')}"`;
    return `'${arg.replace(/'/g, "'\\''")}'`;
}

let terminal: vscode.Terminal | undefined;

async function runInTerminal(args: string[], cwd: string) {
    if (!terminal || terminal.exitStatus !== undefined) terminal = vscode.window.createTerminal({ name: "Clear", cwd });
    terminal.show(true);
    const command = [compilerPath(), ...args].map(quote).join(" ");
    terminal.sendText(`cd ${quote(cwd)} && ${command}`);
}

async function activeClearFile(): Promise<vscode.TextDocument | undefined> {
    const document = vscode.window.activeTextEditor?.document;
    if (!document || document.languageId !== LANGUAGE) {
        vscode.window.showInformationMessage("Clear: open a .cl file first.");
        return undefined;
    }
    if (document.isUntitled) {
        vscode.window.showInformationMessage("Clear: save the file first.");
        return undefined;
    }
    if (document.isDirty) await document.save();
    return document;
}

class TaskProvider implements vscode.TaskProvider {
    provideTasks(): vscode.Task[] {
        const tasks: vscode.Task[] = [];
        for (const folder of vscode.workspace.workspaceFolders ?? []) {
            if (!fs.existsSync(path.join(folder.uri.fsPath, "clear.toml"))) continue;
            for (const command of ["run", "build", "check"]) tasks.push(this.make({ type: LANGUAGE, command }, folder));
        }
        return tasks;
    }

    resolveTask(task: vscode.Task): vscode.Task | undefined {
        const definition = task.definition as { type: string; command: string; file?: string; args?: string[] };
        if (!definition.command) return undefined;
        const folder = typeof task.scope === "object" ? task.scope as vscode.WorkspaceFolder : vscode.workspace.workspaceFolders?.[0];
        return this.make(definition, folder);
    }

    private make(definition: { type: string; command: string; file?: string; args?: string[] }, folder?: vscode.WorkspaceFolder): vscode.Task {
        const target = definition.file ?? (folder ? folder.uri.fsPath : ".");
        const args = [definition.command, target, ...(definition.args ?? (definition.command === "check" ? [] : config().get<string[]>("run.arguments", [])))];
        const execution = new vscode.ProcessExecution(compilerPath(), args, { cwd: folder?.uri.fsPath });
        const task = new vscode.Task(definition, folder ?? vscode.TaskScope.Workspace, definition.command, "clear", execution, "$clearc");
        if (definition.command === "build") task.group = vscode.TaskGroup.Build;
        return task;
    }
}

// ---------------------------------------------------------------- activation

export function activate(context: vscode.ExtensionContext) {
    extensionRoot = context.extensionPath;
    const output = vscode.window.createOutputChannel("Clear");

    const project = new Project({
        standardLibrary,
        openDocument: (file) => {
            const document = vscode.workspace.textDocuments.find((d) => d.uri.scheme === "file" && path.resolve(d.uri.fsPath) === path.resolve(file));
            return document ? { text: document.getText(), version: document.version } : undefined;
        },
    });
    const service = new LanguageService(project, standardLibrary);
    const checker = new Checker(output);
    const fileOf = (document: vscode.TextDocument) => {
        if (document.uri.scheme === "file") return document.uri.fsPath;
        // an untitled document: parse its text under a made-up name next to the workspace
        const file = path.join(workspaceRoots()[0] ?? process.cwd(), `untitled-${document.uri.path}.cl`);
        project.parseText(file, document.getText(), document.version);
        return file;
    };

    context.subscriptions.push(output, checker);

    // ------------------------------------------------ completion
    context.subscriptions.push(vscode.languages.registerCompletionItemProvider(SELECTOR, {
        provideCompletionItems(document, position) {
            const file = fileOf(document);
            const entries = service.completions(file, position.line, position.character);
            return entries.map((entry) => {
                const item = new vscode.CompletionItem(entry.label, completionKind(entry.kind));
                item.detail = entry.detail;
                if (entry.doc) item.documentation = new vscode.MarkdownString(entry.doc);
                item.sortText = entry.sortText;
                if (entry.insertText) item.insertText = entry.isSnippet ? new vscode.SnippetString(entry.insertText) : entry.insertText;
                if (entry.replaceFrom !== undefined) {
                    const line = document.lineAt(position.line).text;
                    let end = position.character;
                    while (end < line.length && line[end] !== "\"") end++;
                    item.range = new vscode.Range(position.line, entry.replaceFrom, position.line, end);
                    item.filterText = entry.label;
                    if (entry.kind === "folder") item.command = { command: "editor.action.triggerSuggest", title: "" };
                }
                if (entry.addImport) {
                    item.additionalTextEdits = [importEdit(document, entry.addImport)];
                    item.label = { label: entry.label, description: `import "${entry.addImport}"` };
                }
                return item;
            });
        },
    }, ".", "\"", "/"));

    // ------------------------------------------------ hover, definition, signature help
    context.subscriptions.push(vscode.languages.registerHoverProvider(SELECTOR, {
        provideHover(document, position) {
            const result = service.hover(fileOf(document), position.line, position.character);
            if (!result || (!result.code && !result.doc)) return undefined;
            return new vscode.Hover(markdown(result.code, result.doc), document.getWordRangeAtPosition(position));
        },
    }));

    context.subscriptions.push(vscode.languages.registerDefinitionProvider(SELECTOR, {
        provideDefinition(document, position) {
            const target = service.definition(fileOf(document), position.line, position.character);
            if (!target) return undefined;
            return new vscode.Location(vscode.Uri.file(target.file), new vscode.Range(target.line, target.character, target.line, target.character + target.length));
        },
    }));

    context.subscriptions.push(vscode.languages.registerSignatureHelpProvider(SELECTOR, {
        provideSignatureHelp(document, position) {
            const result = service.signature(fileOf(document), position.line, position.character);
            if (!result) return undefined;
            const signature = new vscode.SignatureInformation(result.label, result.doc ? new vscode.MarkdownString(result.doc) : undefined);
            signature.parameters = result.params.map((p) => new vscode.ParameterInformation(p));
            const help = new vscode.SignatureHelp();
            help.signatures = [signature];
            help.activeSignature = 0;
            help.activeParameter = result.activeParameter;
            return help;
        },
    }, { triggerCharacters: ["(", ","], retriggerCharacters: [")"] }));

    // ------------------------------------------------ outline and workspace symbols
    context.subscriptions.push(vscode.languages.registerDocumentSymbolProvider(SELECTOR, {
        provideDocumentSymbols(document) {
            const parsed = project.parseFile(fileOf(document));
            return parsed ? parsed.symbols.map((s) => documentSymbol(document, s)) : [];
        },
    }));

    const workspaceFiles = async (): Promise<string[]> => {
        const uris = await vscode.workspace.findFiles("**/*.cl", "{**/node_modules/**,**/build/**,**/.git/**}", 5000);
        return uris.map((u) => u.fsPath);
    };

    context.subscriptions.push(vscode.languages.registerWorkspaceSymbolProvider({
        async provideWorkspaceSymbols(query) {
            const lower = query.toLowerCase();
            const result: vscode.SymbolInformation[] = [];
            const matches = (name: string) => {
                // the letters of the query in order, like VS Code's own fuzzy matching
                let i = 0;
                for (const c of name.toLowerCase()) if (c === lower[i]) i++;
                return i === lower.length;
            };
            for (const file of await workspaceFiles()) {
                const parsed = project.parseFile(file);
                if (!parsed) continue;
                const visit = (symbols: ClearSymbol[], container?: string) => {
                    for (const s of symbols) {
                        if (matches(s.name))
                            result.push(new vscode.SymbolInformation(s.name, symbolKind(s.kind), container ?? "", new vscode.Location(vscode.Uri.file(file), new vscode.Position(s.line, s.character))));
                        visit(s.children, s.name);
                    }
                };
                visit(parsed.symbols);
            }
            return result;
        },
    }));

    // ------------------------------------------------ references, highlights, rename
    context.subscriptions.push(vscode.languages.registerReferenceProvider(SELECTOR, {
        async provideReferences(document, position, options) {
            const occurrences = service.references(fileOf(document), position.line, position.character, await workspaceFiles());
            return occurrences
                .filter((o) => options.includeDeclaration || !o.isDeclaration)
                .map((o) => new vscode.Location(vscode.Uri.file(o.file), new vscode.Range(o.line, o.character, o.line, o.character + o.length)));
        },
    }));

    context.subscriptions.push(vscode.languages.registerDocumentHighlightProvider(SELECTOR, {
        provideDocumentHighlights(document, position) {
            const file = fileOf(document);
            return service.references(file, position.line, position.character, [file])
                .filter((o) => path.resolve(o.file) === path.resolve(file))
                .map((o) => new vscode.DocumentHighlight(new vscode.Range(o.line, o.character, o.line, o.character + o.length), o.isDeclaration ? vscode.DocumentHighlightKind.Write : vscode.DocumentHighlightKind.Read));
        },
    }));

    context.subscriptions.push(vscode.languages.registerRenameProvider(SELECTOR, {
        prepareRename(document, position) {
            const result = service.canRename(fileOf(document), position.line, position.character);
            if (typeof result === "string") throw new Error(result);
            return { range: new vscode.Range(position.line, result.start, position.line, result.start + result.name.length), placeholder: result.name };
        },
        async provideRenameEdits(document, position, newName) {
            if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(newName)) throw new Error(`'${newName}' is not a valid name.`);
            const edit = new vscode.WorkspaceEdit();
            const standard = standardLibrary();
            for (const o of service.references(fileOf(document), position.line, position.character, await workspaceFiles())) {
                if (standard && path.resolve(o.file).startsWith(path.resolve(standard) + path.sep)) continue;
                edit.replace(vscode.Uri.file(o.file), new vscode.Range(o.line, o.character, o.line, o.character + o.length), newName);
            }
            return edit;
        },
    }));

    // ------------------------------------------------ inferred types as inlay hints
    context.subscriptions.push(vscode.languages.registerInlayHintsProvider(SELECTOR, {
        provideInlayHints(document, range) {
            if (!config().get<boolean>("inlayHints.variableTypes", true)) return [];
            return service.inferredTypes(fileOf(document), range.start.line, range.end.line).map((h) => {
                const hint = new vscode.InlayHint(new vscode.Position(h.line, h.character), `: ${h.type}`, vscode.InlayHintKind.Type);
                hint.paddingLeft = false;
                return hint;
            });
        },
    }));

    // ------------------------------------------------ quick fix: add the missing import
    context.subscriptions.push(vscode.languages.registerCodeActionsProvider(SELECTOR, {
        provideCodeActions(document, _range, context) {
            const actions: vscode.CodeAction[] = [];
            for (const diagnostic of context.diagnostics) {
                if (diagnostic.source !== "clearc") continue;
                const name = document.getText(diagnostic.range);
                if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(name)) continue;
                for (const module of service.modulesDeclaring(name)) {
                    const action = new vscode.CodeAction(`Add import "${module}"`, vscode.CodeActionKind.QuickFix);
                    action.edit = new vscode.WorkspaceEdit();
                    action.edit.set(document.uri, [importEdit(document, module)]);
                    action.diagnostics = [diagnostic];
                    action.isPreferred = true;
                    actions.push(action);
                }
            }
            return actions;
        },
    }, { providedCodeActionKinds: [vscode.CodeActionKind.QuickFix] }));

    // ------------------------------------------------ links: import paths open the module
    context.subscriptions.push(vscode.languages.registerDocumentLinkProvider(SELECTOR, {
        provideDocumentLinks(document) {
            const file = fileOf(document);
            const parsed = project.parseFile(file);
            if (!parsed) return [];
            const links: vscode.DocumentLink[] = [];
            for (const info of parsed.imports) {
                const resolved = project.resolveImport(file, info.path);
                if (resolved) links.push(new vscode.DocumentLink(new vscode.Range(info.line, info.start, info.line, info.end), vscode.Uri.file(resolved)));
            }
            return links;
        },
    }));

    // ------------------------------------------------ diagnostics
    const trigger = () => config().get<string>("diagnostics.trigger", "onSave");
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument((d) => checker.schedule(d, 50)),
        vscode.workspace.onDidSaveTextDocument((d) => {
            if (d.languageId === LANGUAGE) project.invalidate();
            checker.schedule(d, 0);
        }),
        vscode.workspace.onDidChangeTextDocument((e) => {
            if (e.document.languageId === LANGUAGE && trigger() === "onType" && e.contentChanges.length > 0) checker.schedule(e.document, 600);
        }),
        vscode.workspace.onDidCloseTextDocument((d) => {
            if (d.languageId === LANGUAGE) checker.clear(d);
        }),
        vscode.workspace.onDidChangeConfiguration((e) => {
            if (!e.affectsConfiguration("clear")) return;
            project.invalidate();
            checker.resetWarning();
            for (const d of vscode.workspace.textDocuments) {
                if (config().get<boolean>("diagnostics.enable", true)) checker.schedule(d, 0);
                else checker.clear(d);
            }
        }),
    );

    const watcher = vscode.workspace.createFileSystemWatcher("**/{*.cl,clear.toml}");
    watcher.onDidChange((uri) => project.invalidate(uri.fsPath));
    watcher.onDidCreate(() => project.invalidate());
    watcher.onDidDelete(() => project.invalidate());
    context.subscriptions.push(watcher);

    for (const document of vscode.workspace.textDocuments) checker.schedule(document, 50);

    // ------------------------------------------------ commands
    const extraArgs = () => config().get<string[]>("run.arguments", []);

    context.subscriptions.push(
        vscode.commands.registerCommand("clear.run", async () => {
            const document = await activeClearFile();
            if (document) runInTerminal(["run", document.uri.fsPath, ...extraArgs()], path.dirname(document.uri.fsPath));
        }),
        vscode.commands.registerCommand("clear.build", async () => {
            const document = await activeClearFile();
            if (!document) return;
            const file = document.uri.fsPath;
            const output = path.join(path.dirname(file), path.basename(file, ".cl") + (process.platform === "win32" ? ".exe" : ""));
            runInTerminal(["build", file, "-o", output, ...extraArgs()], path.dirname(file));
        }),
        vscode.commands.registerCommand("clear.check", async () => {
            const document = vscode.window.activeTextEditor?.document;
            if (!document || document.languageId !== LANGUAGE) return;
            checker.resetWarning();
            checker.check(document);
            vscode.commands.executeCommand("workbench.actions.view.problems");
        }),
        vscode.commands.registerCommand("clear.runProject", async () => {
            const active = vscode.window.activeTextEditor?.document;
            const root = (active && project.projectRoot(active.uri.fsPath)) ?? workspaceRoots().find((r) => fs.existsSync(path.join(r, "clear.toml")));
            if (!root) {
                vscode.window.showInformationMessage("Clear: no clear.toml found. Create a project with 'Clear: New Project...'.");
                return;
            }
            await vscode.workspace.saveAll(false);
            runInTerminal(["run", root, ...extraArgs()], root);
        }),
        vscode.commands.registerCommand("clear.newProject", async () => {
            const picked = await vscode.window.showOpenDialog({ canSelectFolders: true, canSelectFiles: false, openLabel: "Create the project in this folder" });
            if (!picked?.[0]) return;
            const name = await vscode.window.showInputBox({ prompt: "Project name", validateInput: (v) => (/^[A-Za-z_][A-Za-z0-9_-]*$/.test(v) ? undefined : "Use letters, digits, _ and -") });
            if (!name) return;
            const directory = path.join(picked[0].fsPath, name);
            cp.execFile(compilerPath(), ["new", directory], (error, stdout, stderr) => {
                if (error) {
                    vscode.window.showErrorMessage(`Clear: clearc new failed: ${(stderr || stdout || error.message).trim()}`);
                    return;
                }
                vscode.commands.executeCommand("vscode.openFolder", vscode.Uri.file(directory), { forceNewWindow: (vscode.workspace.workspaceFolders?.length ?? 0) > 0 });
            });
        }),
        vscode.commands.registerCommand("clear.showIR", async () => {
            const document = await activeClearFile();
            if (!document) return;
            const file = document.uri.fsPath;
            const temp = fs.mkdtempSync(path.join(os.tmpdir(), "clear-ir-"));
            const exe = path.join(temp, path.basename(file, ".cl"));
            cp.execFile(compilerPath(), ["build", file, "-o", exe, "--emit-ir", ...extraArgs()], { cwd: path.dirname(file) }, async (error, stdout, stderr) => {
                const ir = [`${exe}.ll`, path.join(temp, `${path.basename(file, ".cl")}.ll`)].find(isFile);
                if (!ir) {
                    vscode.window.showErrorMessage(`Clear: no IR was written. ${(stderr || stdout || error?.message || "").trim().split("\n")[0]}`);
                    return;
                }
                const irDocument = await vscode.workspace.openTextDocument({ content: fs.readFileSync(ir, "utf8"), language: "llvm" });
                fs.rm(temp, { recursive: true, force: true }, () => undefined);
                vscode.window.showTextDocument(irDocument, vscode.ViewColumn.Beside);
            });
        }),
        vscode.tasks.registerTaskProvider(LANGUAGE, new TaskProvider()),
    );
}

export function deactivate() {
    terminal?.dispose();
}
