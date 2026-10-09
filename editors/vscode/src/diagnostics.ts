// Reads what `clearc check` prints:
//
//   error[E048]: Cannot convert implicitly, the conversion may lose information.
//     --> /path/file.cl:2:18
//     |
//   2 |     let x: int = "hi"
//     |                  ^^
//     = help: Converting 'str' to 'int32' needs an explicit cast.

export type Severity = "error" | "warning" | "note";

export interface CompilerDiagnostic {
    severity: Severity;
    code: string;
    message: string;
    help?: string;
    file?: string;
    /** zero based */
    line: number;
    character: number;
    length: number;
}

const HEADER = /^(error|warning|note)\[(E\d+)\]:\s*(.*)$/;
const LOCATION = /^\s*-->\s*(.*):(\d+):(\d+)\s*$/;
const CARETS = /^\s*\|\s?(\s*)(\^+)\s*$/;
const HELP = /^\s*=\s*help:\s*(.*)$/;

export function parseDiagnostics(output: string): CompilerDiagnostic[] {
    const result: CompilerDiagnostic[] = [];
    let current: CompilerDiagnostic | undefined;

    for (const line of output.split(/\r?\n/)) {
        const header = HEADER.exec(line);
        if (header) {
            current = { severity: header[1] as Severity, code: header[2], message: header[3].trim(), line: 0, character: 0, length: 1 };
            result.push(current);
            continue;
        }

        if (!current) continue;

        const location = LOCATION.exec(line);
        if (location) {
            current.file = location[1];
            current.line = Math.max(0, Number(location[2]) - 1);
            current.character = Math.max(0, Number(location[3]) - 1);
            continue;
        }

        const carets = CARETS.exec(line);
        if (carets && current.file) {
            current.length = carets[2].length;
            continue;
        }

        const help = HELP.exec(line);
        if (help) {
            current.help = help[1].trim();
            continue;
        }
    }

    return result;
}

/** A clearc crash or a message without the usual layout ("clearc: ...", "panic: ..."). */
export function parseLooseError(output: string): string | undefined {
    const line = output.split(/\r?\n/).find((l) => /^(clearc:|internal compiler error|panic:)/i.test(l.trim()));
    return line?.trim();
}
