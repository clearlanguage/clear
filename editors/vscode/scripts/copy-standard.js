// Bundles ../../Standard into the extension, so completion and hover know the standard library
// even when the compiler's sources are not open.
const fs = require("fs");
const path = require("path");

const from = path.resolve(__dirname, "..", "..", "..", "Standard");
const to = path.resolve(__dirname, "..", "standard");

if (!fs.existsSync(from)) {
    console.log(`no standard library at ${from}, keeping the bundled copy`);
    process.exit(0);
}

fs.rmSync(to, { recursive: true, force: true });
fs.mkdirSync(to, { recursive: true });

for (const name of fs.readdirSync(from)) {
    if (name.endsWith(".cl"))
        fs.copyFileSync(path.join(from, name), path.join(to, name));
}

// the license travels with the packaged extension
const license = path.resolve(__dirname, "..", "..", "..", "LICENSE");
if (fs.existsSync(license)) fs.copyFileSync(license, path.resolve(__dirname, "..", "LICENSE"));
