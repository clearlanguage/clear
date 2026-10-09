// Markdown to HTML: headings (with ids and a table of contents), paragraphs, lists,
// block quotes, fenced code, rules, and inline emphasis / code / links.

enum Block:
    Heading(level: int, id: String, text: String)
    Para(text: String)
    Items(ordered: bool, items: List[String])
    Code(lang: String, lines: List[String])
    Quote(text: String)
    Rule

class Stack[T]:
    items: List[T]

    function push(self, v: T):
        self.items.push(v)

    function pop(self) -> ?T:
        if len(self.items) == 0:
            return none
        return self.items.pop()

    function size(self) -> int64:
        return len(self.items)

function lines_of(doc: str) -> Generator[str]:
    let start: int64 = 0
    for i in 0..len(doc):
        if doc[i] == '\n':
            yield doc[start:i]
            start = i + 1
    if start < len(doc):
        yield doc[start:]

function find_from(s: str, pat: str, from: int64) -> int64:
    let i = from
    while i + len(pat) <= len(s):
        if s[i:i + len(pat)] == pat:
            return i
        i += 1
    return -1

function escape(s: str) -> String:
    let out = String()
    for c in s:
        switch c:
            case '&':
                out += "&amp;"
            case '<':
                out += "&lt;"
            case '>':
                out += "&gt;"
            case '"':
                out += "&quot;"
            default:
                out.push(c)
    return out

function inline(s: str) -> String:
    let out = String()
    let i: int64 = 0
    while i < len(s):
        let c = s[i]
        if c == '`':
            let j = find_from(s, "`", i + 1)
            if j >= 0:
                out += "<code>" + escape(s[i + 1:j]) + "</code>"
                i = j + 1
                continue
        else if c == '*' and i + 1 < len(s) and s[i + 1] == '*':
            let j = find_from(s, "**", i + 2)
            if j >= 0:
                out += "<strong>" + inline(s[i + 2:j]) + "</strong>"
                i = j + 2
                continue
            out += "**"
            i += 2
            continue
        else if c == '*':
            let j = find_from(s, "*", i + 1)
            if j >= 0:
                out += "<em>" + inline(s[i + 1:j]) + "</em>"
                i = j + 1
                continue
        else if c == '[':
            let close = find_from(s, "]", i + 1)
            if close >= 0 and close + 1 < len(s) and s[close + 1] == '(':
                let end = find_from(s, ")", close + 2)
                if end >= 0:
                    out += "<a href=\"" + escape(s[close + 2:end]) + "\">" + inline(s[i + 1:close]) + "</a>"
                    i = end + 1
                    continue
        out += escape(s[i:i + 1])
        i += 1
    return out

function slug(text: str, seen: *Map[String, int]) -> String:
    let s = String()
    for c in text:
        if (c >= 'a' and c <= 'z') or (c >= '0' and c <= '9'):
            s.push(c)
        else if c >= 'A' and c <= 'Z':
            s.push(c + 32)
        else if c == ' ' or c == '-':
            s.push('-')
    if s in seen:
        seen[s] += 1
        return s + "-" + from_int(seen[s])
    seen[s] = 0
    return s

function list_marker(line: str) -> (int, int64):
    // (0 = not an item, 1 = unordered, 2 = ordered), where the text starts
    if len(line) >= 2 and (line[:2] == "- " or line[:2] == "* "):
        return 1, 2
    let i: int64 = 0
    while i < len(line) and line[i] >= '0' and line[i] <= '9':
        i += 1
    if i > 0 and i + 1 < len(line) and line[i:i + 2] == ". ":
        return 2, i + 2
    return 0, 0

function parse(doc: str) -> List[Block]:
    let blocks = List[Block]()
    let seen = Map[String, int]()
    let para = String()
    let quote = String()
    let items = List[String]()
    let kind = 0
    let code = List[String]()
    let lang = String()
    let in_code = false
    for raw in lines_of(doc):
        let line = String(raw).strip()
        if in_code:
            if line.starts_with("```"):
                blocks.push(Block.Code(lang, code))
                code = List[String]()
                in_code = false
            else:
                code.push(String(raw))
            continue
        let mk, start = list_marker(line)
        let is_quote = line.starts_with("> ")
        // close whatever is open that this line does not continue
        if len(para) > 0 and (len(line) == 0 or mk != 0 or is_quote or line[0] == '#' or line.starts_with("```") or line == "---"):
            blocks.push(Block.Para(para))
            para = String()
        if kind != 0 and mk != kind:
            blocks.push(Block.Items(kind == 2, items))
            items = List[String]()
            kind = 0
        if len(quote) > 0 and not is_quote:
            blocks.push(Block.Quote(quote))
            quote = String()
        if len(line) == 0:
            continue
        if line.starts_with("```"):
            lang = String(line[3:]).strip()
            in_code = true
        else if line == "---":
            blocks.push(Block.Rule)
        else if line[0] == '#':
            let level = 0
            while level < len(line) and line[level] == '#':
                level += 1
            let text = String(line[level:]).strip()
            blocks.push(Block.Heading(level as int, slug(text, &seen), text))
        else if mk != 0:
            kind = mk
            items.push(String(line[start:]))
        else if is_quote:
            if len(quote) > 0:
                quote += " "
            quote += line[2:]
        else:
            if len(para) > 0:
                para += " "
            para += line
    if len(para) > 0:
        blocks.push(Block.Para(para))
    if kind != 0:
        blocks.push(Block.Items(kind == 2, items))
    if len(quote) > 0:
        blocks.push(Block.Quote(quote))
    return blocks

function render(b: *Block) -> String:
    switch *b:
        case Heading(level, id, text):
            let tag = "h" + from_int(level)
            return "<" + tag + " id=\"" + id + "\">" + inline(text) + "</" + tag + ">"
        case Para(text):
            return "<p>" + inline(text) + "</p>"
        case Items(ordered, items):
            let tag: str = when ordered use "ol" otherwise "ul"
            let out = "<" + tag + ">\n"
            for it in items:
                out += "  <li>" + inline(it) + "</li>\n"
            out += "</" + tag + ">"
            return out
        case Code(lang, lines):
            let out = String("<pre><code")
            if len(lang) > 0:
                out += " class=\"language-" + lang + "\""
            out += ">"
            for i in 0..len(lines):
                if i > 0:
                    out += "\n"
                out += escape(lines[i])
            out += "</code></pre>"
            return out
        case Quote(text):
            return "<blockquote>" + inline(text) + "</blockquote>"
        case Rule:
            return "<hr>"

function join[T](xs: []T, sep: str, f: function(T) -> String) -> String:
    let out = String()
    for i in 0..len(xs):
        if i > 0:
            out += sep
        out += f(xs[i])
    return out

function words(s: str) -> Generator[String]:
    let cur = String()
    for c in s:
        if (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z'):
            cur.push(c)
        else if len(cur) > 0:
            yield cur
            cur = String()
    if len(cur) > 0:
        yield cur

function main() -> int32:
    let doc = String("# Clear *Markdown* demo\n\nThis is **bold**, *em* and `a < b && c`.\nSecond line of the same paragraph with a [link](http://x.org/?a=1&b=2).\n\n## Lists\n\n- first item\n- second with **strong *nested* text**\n* third\n1. one\n2. two\n10. ten\n\n## Code\n\n```clear\nfunction main() -> int32:\n    print(\"<hi>\")\n```\n\n> quoted *text*\n> continues here\n\n---\n\n## Lists\n\nUnclosed *star and ** pair and [bad](link and `tick.\n### Deep heading!\nTrailing paragraph")
    let blocks = parse(doc)
    let html = List[String]()
    for b in blocks:
        html.push(render(&b))
    print(join(html, "\n", lambda s: s))

    // table of contents from the headings, then a few List operations on it
    let toc = List[String]()
    let levels = List[int]()
    for b in blocks:
        switch b:
            case Heading(level, id, text):
                toc.push(id)
                levels.push(level)
            default:
                pass
    toc.insert(0, "top")
    levels.insert(0, 0)
    let deep = toc.index_of("deep-heading")
    if deep:
        toc.remove(deep)
        levels.remove(deep)
    print("toc:", toc, levels)
    print("lists at", toc.index_of("lists") ?? -1, "missing at", toc.index_of("nope") ?? -1)

    let counts = Map[str, int]()
    for b in blocks:
        let name: str = "rule"
        switch b:
            case Heading(level, id, text):
                name = "heading"
            case Para(text):
                name = "para"
            case Items(ordered, items):
                name = when ordered use "ol" otherwise "ul"
            case Code(lang, lines):
                name = "code"
            case Quote(text):
                name = "quote"
            case Rule:
                name = "rule"
        if name in counts:
            counts[name] += 1
        else:
            counts[name] = 1
    let names = List[str]()
    for k in counts:
        names.push(k)
    names.sort()
    let st = Stack[String]()
    for n in names:
        st.push(n + "=" + from_int(counts[n]))
    let parts = List[String]()
    while top := st.pop():
        parts.insert(0, top)
    print("blocks:", join(parts, ", ", lambda s: s), st.size())

    // word frequency over the paragraph text
    let freq = Map[String, int]()
    for b in blocks:
        switch b:
            case Para(text):
                for w in words(text):
                    let key = w.lower()
                    if key not in freq:
                        freq[key] = 0
                    freq[key] += 1
            default:
                pass
    let top = List[String]()
    for k in freq:
        top.push(k)
    top.sort_by(lambda k: -freq[k] * 1000 + (k[0] as int))
    while len(top) > 5:
        top.remove(len(top) - 1)
    let shown = top.map(lambda k: k + "=" + from_int(freq[k]))
    print("top words:", shown)
    return 0

// expect:
// <h1 id="clear-markdown-demo">Clear <em>Markdown</em> demo</h1>
// <p>This is <strong>bold</strong>, <em>em</em> and <code>a &lt; b &amp;&amp; c</code>. Second line of the same paragraph with a <a href="http://x.org/?a=1&amp;b=2">link</a>.</p>
// <h2 id="lists">Lists</h2>
// <ul>
//   <li>first item</li>
//   <li>second with <strong>strong <em>nested</em> text</strong></li>
//   <li>third</li>
// </ul>
// <ol>
//   <li>one</li>
//   <li>two</li>
//   <li>ten</li>
// </ol>
// <h2 id="code">Code</h2>
// <pre><code class="language-clear">function main() -&gt; int32:
//     print(&quot;&lt;hi&gt;&quot;)</code></pre>
// <blockquote>quoted <em>text</em> continues here</blockquote>
// <hr>
// <h2 id="lists-1">Lists</h2>
// <p>Unclosed <em>star and </em>* pair and [bad](link and `tick.</p>
// <h3 id="deep-heading">Deep heading!</h3>
// <p>Trailing paragraph</p>
// toc: [top, clear-markdown-demo, lists, code, lists-1] [0, 1, 2, 2, 2]
// lists at 2 missing at -1
// blocks: code=1, heading=5, ol=1, para=3, quote=1, rule=1, ul=1 0
// top words: [and=4, a=3, b=2, link=2, paragraph=2]
