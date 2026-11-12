# Ψ (Psi) — design notes

Working document for the new loader module under `impl/`. The format is **Ψ**, written *Psi* where
a name has to be ASCII: just the letter, no acronym — the soul of an application, its
configuration. Until session 10 it was DEF, *Declarative Extensible Format* (§14).

| Where | Name |
|---|---|
| Format | Ψ, Psi |
| Module directory | `impl/psi` |
| sbt project | `psi` |
| Artifact | `cfg-psi` |
| Package | `h8io.cfg.impl.psi` |
| Loader object | `Psi` |
| File extensions | `.Ψ`, `.ψ` and `.psi`, searched in that order — one extension where file names ignore case |

**Status:** under discussion, expected to span several sessions. §2 lists what is settled, §13 what
is still open, and §14 logs how each decision was reached so a later session does not reopen a
question that was already argued through. Add to §14 rather than silently editing §2.

The implementation is tracked step by step in [`plan.md`](plan.md).

## 1. Why a format of our own

The `cfg` protocol says an `IScalar` carries the raw string that was written in the source, that a
`tag` is present only when the author wrote one, and that every type conversion is the decoder's
business. No third-party format we wrap actually honours that:

- `impl/hocon` — typesafe-config parses values into Java types *before* we see them, and scalars are
  recovered with `unwrapped.toString`. This is already documented as a defect in `HOCON.scala`:
  `1e5` comes back as `"100000.0"`, so `IScalar.value` is not what the file said. Tags are always
  `None` because HOCON has no concept of them. `Location` is whatever `ConfigOrigin.description`
  returns.
- `impl/yaml` (branch `yaml-impl`) — closer, but it takes a `Composer` subclass reading
  `parser.peekEvent` to recover *explicit* tags, because snakeyaml-engine exposes no
  `Node.isResolved`.

Writing the parser removes the impedance mismatch by construction: the scalar is the exact substring
of the source, the tag is present iff written, and `Location` can carry source, line and column
instead of a foreign description string. It also removes the last external dependency from the
loader path.

## 2. Decisions already taken

| Decision | Value |
|---|---|
| Syntax shape | Nested blocks with braces (HOCON/JSON-like), not indentation |
| Explicit tags | Yes, on scalars **and** containers |
| Substitutions | Yes, `${…}`, HOCON-style lookup order |
| Includes | Yes, but as a **directive**, not a keyword |
| Include placement | Anywhere in the tree, not just at file top level |
| Include sources | Relative paths, classpath resources; optional variant that tolerates a missing file |
| Comments | `//` and nesting `/* */`; `/** */` reserved for doc comments (§3) |
| Multi-line strings | Triple quotes, with the escapes of `"…"` and `${…}`; always a block; indent markers, Java's rule by default (§5) |
| Raw strings | `'…'` and `'''…'''`, as Perl's single quotes: no substitutions, `\'` and `\\` the only escapes (§5) |
| Special words | Avoided — `#`-prefixed directives instead, as an open, extensible set. `null` is the one exception |
| Error model | `Either`, no Cats; as many errors per run as can be reported honestly (§11) |
| Dotted keys | Sugar for nesting; a quoted key is indivisible, dots included |
| Substitution location | Reference site, carrying the definition site with it (§10) |
| Location coordinates | Source as it was named — the URL passed to the loader, or an include argument as written; line and column 1-based, column in code points (§10) |
| `impl/hocon` | Stays, alongside this module |
| Overriding | `:` merges maps, `:=` replaces whatever was there (§7); no append operator |

## 3. Lexical structure

- Encoding is UTF-8. A byte order mark at the start of a source is skipped. A byte sequence that is
  not UTF-8 ends the source as unreadable, pointing at the first bad byte; nothing is replaced. A
  newline is LF or CRLF; a lone CR is an error.
- **Whitespace** is space and tab, nothing else — YAML's set, which Scala 3 and Python widen only
  with ASCII control characters such as form feed. Every other whitespace character outside quotes
  and comments is an error: NBSP and the other Unicode spaces. Each is reported where it stands and then read as whitespace, as a lone CR is. Inside quotes and
  comments such characters are content. FF and VT are control characters, errors anywhere (below).
- **NEL, U+2028 and U+2029 are errors anywhere**, quotes and comments included, as a lone CR is:
  some tools show them as a line break and others do not. In `"…"` and backticks they are written
  `\u0085`, `\u2028`, `\u2029`, and in `"""…"""` as well (§5).
- **Control characters other than tab are errors anywhere**, quotes and comments included: U+0000
  to U+001F but tab, LF and the CR of a CRLF, then DEL and U+0080 to U+009F, FF and VT among them;
  NEL and a lone CR keep the errors above. No reader sees them, so a value would differ from what
  is on the screen. Each is reported where it stands and then read as whitespace outside quotes and
  dropped inside quotes and comments, as a lone CR is. In `"…"`, backticks and `"""…"""` one is
  written as an escape, `\u0001`, or `\b` and `\f` for the two that have one; a raw string cannot
  hold one (§5). A tab is content inside quotes, whitespace between tokens, and an error in
  indentation (below).
- **Bidirectional formatting and invisible characters are errors anywhere**, as control characters
  are: the embeddings, overrides and isolates U+202A–U+202E and U+2066–U+2069, which make source
  display in another order than it parses ("Trojan Source"); the zero-width space U+200B; the word
  joiner and the invisible operators U+2060–U+2064; and U+FEFF past the start of a source. Handled
  as control characters are, and written the same way: `\u202E` in `"…"`, backticks and
  `"""…"""`, never in a raw string. Left alone, being needed in text: the joiners U+200C and
  U+200D (Persian, the Indic scripts, emoji sequences), the marks of direction U+200E and U+200F
  (right-to-left text), and the soft hyphen U+00AD.
- **A tab in indentation is an error**, already in v1, as in YAML. Indentation is the whitespace
  before the first token of a line, comments included, so `/* c */<TAB>x` has a tab in it; a blank
  or comment-only line has none. A line is what newlines delimit, and a newline inside `/* … */` is
  none (below), so in `<TAB>/* c⏎*/ x` the tab is in the indentation of `x`, while in `a /* c⏎*/<TAB>x`
  `x` is not the first token of its line and the tab is between tokens. Between tokens a tab is
  whitespace like a space. Inside `"""…"""` leading whitespace is content (§5), tabs included.
- **Comments** are `//` to the end of the line and `/* … */`, which nest, as in Scala: commenting out
  a block that already holds a comment is safe. An unterminated `/*` is reported at its opening, a
  stray `*/` is an error, and `/**/` is an empty ordinary comment. Inside an unquoted scalar `//`
  and `/*` are ordinary characters; anywhere else they open a comment. So `url: http://host` is one
  scalar — and so is `a: 1//x`, the price of the rule — while `pattern: /*.log` opens a comment, and
  a value starting with `//` or `/*` is written in quotes. `#` has no meaning of its own here.
  A comment counts as whitespace, so it never separates: a newline inside `/* … */` is not a
  newline, and `a: 1 /*⏎*/ b: 2` needs a `,`. A `//` comment ends before its newline, which stays.
- **Doc comments** — `/** … */` is reserved for documentation that DSLs built on Psi may one day
  want in the source. v1 ignores it, but accepts it only before a field, a sequence element or a
  directive, on its line or a line above it; anywhere else (between a tag and its value, after a
  value on the same line, before another doc comment, at the end of a block) it is an error, so of
  two doc comments in a row the first is the error. Doc comments carry no meaning for values, so
  giving them one later breaks no v1 file. That
  will take a field such as `doc: Option[String]` on `ISome` — a change to `cfg`, for a higher level
  to read (generated reference docs, error messages) — so the lexer emits a doc comment as a token
  and the Ast keeps it on the node it precedes from the start.
- **Bare keys** match `Id.SafeKeyPattern` exactly — `^[\p{L}_][\p{L}\p{N}_-]*$`. Anything else is
  written as a quoted key. A key that would pass bare may be quoted too: `` `port` `` and `port` are
  the same key.
- **Names in backticks, values in double quotes.** A quoted key — in a field, a dotted key or a path
  inside `${…}` — is written `` `…` ``; `"…"` and `'…'` are only ever scalars. The role of a token
  is therefore visible to the lexer without context, and `"port": 1` is an error with a hint, not a
  second spelling. Both forms share one escape set, and so does `"""…"""` (§5): `\"`, ``\` ``,
  `\\`, `\b`, `\f`, `\n`, `\r`, `\t` and `\uXXXX` — the set `Id.quote` produces today, plus ``\` ``
  and `\$`. Both quotes are
  escapable in both forms, so there is one escape grammar rather than one per delimiter; `Id.quote`
  itself only needs ``\` `` and `\\`. Any other `\` is an error. Neither form may hold a raw
  newline — that is what `\n` and `"""…"""` are for — so an unterminated quote is reported on its own
  line rather than at the end of the file.
- **Dotted keys** — `a.b.c: v` is sugar for `a { b { c: v } }`. The split happens only on dots
  *between* key tokens: a quoted key is indivisible, so `` `io.h8`: v `` is one key named `io.h8`,
  and ``x.`a.b`.c`` is three levels. This is also what `Id.path` produces — a key containing a dot
  fails `SafeKeyPattern` and is rendered quoted — so the round-trip property holds.

  The two rules together buy a property worth keeping: the output of `Id.path` is itself valid
  source. ``server.`odd key`[0]`` renders from a node and parses back to the same address. This
  takes `Id.quote` in `cfg` from `"…"` to backticks, which changes every path the HOCON and YAML
  loaders print as well. Any later change to `Id.quote` has to be mirrored here, and vice versa.
- **Unquoted scalars** contain no whitespace. One runs to the first whitespace, `,`, `}` or `]`;
  after it only a separator, a comment or the end of the block may follow, so `a: foo bar` is an
  error that says to quote the value. A `"` or `` ` `` inside one is an error with the same hint:
  `abc"def"` would otherwise silently keep its quotes, as YAML's `'x'` would. What was read becomes `IScalar.value` verbatim — `1e5` stays
  `"1e5"`, `true` stays `"true"`, `007` stays `"007"`. There is no type inference anywhere in the
  parser. A value with spaces is written quoted: `greeting: "Hello, world"`.

  A value starting with `'` is a raw string (§5), so YAML's `a: 'x'` means what it means in YAML,
  the quotes gone. Inside an unquoted scalar `'` is an ordinary character: `it's`. `~` gets no
  rule — it is the string `~`, and `null` is written out.

  `${` opens a substitution (§9) in every kind of scalar: unquoted (`jdbc:${host}/db`), in
  `"…"` (`"jdbc:${host}/db"`) and in `"""…"""`. A literal `${` is written `\${`, which is what
  the `\$` escape is for: `"\${not.a.substitution}"`. Inside backticks there are no
  substitutions — a name is never computed — but `\$` is accepted there too, the escape set being
  one. Inside `"""…"""` a literal `${` is `\${` as well, the escape set being the same (§5).
- **`:` after a key** must be followed by whitespace or the end of the line, so `a:b` in a block is an
  error with a hint rather than a field. Inside a value `:` is ordinary: `url: http://host:8080`.
  `:=` is the replacing form of `:` (§7) and follows the same rule: `a:=b` is an error.
- **Separators** are `,` and newline, nothing else — whitespace never separates, so `[foo bar]` and
  `a: 1 b: 2` are errors, not two elements or two fields: in a block the second word of
  `a: foo bar` is a value to quote unless a field starts there, as `b:` does, which needs a
  separator; in a sequence a second word needs a separator, `[foo bar]` most likely being two
  elements. After any value, scalar or container,
  only a separator, a comment or the closing bracket may follow: `a { x: 1 } b: 2` needs a `,`.
  A run of newlines with at most one `,` among them is one separator, wherever the `,` stands:
  `a: 1,⏎b: 2` and `a: 1⏎, b: 2` alike, blank lines separating nothing extra. An empty element
  (`[a,,b]`, `[, a]`) is two `,` with no value between them, and an error — there is no empty
  value, `null` is written out. Newlines after an opening bracket or before a closing one are no
  element either: `[⏎a⏎]` is `[a]`, as `{⏎a: 1⏎}` is `{a: 1}`. A trailing separator is allowed in
  sequences and blocks alike.

  The rule also frees `- ` (dash, space) at the start of an indented line for the sequence element
  marker (§13): `- 5` can never be a scalar, `-5` and `"- 5"` always are.
- **`null`** — the one reserved word. An unquoted scalar whose trimmed text is exactly `null`
  becomes `Node.INull`; `"null"` in quotes stays a scalar. Tags are kept: `@secret null` is an
  `INull` with `tags = Set("secret")`. `true`, `false` and every other word remain plain scalars.
  `null` is reserved only in value position — as a key it is an ordinary bare key.

## 4. Grammar sketch

```
document   = block-body
block      = "{" block-body "}"
block-body = { field | directive } 
field      = key { "." key } ( ( ":" | ":=" ) value | block )
value      = [ tag ] ( scalar | block | seq | directive | substitution-expr )
seq        = "[" [ value { sep value } [ sep ] ] "]"
sep        = "," | newline
tag        = "@" name
directive  = "#" name [ "(" [ arg { sep arg } [ sep ] ] ")" ]
arg        = [ name ":" ] literal
scalar     = null | bare-scalar | quoted-scalar | triple-quoted
```

Fields are separated by a newline or a `,` (§3). `key { … }` needs no `:`, and the `{` must be on the
same line as the key: `key` followed by `{` on the next line is an error. That keeps a key at the end
of a line free for the indentation syntax (§13). For the same reason a value starts on the line of
its `:` or `:=`, and the value a tag precedes on the line of the tag: `a:⏎5` and `a: @t⏎{ … }` are
errors in v1, whatever the indentation of the next line. Lifting this later breaks no v1 file
(§14, session 12).

A dotted key (`server.tls.enabled: true`) is sugar for nested blocks. Its keys and dots touch, with
no whitespace or comment between them, as `Id.path` writes them: `a . b` and `a. b` are errors. The
block form `key { … }` means `key: { … }`, so it merges; replacing a block is written
`key := { … }`. It has no place for a tag: a tagged block is written `key: @t { … }`, and
`key @t { … }` is an error that says so.

Every syntax error names the fix where there is one, as these do: the error for `key @t { … }`
shows `key: @t { … }`, the one for `a:⏎5` says to move the value up.

## 5. Values

**Sequences** — `[a, b, c]`; a newline also separates, and a trailing separator is allowed.

**Triple-quoted scalars** — `"""…"""`, with the escape set of `"…"` (§3) and `${…}`, which is
substituted here as in any scalar (§3, §9). As in Java's text blocks, a triple-quoted string is not
raw: what it saves over `"…"` is escaping quotes and writing newlines, not escaping backslashes. A
backslash is `\\`; any other `\` outside the set is an error,
as in `"…"`, so a forgotten one is reported rather than kept. `\"""` still writes a literal `"""`
— `\"` and then two quotes, which do not close — and `\${` a literal `${`. A lone CR, NEL, U+2028
and U+2029 (§3) are written `\r`, `\u0085`, `\u2028`, `\u2029`, and a control character (§3) as
`\uXXXX`, `\b` or `\f`.

Escapes are interpreted **after the layout rules** below — the indent, trailing whitespace, folding
— again as in Java: what an escape produces is kept as written. So `\t` or `\u0020` at the end of a
line survives the stripping of trailing whitespace, and `\n` survives folding.

**Raw strings** — `'…'` and `'''…'''`, as Perl's single quotes: no substitutions, `${` being text,
and two escapes, `\'` for a quote and `\\` for a backslash. Any other `\` is an ordinary character,
so `'^\d+"$'` is a regex as written, and `'''…'''` holds a shell script or a Helm template, `${`
and all, untouched. `'it\'s'` is `it's`, and a value ending in `\` ends in `\\`: `'C:\dir\\'`.
As in Perl, a doubled backslash collapses — `'\\host\share'` is `\host\share` — so a UNC path is
written `'\\\\host\share'`.

`'…'` is one line, like `"…"`. `'''…'''` is a block and takes the layout rules below exactly as
`"""…"""` does, a literal `'''` being `\'''`; everything said below of `"""` holds of `'''` alike.
A lone CR, NEL, U+2028, U+2029, the control characters other than tab and the invisible characters
(§3) cannot be written in a raw string, being errors anywhere (§3); a value holding one is written in `"…"` or `"""…"""`.

**A triple-quoted string is always a block**: its content starts on the line after the opening
delimiter, and what stands between the delimiter and the end of its line are its markers (below),
or nothing. There is no one-line form — a short value with quotes in it is written in `"…"` with
`\"`, or in `'…'`: `pattern: '^\d+"$'`. Text after the delimiter that is not markers, `"""abc"""`
included, is an error, and so is whitespace there; neither is fatal, the block being read on
(§11).

A run of more than three quotes closes the string at its last three, so a value may end in `"`:
a last line `say "hi""""` gives `say "hi"`. After the closing delimiter comes what follows any
value — a separator, a comment, the end of the block; a substitution belongs inside the quotes (§9).

The newline immediately after the opening delimiter is dropped, and so is the one
immediately before the closing delimiter, together with the whitespace between it and the
delimiter. These newline rules hold whatever the indent rule is.

A final newline is therefore written as one, by an empty line before the closing delimiter:

```
key: """
  abc
  def

  """
// value: "abc\ndef\n"
```

The layout of a block is picked by **markers** between the opening delimiter and the end of its
line. A marker is a name — a run of the symbols `! # $ % & * + - . : ; < = > ? @ ^ | ~`, or a name
by the key rule (§3) — followed, if it takes any, by its arguments in parentheses, written as a
directive's are (§6): `name: literal` or a literal alone, the name bare or in backticks, separated
by `,` with a trailing one allowed and an empty one an error, a literal being a
string in `"…"` (with its escapes, `${` an error) or `'…'`, an integer, or a bare name. As in
Scala, positional arguments go to the parameters in order and come before the named ones:
`width(2)` and `width(n: 2)` are one. Markers are separated by whitespace; none may stand before
the first or after the last.
`/` is not a symbol, being kept for a comment after the markers, which v1 does not allow.

| Marker | Effect |
|---|---|
| none | the common indent is stripped, Java's rule (below) |
| `<` | all of the indent is stripped |
| `>` | the indent is kept as written |
| `width(n: N)` | the opening line's indent and N more are stripped (below) |
| `+` | lines are folded (below) |

**Markers are applied left to right**, each to the lines the one before it left, once trailing
whitespace is stripped and the closing delimiter's line set aside: `"""< +` strips the indent and
then folds, while `"""+ <` folds lines that still carry their indent — which leaves them as they
are — and then strips it. `<`, `>` and `width` set the indent, and a block takes at most one of
them; with none, Java's rule comes first, so `"""+` folds what that rule left.

**The set of markers is open**, as the set of directives is (§6): the loader is built with the
marker handlers it knows, and a handler maps the lines of a block to lines (§14, session 10). **The
four above — `<`, `>`, `width` and `+` — are reserved**: every loader has them, and none can
replace or remove them, so `"""<` means the same whatever loader reads the file. A loader's own
markers are added to them, and registering one of the four names is an error when the loader is
built, as is registering any name twice. The layout rules depend on it as well: a block takes one
marker that sets the indent, and Java's rule comes first when none does, which only holds if the
markers that set it are known. `#include` is reserved the same way, except that a loader can
disable it (§6). Each of these is reported, none of them fatal, and the block is
then read as with no marker: a marker the loader does not know; a named argument it does not take,
a positional one past its last parameter, a positional one after a named one, an argument given
twice — by name, or by position and then by name; a value it cannot take, an argument it requires
and was not given; and a second marker that sets the indent. Markers that cannot be read at all — `"""2`, `"""<x`, `"""abc"""` —
are one error, where they stop fitting, and the rest of the line is not looked at. Whitespace after
the delimiter or after the last marker is an error as well, but the markers are applied.

`<` and `>` are a pair: the text pushed to the left edge, or left where it stands.

**Folding** under `+`, as YAML does under `>`: a newline between two lines with no indent becomes a
space, an empty line becomes a newline,
and a line indented deeper is not folded — neither it nor the newlines around it — so a list or a
code sample inside prose survives.

**An explicit width** is `width(n: N)`, N a positive integer. As in YAML it counts from the indent
of the line holding the opening delimiter: that indent and then N whitespace characters are
stripped from every non-blank line, and a line that does not start with them is an error. The closing delimiter's column does not count then. Counting from that line
keeps the value when a formatter shifts the whole block:

```
server {
  banner: """width(n: 2)
      abc
    """
}
// value: "  abc" — 2 + 2 stripped; nested one level deeper, 4 + 2, the same value
```

**Every newline in a block is LF** in the value, whether the file has LF or CRLF, so the value does
not depend on the editor or on git's `core.autocrlf`. A substituted value keeps its own newlines.

**Trailing whitespace** is stripped from every line of a block, under any marker and before folding,
so a line holding only whitespace becomes empty. It is stripped from the source text, before
substitution: a substituted value keeps its own. A line that has to end in whitespace ends in an
escape: `\t`, `\u0020`.

Indents are compared character by character, so a tab never matches a space. Under no marker, two
indents of which neither is a prefix of the other (a tab where the other has a space) are an error,
rather than a common indent silently shorter than intended.

With no marker, the common indent is the longest leading whitespace shared by the non-blank lines
and by the line of the closing delimiter, when that delimiter stands on a line of its own:

```
server {
  banner: """
    Welcome.
      Indented line.
    """
}
// value: "Welcome.\n  Indented line."
```

Moving the closing delimiter left keeps an indent in the value:

```
key: """
    abc
    def
  """
// value: "  abc\n  def"
```

**Tags** — `@name` before any value, including containers. They are to a value what annotations
are to a Scala or Java class: metadata the parser carries and something else — a decoder, a later
schema — reads.

```
timeout: @duration 5s
routes: @ordered { a: 1, b: 2 }
```

A tag is a name, not a call: `@name(` is an error, which keeps arguments to tags available for later
without breaking a v1 file. A value starting with `@` is written in quotes (`user: "@john"`); inside
an unquoted scalar `@` is ordinary (`admin@host`).

A tag name follows the key rule: bare if it matches `Id.SafeKeyPattern`, in backticks otherwise.
`ISome.tags` holds names only — the prefix and the quotes are syntax of the format, not part of
the tag: `@duration` gives `duration`, ``@`tag:yaml.org,2002:int` `` gives `tag:yaml.org,2002:int`,
so there is no need for `!!`. A tag may precede any value, a multi-line string included:
`query: @sql """…"""`.

`impl/yaml` follows the same rule and drops the leading `!` of a tag (`!postgres` gives `postgres`, `!!int` still `tag:yaml.org,2002:int`) — a breaking change to
`cfg-yaml`, made in step 2a of the plan. It merges YAML's `!foo` and `!<foo>`, two different tags in
YAML, into one name; the protocol accepts that.

Only tags written in the source reach `ISome.tags`; nothing is ever inferred, and there is no
non-specific tag that resolves to nothing. This matches the rule already settled for `impl/yaml`.

**A value takes any number of tags, and they form a set** — as an element takes CSS classes, which
is the role tags are meant to play for a later Psi schema. They are written stacked, like
annotations: `@pooled @readonly { … }`. Order carries no meaning, `@a @b` is the same as `@b @a`,
and a tag written twice counts once: `@a @a x` is `@a x`, not an error.

**Tags mean nothing at this level.** Psi and `cfg` are the low-level protocol; what a tag means,
and how particular tags combine when values merge, is for the level above — whatever consumes the
tree, and the planned Psi schema — to define. So the rules below are deliberately the neutral ones: a set,
union on merge and graft, nothing dropped except by `:=`. They lose no tag a higher level might
need, and they pre-empt none of its choices — that level can still give two tags a conflict, an
order or a "later wins" of its own.

In the protocol `ISome.tag: Option[String]` becomes `ISome.tags: Set[String]`, empty when nothing
was written — a change to `cfg` and every loader, made in step 2a of the plan together with the `!`
above, so the API breaks once. `impl/hocon` always gives the empty set, `impl/yaml` at most one
element, YAML allowing one tag per node.

**`@-name` is reserved** for removing an inherited tag (§7) and is an error in v1. It costs nothing:
a bare tag name follows `Id.SafeKeyPattern`, which cannot start with `-`, so `@-` never begins a
legal tag. A tag whose name starts with `-` is written in backticks, ``@`-name` ``, and is an
ordinary tag.

## 6. Directives

The extension point, and the reason `include` is not a keyword. A directive is

```
#name(arg: literal, arg: literal)
```

The `#` is the C preprocessor's, and for the same reason: directives are expanded while parsing,
before merging and substitution (§8). A value starting with `#` is written in quotes
(`color: "#fff"` — unquoted it is a call of an unknown directive `fff`); inside an unquoted scalar
`#` is ordinary. A pasted YAML or HOCON comment fails loudly: `# text` is a directive with no name.

A directive may appear in two positions:

- **statement position**, inside any block or at the top of a file — it must produce a map, which is
  merged into the enclosing block at the point where it appears, so fields below it override it;
- **value position**, on the right of `:` or as a sequence element — it produces a single node.

A directive name follows the key rule, as a tag name does (§5): bare if it matches
`Id.SafeKeyPattern`, in backticks otherwise — ``#`my.directive`(…)``. One rule covers every name
in the language: keys, path segments, tags and directives.

**`#name` alone is a call with no arguments**, the same as `#name()`, as a marker with none is
written without parentheses (§5). When there are parentheses, the `(` touches the name: `#name (…)`
and `#name` with `(` on the next line are errors that say to move it.

Arguments are separated like everything else, by `,` or a newline, so a long directive can be split
across lines. The rules are a block's (§3): a run of newlines with at most one `,` is one
separator, a trailing one is allowed, an empty argument is an error, and newlines after `(` or
before `)` are no argument.

**A named argument is written as a field is, `name: literal`**, and follows the rules of `:` (§3):
whitespace or the end of the line after it, so `#x(a:1)` is an error with a hint, as `a:1` is in a
block. The handler gets its arguments as a map (below), and they are written as one: Psi has one
way to pair a name with a value. `=` is no part of the syntax; `#x(a = 1)` is an error that says
to write `:`. An argument may also be positional — a literal alone, going to the handler's
parameters in order — and positional arguments come before the named ones, as in Scala; markers
take arguments the same way (§5). An argument's value is a literal in v1; a block or a sequence
there could be allowed later, the arguments being a map anyway, but never a substitution (below).

A parameter name follows the rule of every other name: bare if it matches `Id.SafeKeyPattern`, in
backticks otherwise, ``#x(`odd name`: 1)``. An argument given twice by name, or a positional one
after a named one, is an error the parser reports; one given by position and again by name is
reported where the handler's parameters are known (plan step 10), as for markers (§5).

Argument literals are strings in `"…"` or `'…'` — not `"""…"""` or `'''…'''` in v1, an argument
being a short literal — bare identifiers, `true`/`false`, which are bare identifiers like any
other, and integers: an optional `-` and decimal digits with no leading zeros, as a marker's (§5).
Every one reaches the handler as a string (below), which these rules keep to one spelling per
number. Arguments are
**literal only — no substitutions**. This is not a simplification, it is forced by the phase order in
§8: directives are expanded during parsing, while `${…}` is resolved after every source has been
merged, so a directive argument written as `${base}"/x"` would have to be resolved before the value
it refers to exists.

Directives are an open set. The loader is built with a set of handlers; the format itself only fixes
the call syntax. An unknown directive is an error, never a silently ignored field — otherwise a typo
in a directive name becomes a config silently missing half its content.

**A name means one thing, or nothing** (session 13). A loader's directives are added to the
built-in `#include`, and registering a name twice is an error when the loader is built, `include`
included: no handler can silently shadow another, and `#include` means what this section says under
every loader that has it. A loader can instead *disable* a name, whether a handler for it exists or
not: a call of it is then a `DisabledDirective`, not an `UnknownDirective`, so a reader learns that
the directive was turned off rather than misspelled. Disabling is final. Registering a disabled
name, before or after disabling it, is an error when the loader is built, so a handler cannot bring
a directive back; disabling a name twice counts once. This is how a loader for input it does not
trust leaves out `#include`. Both restrictions can be lifted later without breaking a v1 file.

**A handler's contract** (session 11). A handler declares the names of its parameters and gets the
arguments as a `Node.IMap` keyed by them, positional arguments already laid out over the parameters
in order, as markers' are (§5); so the map never mixes the two. Every value is a scalar string, as
everywhere in `cfg`: `true` and `"true"`, `2` and `"2"` arrive the same, and reading one, and
reporting an argument it cannot read, is the handler's job. What a handler gives back is not a
`Node` but a *fragment*, Psi's own unexpanded tree, opaque to the handler, and it gets one in one of
two ways:

- from its context, by parsing another source — what `#include` does. A `${…}` in that source stays
  unresolved until phase 4, so an included file may still refer to the file that included it (§8);
- from a `Node` it built — such a fragment holds no substitution. Its ids are dropped: where the
  fragment lands is the directive's position.

In statement position the fragment must be a map, in value position any node (above).

**Tags before a directive in value position** are added to the tags of the node it produces, as
tags before a whole-value `${…}` are added to the grafted node's (§9): `a: @t #include(…)`.

`#include` is the only built-in, reserved and possibly disabled (above):

```
#include(file: "conf.d/extra")              // relative to the including file's URL
#include(file: "local", optional: true)     // missing file is not an error
#include(resource: "reference")             // classpath, JVM only
#include(url: "https://…")
```

Exactly one of `file` / `resource` / `url` is required. Relative paths resolve against the URL of the
including file, never the working directory. Include cycles are reported as an error rather than
looping; how they are caught, for every directive and not `#include` alone, is open (§13 item 5).

## 7. Merging

Deep merge, last one wins — maps merge recursively, scalars and sequences are replaced wholesale.
The same rule applies in all three places it is needed, so there is one behaviour to learn:

- duplicate keys within one file,
- a directive's result against the block it lands in,
- the N URLs passed to the loader.

This follows the call already made for `impl/yaml`. `null` is a value like any that is not a map:
`a: null` replaces a map with `:`, and a map after it replaces the `null`.

**The three are one fold.** Every field is merged in turn into the map built so far, a block into
the map already at its key, and a later source goes on where the one before it ended. A dotted key
is a nest of blocks — `a.b.c: v` is `a { b { c: v } }`, each block standing where its key segment
is written — so `a.b.c: 1` and `a.b.d: 2` define `a` and `b` twice each, and merge them.

**Key order** is the order of first appearance, as in `impl/yaml`. A key keeps its place when a
later value replaces its own, `:=` included; one dropped by an error (below) is gone, and a later
definition puts it last.

**Location of a merged map.** A map defined in one place carries that place: the `{` of its block,
or its key segment in a dotted key. A map defined in several, merged, carries every one of them,
the latest first (§10): an error about the map as a whole points first where it was defined last,
usually the file closest to the application, and lists the rest. `:=` starts the list anew. The root
of a load lists the start of each source.

**Tags in a merge.** When two maps merge, the result carries the union of their tags, as classes
accumulate in CSS. Being untagged is not a reason to drop a tag:

```
routes: @ordered { a: 1 }
routes { b: 2 }            // @ordered { a: 1, b: 2 }
routes: @sorted { c: 3 }   // @ordered @sorted { a: 1, b: 2, c: 3 }
```

A replaced value carries only its own tags; with `:=` that holds for maps too. `:=` is also the
only way to drop an inherited tag in v1 — it drops the fields with it. A finer tool is reserved,
not designed: `@-name` (§5).

**Explicit replacement — `:=`.** A field written with `:=` discards whatever the key held before and
takes the new value as is, with its own tags:

```
// reference.conf
pool { min: 1, max: 10, idle: 30s }

// app.conf
pool := { max: 4 }    // pool is { max: 4 }; min and idle are gone
```

With `:` the result would have been `{ min: 1, max: 4, idle: 30s }`, and there is no other way to drop
a key that an earlier source defined. For scalars and sequences `:=` gives the same result as `:`, so
in practice it matters for maps, and anywhere the author wants the intent to be visible.

- With a dotted key the operator applies to the last key only: `a.b.c := { … }` replaces `c` and
  merges `a` and `b` as usual.
- The operator is kept in the syntax tree until merging, and works in all three places listed above:
  `:=` in an included file resets what the including block held before the `#include`, and `:=` in
  the second URL resets what the first defined.
- With no earlier value `:=` behaves like `:`.
- `a := ${b}` is always allowed: whatever `${b}` resolves to, nothing before it survives.

**No merging over a substitution in v1.** Merging runs in phase 3 and substitutions resolve in phase 4
(§8), so when one side of a `:` merge is a whole-value `${…}`, phase 3 cannot know whether it is
merging two maps or replacing one value with another. Instead of deferring the merge, v1 rejects it:
a `:` field meeting an earlier value of the same key is an error when one side is a substitution and
the other is a map or a substitution.

```
a: ${server}
a { port: 1 }      // error: cannot merge into a substitution, use := to replace

b { port: 1 }
b: ${server}       // error: ${server} may be a map, use := to replace

c: ${x}
c: 5               // fine: a scalar replaces whatever ${x} turns out to be
```

A concatenation such as `"jdbc:${host}"` is always a scalar and never triggers the rule; `${?x}` does,
since whether it resolves is not known in phase 3 either. The rule applies at every depth of a
recursive merge, and the error offers `:=` as the fix. Merge errors are reported in the same batch as
substitution errors (§11); the key is dropped, and nodes depending on it are silenced by the
root-cause rule.

There is deliberately no append operator. Appending to a sequence or concatenating values is a job for
an expression in value position — `a: ${b} + ${c}` — which can be added later (§13); replacement
cannot be expressed any other way.

## 8. Phase order

1. Parse each source URL into a syntax tree, directives unexpanded.
2. Expand directives depth-first. `#include` parses the referenced source recursively and merges it
   positionally. Cycle detection lives here.
3. Merge the input sources, last wins.
4. Resolve substitutions over the merged tree.
5. Wrap the result as `Node.IMap[Id.Root]`.

The consequence worth stating out loud: because 2 runs before 4, an included file may reference
values from the file that included it and vice versa — but an include target can never be computed
from a substitution.

## 9. Substitutions

- `${path}` in place of a whole value grafts the referenced node, whatever it is — scalar, block or
  sequence.
- **A path is written as `Id.path` renders one** (§3), right after `${` or `${?` and right up to
  `}`, with no whitespace: keys joined by dots, quoted in backticks when
  not bare, and indices into sequences in brackets — `${x.y[7].z}`, `${a[0][1]}`,
  ``${server.`odd key`[0]}``. An index is a decimal with no sign and no leading zeros, as `Id.path`
  prints it, so `[07]` and `[-1]` are errors. An index past the end of a sequence resolves to
  nothing, as a missing key does: an error for `${…}`, an omitted field for `${?…}`.
- Tags written before a whole-value `${…}` are added to the grafted node's own: with
  `b: @x { … }`, `a: @y ${b}` gives `a` the tags `x` and `y`. The union is the same as in a merge
  (§7), so a graft and a merge never disagree about tags.
- A concatenation builds a new scalar, which carries only the tags written before it: in
  `@t "jdbc:${host}/db"` the result is tagged `t` alone, whatever `host` carries.
- **A concatenation is one scalar with `${…}` inside it**: unquoted, `jdbc:${host}/db`, or quoted,
  `"jdbc:${host}/db"` and `"""…${x}…"""`. A `${…}` inside quotes concatenates even alone: `"${a}"` is
  a concatenation, not a graft. The referenced node must then be a scalar, and the result is a
  scalar. Values never join: `"jdbc:"${host}"/db"`, HOCON's spelling, is an error with a hint to move
  the substitution inside the quotes — it follows from the rule that after a value comes only a
  separator, a comment or a closing bracket (§3), and so does `"a" ${b}`, whitespace never joining
  values.
- `${?path}` is optional: when it does not resolve, the field is omitted entirely, so a later lookup
  yields `Node.INone` rather than an error. As an element of a sequence, it drops the element, the
  ones after it moving up. As a part of a concatenation, it reads as the empty string.
- Lookup order is the merged tree, then system properties, then environment variables — the HOCON
  behaviour, chosen deliberately over an explicit `${env:…}` form. Each is looked up by the whole
  path, and the first that has it wins; a path that fails in the tree for another reason than its
  absence — a cycle, a node that failed — is not looked up further.
- **System properties form a tree of strings**, their names split at every dot: `java.home` is
  `home` in `java`, so `${java.home}` is the property and `${java}` grafts a map of every `java.*`
  one. Where a name is both a value and a prefix of others, `a=1` and `a.b=2`, the map wins and the
  value is lost, as in HOCON. **Environment variables are not split**: each is a key at the top,
  its value a string, so `${HOME}` reads `HOME` and a multi-segment path never reaches the
  environment. Turning a string into structure — a list out of `PATH` — is a directive's job, not
  the lookup's.
- **A grafted node takes its new position**, the id of the field it is grafted at, not that of the
  node it came from; its location keeps the origin (§10). The graft is made in the merged tree,
  before the nodes are wrapped, so `Node` never hands a container a foreign id.
- The graft keeps `null`: `b: ${a}` with `a: null` gives an `INull` at `b`, which is not the same
  as `${?a}` over a missing `a`.
- Cycles are an error naming the path. Self-reference (`a: ${a}" x"` reading the pre-merge value) is
  **not** supported in v1; it is reported as a cycle.

## 10. Locations

Every node carries its own `Location`, with `description` rendered as `<source>:<line>:<column>`.
This is the visible win over `impl/hocon`, where the description is whatever typesafe-config
composed.

**`<source>` is the source as it was named**, so the reader sees the name they wrote and can find
the file by it:

- a source passed to `Psi(urls*)` — the full URL, as the caller gave it (`file:/etc/app/app.psi`);
- a source pulled in by `#include` — the argument as written in the directive (`conf.d/extra`,
  `reference`, `https://…`), not the URL it resolves to. A `file` argument is therefore relative to
  the including file, not to the working directory or the top-level source.

The same written name can stand for different files when two files in different directories include
it; the location does not disambiguate them. Accepted: the including file is one step away, and an
error inside an include can name the include chain if that turns out to be needed.

**Line and column are 1-based**, as in the YAML backend. **The column counts code points**, not
UTF-16 units: `é`, `ж` and `😀` are one column each, so the number matches what an editor shows
rather than `String.length`. A tab is one column. A line ends at LF or CRLF (§3); the CR of a CRLF is
not part of the line.

A node produced by a substitution carries **both** sites: the reference site as its primary
coordinates, and the location of the node it was taken from. `Location` is an open trait in `cfg`, so
this needs no protocol change — the module defines its own hierarchy:

```scala
sealed trait PsiLocation extends Location

// written here
final case class SourceLocation(source: String, line: Int, column: Int) extends PsiLocation
// grafted here by `${path}`; `origin` is where the value came from
final case class ReferenceLocation(at: SourceLocation, path: String, origin: Location) extends PsiLocation
// a scalar built by concatenation; one entry per `${…}`, each with its own column
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends PsiLocation
// a map defined in several places, merged (§7); `at` the last, `earlier` the rest, the latest first
final case class MergedLocation(at: SourceLocation, earlier: ::[SourceLocation]) extends PsiLocation
// the root of a load given no source, an empty map written nowhere
case object NoSourceLocation extends PsiLocation
// origins that are not a file
final case class SystemPropertyLocation(name: String) extends PsiLocation
final case class EnvLocation(name: String) extends PsiLocation
```

`origin` is a `Location`, not a `SourceLocation`, so chains fall out on their own: with
`a: ${b}`, `b: ${c}`, `c: 1`, the node at `a` is `ReferenceLocation(a-site, "b",
ReferenceLocation(b-site, "c", SourceLocation(c-site)))`. The environment and system properties get
a location of their own instead of a fake file position.

`description` puts the reference site first, because that is where the reader has to go:
`file:/etc/app/app.psi:12:9 (${db.url} from base.psi:3:7)`. The other cases follow the same shape:

| Location | `description` |
|---|---|
| `SourceLocation` | `app.psi:12:9` |
| `ReferenceLocation` | `app.psi:12:9 (${db.url} from base.psi:3:7)` |
| — in a chain | `app.psi:1:4 (${b} from app.psi:2:4 (${c} from app.psi:3:4))` |
| `ConcatenationLocation` | `app.psi:12:9 (${host} from base.psi:3:7, ${port} from env DB_PORT)` |
| `MergedLocation` | `app.psi:12:1 (also app.psi:7:1, base.psi:3:1)` |
| `NoSourceLocation` | `no source` |
| `SystemPropertyLocation` | `system property db.url` |
| `EnvLocation` | `env DB_PORT` |

A part of a concatenation is rendered without its own site, which shares the concatenation's line;
the part's `ReferenceLocation` still holds it.

**Grafted containers.** With `a: ${server}`, *every* node in the grafted subtree gets a
`ReferenceLocation` — the reference site of `a`, the path it was taken by (`server.port`), and that
node's own origin. Marking only the root would leave an error at `a.port` pointing at `server`'s
line with no hint of how it ended up under `a`.

**Concatenation.** `url: "jdbc:${host}/db"` is a new scalar written at the reference site, so its
primary coordinates are that site, and it carries one `ReferenceLocation` per `${…}` in the order
written — every source the value was assembled from.

## 11. Errors

The loader returns `Either[PsiErrors, Node.IMap[Id.Root]]`, where `PsiErrors` is a non-empty list of
module-local errors and itself a `CfgError` — `Either` is covariant, so it reads as
`Either[CfgError, …]` to anyone who does not care. The list is the module's own type (head + `List`),
because `cfg` has no type that accumulates errors and Cats is not a dependency here. Every
error carries a `Location`.

The goal is **as many errors per run as can be reported without inventing any**: a user fixing a
config should not have to rerun the loader once per mistake, but a cascade of consequences of one
mistake is worse than stopping.

- **Parsing recovers at synchronisation points.** A broken item is skipped to the next separator
  (newline or `,`) at the same bracket depth, or to the bracket closing its container; the parser
  tracks bracket depth to find them, and skips a container or a directive's arguments inside the
  broken item whole. The item is a field in a block, an element in `[…]` and an argument in
  `#x(…)` alike. The braced syntax is what makes this cheap. Each source is parsed independently,
  so an error in one file never hides errors in another.
- **What recovery skips leaves a hole.** The broken item is left out of the syntax tree, with no
  node of its own standing for it; phases 3–5 never see the tree (below), and a directive's
  arguments are literals, so no hole can make an error of its own. What the lexer finds in skipped
  text is not reported, a fatal error excepted: the parser cannot tell what the text was meant to be.
- **A directive call with any error in it is left out whole**, with the field or element it is the
  value of. Every error in its arguments is reported, a duplicate name and a positional argument
  after a named one included, but its handler never runs on arguments that are not what was meant.
- **Some errors end the source.** An unterminated `"…"` or `"""…"""`, or an unbalanced bracket,
  leaves nothing trustworthy to resynchronise on; the parser reports it and stops *that source*,
  keeping the fields read before it. A bracket is unbalanced at the end of input, and where it
  closes another one than the innermost open — `x { a: [1 }`, reported as the `[` unclosed when the
  bracket closes one further out, as a bracket closing nothing otherwise: where the inner one was
  meant to close is a guess, and a wrong guess makes errors of its own. A closing bracket with
  nothing open at all, at the top level, is reported and skipped.
- **Directive failures are local.** A missing include, a cycle, an unknown or disabled
  directive or a bad argument is reported, the directive contributes nothing, and expansion
  continues.
- **Phases are gated.** If phases 1–2 (§8) produced any error, phases 3–5 do not run: substitutions
  over a tree with holes cut by recovery would report references to fields that exist but were lost
  in the skip. Syntax and directive errors come as one batch; substitution errors, if the first
  batch was empty, as the next.
- **Substitution errors report root causes only.** Every unresolved reference, cycle and
  concatenation of a non-scalar is reported, but a node that depends on an already failed node is
  dropped silently: with `a: ${missing}` and `b: ${a}`, only `missing` is named.

## 12. Implementation notes

- No external dependencies, like `cfg` and `impl/hocon`.
- Unlike the hocon and yaml backends, which wrap a foreign structure lazily, this module owns its
  data: phase 5 builds immutable `Map`/`Vector` structures once, and `MapImpl`/`SeqImpl` are thin
  views over them.
- Cross-built for 2.13, 2.12 and 3.9, from one source tree in Scala 2 style that Scala 3 accepts. A
  hand-written lexer is exactly the kind of code that reaches for 2.13-only collection methods;
  `scala-collection-compat` is already wired in for 2.12.
- Warnings are fatal on every version (`-Xfatal-warnings` on 2.x, `-Werror` on 3), so exhaustiveness
  in the parser's pattern matches is enforced rather than merely intended.

## 13. Open questions

1. **Value expressions**, e.g. `${b} + ${c}` for sequence concatenation — the replacement for an
   append operator (§7). Not in v1. Whitespace never joins values (§3), so `${b} + ${c}` is an error
   today and the syntax stays free. **Parked** by the user as hard from every angle — do not reopen
   it until it is raised again.
2. **Indentation as an alternative to braces**, Scala 3 style — both forms allowed. Not in v1; v1 is
   braces only. **Parked until implementation of indentation starts** — do not reopen it before then.

   Settled:
   - The lexer turns indentation into virtual `{` / `}` from a stack of widths; the grammar in §4 and
     the parser stay unchanged, which is why it can be added later.
   - A tab in indentation is an error — already in v1 (§3), so no v1 file holds one.
   - A dedent must land exactly on a width in the stack. Recovery (§11): every line is reported as an
     error until one lands on a width from the stack again.
   - **Sequences use a `- ` element marker.** Unquoted scalars contain no whitespace (§3), so a line
     starting with `- ` is always an element: `- 5` is an element holding `5`, while `-5` and
     `"- 5"` are scalars. Bare keys cannot start with `-` either, so the marker is never a key.
     Whether a block opened by indentation is a map or a sequence is decided by its first line;
     mixing fields and `- ` elements in one block is an error.
   - **A map element is written as in YAML:** its first field on the marker's line, the rest aligned
     under that field's key. The column after `- ` becomes the element's indentation baseline.

     ```
     servers:
       - host: a
         port: 1
       - host: b
         port: 2
     ```

   Proposed, not confirmed:
   - `key:` at the end of a line, followed by a deeper-indented line, opens a block — the Scala 3
     rule for `:` at the end of a line, and what `key:` would lead a reader to expect anyway. A bare
     key alone on a line stays an error.
   - A container tag goes after `:`: `routes: @ordered`, block on the next line.
   - Blank lines and comment-only lines do not take part in indentation.

   Open:
   - **Backward compatibility with v1 files.** Session 2 sketched indentation as significant at file
     level and inside `{…}`. That breaks any v1 file with sloppy indentation inside braces — the
     dedent rule would reject it. The alternative: indentation is significant only inside a block
     that was *opened* by indentation, and an explicit `{` switches it off until its `}`. Then no v1
     file changes meaning, but a braced block cannot contain an indented one.
   - **Mixing styles** in one file, and in one block.
   - **Indent width** — any deeper indent opens a block, or one step fixed per file (which would also
     catch some shifted lines).
   - **Silently shifted lines.** A line moved by exactly one level is still valid and changes parent.
     Braces make that mistake loud; the stack rule catches only misaligned lines. Accept, or find a
     mitigation (fixed width above is one).
   - **Opt-in.** Enabled everywhere automatically, or per file (a directive, or a separate extension).

   Constraint on v1 meanwhile: leading whitespace must carry no meaning for the structure, so that
   indentation can claim it later without changing what a v1 file means. Inside a triple-quoted
   string it is content (§5); the explicit width reads the indent of its opening line, which keeps
   meaning the same under either syntax.
3. ~~**Multi-line strings**~~ — closed in session 7: reopened by the user and settled point by
   point (§5, §14).
4. **Templates in the direction of StringTemplate 4.** Raised by the user in session 7: substitutions
   stay as they are in v1, but the way forward is to look at ST4's ideas. Nothing is decided yet.
   Noted so far:
   - **Delimiters chosen per template.** ST4 lets a template use `<…>` or `$…$`, whichever does not
     clash with its content. The case for it: a shell script, a Helm template or a Spring config in
     a `"""` block is full of `${`, each of which needs `\${` in v1 — the common case is settled by
     `'''…'''`, which substitutes nothing (§5). A marker on the opening quotes
     could turn substitution off or change its syntax; markers do not combine in v1 (§5), and this
     one would have to combine with the indent markers.
   - **A template applied to a collection with a separator**, `<hosts; separator=",">`. This is the
     job parked as value expressions (item 1, `${b} + ${c}`), so both may end up as one
     mechanism.
   - **Options inside `${…}`**, the user's idea: `${array; null="-1", separator=", "}`. Not in v1;
     the syntax stays free, `;` being outside `Id.SafeKeyPattern`, so `${a; …}` is an error today.

5. **Cycles, for every directive.** Raised by the user in session 12: a universal way to catch
   cycles, not one for `#include` alone. Every cycle runs through the context's way to parse
   another source (§6), so the framework can keep the stack there for every handler. Open: what
   makes two sources the same (a key passed with the source; by default the normalized URI and a
   file's real path; a handler's own for a source that is no URL), a depth limit as a backstop, a
   source included twice not through itself (no cycle, so presumably allowed; parsed again or cached), and where the error
   points. To be settled in plan step 10.

6. **Meta-fields.** Raised by the user in session 15, **after v1**: a meta-field `id`, the key or
   index of an object, to start with. The protocol already gives every node its `Id`; what a
   meta-field adds is the value inside the config text, where it makes templates of substitutions:

   ```
   defaults { name: ${@id}, host: "${@id}.internal" }   // `${@id}` a placeholder syntax
   servers {
     alpha: ${defaults}     // name: alpha, host: alpha.internal
     beta:  ${defaults}
   }
   ```

   Nothing is decided. Open: whose `id` — the object the field stands in, or the field's own, and a
   way up to a parent's; when it binds — where the node ends up after merging and grafting, which the
   template needs and which puts it in phase 4 (§8, §9), or where it is written; the syntax, which no
   key, tag, directive or `Id.path` may clash with; what it gives at the root, and in a directive's
   arguments, where substitutions are not allowed (§6); which other meta-fields (`path`, `parent`,
   the source, the tags) and whether their set is fixed or open, with handlers like directives.

   **Session 16, the user's addition: meta-fields of any node.** A meta-field is read off whatever
   node a path leads to, `${path^meta}`; with no path, `${^meta}`, off the object the reference
   stands in. `^` is the user's placeholder, open to change. Proposed alongside, not decided:
   `${^^meta}` for the parent's, one `^` per level up, as git's `HEAD^^`. Noted so far:
   - The syntax is free: `^` is outside `Id.SafeKeyPattern`, so `${a.b^id}` splits into a path and a
     meta-field with no ambiguity, a key with `^` in it is written in backticks as any other, and
     `${a^…}` is an error today, so adding it later breaks no v1 file.
   - With a path, `id` says little — it is the path's last segment — but other meta-fields do:
     `${workers^size}`, `${db^tags}`, `${db.url^source}`. The user had no use for `${field^id}` yet.
   - With the options of item 4, presumably node, meta-field, then options: `${a^size; …}`.

## 14. Decision log

Each entry records what was chosen, and — where it matters — what was rejected and why. A rejected
alternative listed here should not be re-proposed without new information.

### Session 1 — 2026-08-31

- **A format of our own, rather than another wrapper.** Considered and set aside in the same
  discussion: a `Node` implementation over plain Scala collections, a loader for env vars and system
  properties, and a Scala Native cross-build. Those are separate modules if they are ever wanted;
  none of them is this one.
- **Nested blocks with braces.** Rejected: flat `a.b.c = v` paths (smallest parser, but the file
  stops showing the shape of the tree), indentation (hardest lexer, and it would look like YAML
  while behaving differently), INI-style sections.
- **Everything in v1** — tags, substitutions, includes, multi-line strings. The scope is knowingly
  large; the format is meant to replace HOCON for our own use, and a subset that cannot express an
  existing config would not do that.
- **Directives instead of keywords.** The user's call, and the one structural idea in the format:
  `include` in HOCON is a magic word that cannot be used as a key and cannot be extended.
  `@name(arg = …)` gives an open set with room for whatever comes after `@include`.
  Consequence discovered while writing this up: directive arguments cannot contain substitutions
  (§6, §8) — the phase order forbids it.
- **Tags on containers, not only scalars.** `ISome.tag` exists on every present node, so restricting
  tags to leaves would throw away a field the protocol already has.
- **HOCON-style substitution lookup** (tree, then system properties, then environment). Rejected: an
  explicit `${env:…}` form, which shows the value's origin in the file but breaks the habits of
  everyone migrating from HOCON.
- **Triple quotes with the common indent stripped.** Rejected: raw triple quotes with no stripping
  (config indented inside a block would carry the block's indentation into the value), heredocs.
- **Name deferred.** Candidates raised and not chosen: `native` (collides with Scala Native, and is
  a Scala 2 keyword — the package would need backticks), `SCON`, `H8CON`, `H8`, and role-based names
  like `cfg-text`. To be settled before any code is written, since it appears in the directory,
  artifact id, package, file extension and loader object name.

### Session 2 — 2026-09-25

- **Error model: `Either`.** The reason given was dependency minimisation, held to as long as
  possible. Rejected: exceptions (what `HOCON` and `YAML` do today) and `Validated` (pulls in Cats).
- **Report as many errors as possible, not the first one.** The user's requirement, added after the
  first draft of this entry said "first error only" because `AndError` lives in `schema`. Resolved
  without Cats: the left side is a module-local non-empty error list that is itself a `CfgError`,
  and the parser recovers at synchronisation points (§11).
- **Phases are gated, and substitution errors name root causes only** (§11). Both proposed in
  session 2 and accepted as written: running substitutions over a tree with holes cut by recovery
  would report fields that exist but were skipped, and reporting every dependant of a failed
  reference buries the one line that needs fixing.
- **`null` is a reserved word — probably the only one.** Rejected: a `!null` tag (tags are for the
  decoder, and `INull` already has a `tag` field of its own), an `@null` directive (directives
  expand to maps or nodes during parsing; spending one on a constant is ceremony), an empty value
  (invisible, and easy to produce by accident). This amends the "no special words" row in §2 rather
  than overturning it: the directive system still covers everything extensible.
- **A quoted key is indivisible.** Dots split only between key tokens, so `"a.b"` is one key. This
  matches what `Id.path` already renders.
- **Substitution location: reference site, with the definition site carried along.** The user's
  proposal of two location types. Rejected: definition site only (the proposal in session 1) — it
  points away from the line that actually put the value there.
- **Every node of a grafted subtree gets a `ReferenceLocation`**, not only its root.
- **A concatenated scalar carries the list of its sources**, one `ReferenceLocation` per `${…}`.
  Rejected: a plain `SourceLocation` at the reference site, which forgets where the parts came from.
- **`impl/hocon` stays**, alongside this module, rather than being retired by it.
- **Indentation deferred, braces only in v1.** Raised by the user as Scala 3-style dual syntax, which
  is new information against session 1's rejection (that rejected indentation *instead of* braces).
  Kept as an open question (§13) with the mechanics recorded there; the user's rules so far: tabs in
  indentation are an error, and a misaligned dedent is reported on every line until indentation
  matches the stack again.

- **Unquoted scalars contain no whitespace; a value with spaces is quoted.** The user's rule, and it
  applies to v1, not only to indentation: it replaces "unquoted scalars run to the end of the line".
  Since every value is a string anyway, nothing is lost but the quotes. It is what makes the
  indentation sequence marker unambiguous — `- 5` is an element, `-5` and `"- 5"` are scalars —
  and it turns a stray second word on a line into an error instead of part of the value.
- **`{` must be on the key's line.** Forbidden in v1 so that a key at the end of a line stays free to
  open an indentation block later (§13). Rejected: allowing it, which would make that position
  ambiguous the moment indentation is added.
- **A map inside an indentation sequence is written as in YAML**: first field on the `- ` line, the
  rest aligned under it. Rejected: `-` alone on a line with the map below it, and allowing both.
- **`:` between key and value, not `=`.** The user's preference, and close to Scala 3. `=` is as
  common inside values as `:` (SQL, expressions), so neither frees values from quoting. The known
  cost: a file with `:`, indentation and `- ` looks like YAML, and pasted YAML mostly fails loudly —
  multi-word values, block scalars, anchors — but `a: 'x'` and `a: ~` would parse silently to
  something else (§13). The gain: `key:` at the end of a line becomes a natural indentation opener,
  as in Scala 3, and the proposal in §13 now uses it. Rejected: allowing both `:` and `=` as HOCON
  does — two spellings for one thing.
- **Details of `:`.** Whitespace or end of line is required after it. An unquoted value may not
  start with `'`, which closes the silent YAML difference for `a: 'x'`; `~` gets no rule, being one
  character with `null` spelled out. Directive arguments keep `name = literal`: a call with named
  arguments, as in Scala, distinct from fields.
- **Separators: `,` and newline only.** Whitespace never separates — `[foo bar]` with forgotten
  quotes would otherwise silently become two elements, the same failure `a: foo bar` was made an
  error to prevent; for the same reason concatenation parts must touch. After any value comes a
  separator, a comment or a closing bracket. Empty elements are errors; a trailing separator is
  allowed in sequences and blocks. Directive arguments follow the same rule. Newline is LF or CRLF.

### Session 3 — 2026-10-05

- **`:=` for explicit replacement.** The user's proposal. Deep merge left no way to drop a key that an
  earlier source or an include defined; `:=` discards the earlier value whatever it was. `:` keeps its
  meaning, and so does the block form `key { … }`. The spelling matches sbt.
- **No append operator.** Proposed together with `:=` as `:+` (append to a sequence, merge into a
  map), then dropped by the user: appending can later be expressed as a value expression such as
  `a: ${b} + ${c}` (§13), while replacement has no other spelling. Also discussed and left with it:
  whether `:+` takes an element (as in Scala and HOCON's `+=`) or a sequence.
- **Merging over an unresolved substitution is unspecified** — found while discussing `:+`, but it
  applies to plain `:` already. Recorded as open (§13).
- **Value expressions parked.** Sequence concatenation through expressions is left for later, by the
  user's call: it is not simple from any angle. §13 keeps it as a parked item.
- **No merging over a substitution in v1** (§7). The user's call, the first of two readings offered:
  deep merge stays, but a `:` merge where one side is a whole-value `${…}` and the other a map or a
  substitution is an error pointing at `:=`. Rejected for now: a deferred merge resolved in phase 4
  (HOCON's `ConfigDelayedMerge`), which is what lifting the restriction later would take. Also
  offered and not meant: dropping deep merge from v1 entirely.
- **A merged map takes the later tag if written, the earlier one otherwise** (§7). The user's call.
- **Name: DEF, Declarative Extensible Format.** The user's proposal; "extensible" stands for the
  directive system (§6). `def` is a keyword in Scala 2 and 3, so the package cannot carry the name.
  The user rejected backticks (``h8io.cfg.impl.`def` ``) — every user importing the public location
  and error types would have to write them. Read as an acronym, the name fits the `HOCON`/`YAML`
  convention for the loader object. The package and the sbt project are `defg` — *def* + *cfg*,
  offered by the user half as a joke: no keyword, no association with anything else, searchable.
  Rejected: `df`, briefly adopted, because it reads as a Spark DataFrame and the format is expected
  to be used with Spark, where a local `df` would also shadow the package; an upper-case package
  `DEF` (clashes with the object of the same name and goes against JVM convention); `defs`, which
  the user did not like. The extension `.def` was checked: GitHub Linguist
  maps it to no language, and its other uses (Windows linker module-definition files, Apptainer
  container definitions, Modula-2) barely overlap with a Scala project's resources.

  **Superseded in session 10:** the format is Ψ (Psi), and the entries of this log call it so.
- **Correction to session 1:** `native` is not a Scala 2 keyword (Scala has the `@native` annotation,
  not a reserved word). The collision with Scala Native alone is enough to keep it rejected.

### Session 4 — 2026-10-15

- **`<source>` in a location is the source as it was named** (§10): the full URL for a source given
  to the loader, the argument as written for an include. The user's call — "the full URL or a
  relative path" — refined into this rule. Rejected: the full resolved URL everywhere (long, and an
  include shows up under a name its author never wrote); the file name alone (ambiguous across
  directories and meaningless for a URL or a resource); a path relative to the working directory
  (includes resolve against the including file, never the working directory, §6).
- **Line and column 1-based, column in code points, tab = 1** (§10). The user's call, on the
  recommendation. Rejected: UTF-16 units — what `String.length` gives, but an astral character such
  as an emoji counts twice and the column drifts from what the editor shows. Not discussed: grapheme
  clusters (`e` + combining accent as one column) — they need `BreakIterator`, and editors disagree
  about them anyway.

### Session 5 — 2026-10-24

- **Backticks for names, double quotes for values** (§3). The user's proposal, after weighing SQL's
  separate identifier quotes. A key — in a field, a dotted key, a `${…}` path or a tag — is bare when
  it matches `Id.SafeKeyPattern` and in `` `…` `` otherwise; `"…"` is only a scalar, and `"port": 1`
  is an error. Gained: the role of a token is lexical, a name in value position stays free for
  later syntax, and paths, tags and directive arguments read by one rule. Paid: `Id.quote` in `cfg`
  moves to backticks, changing the paths every loader prints (accepted by the user); JSON and HOCON
  habits break (intended — strictness is preferred over compatibility with HOCON and YAML); inline
  code in the Markdown docs needs double backticks. Rejected: `"…"` for keys, as before — position
  already tells a key from a value, but it closes value position to names for good. Supersedes
  session 2's `"a.b"` example; the indivisibility it records stands.
- **A quoted safe key is allowed**: `` `port` `` is `port`. The user's call — it does not weaken
  strictness, and a generator of configs need not reproduce `SafeKeyPattern`.
- **No raw newline inside `` `…` `` or `"…"`.** Agreed on the recommendation.
- **Tag names follow the key rule; a quoted tag is taken verbatim**, like YAML's `!<…>`. Agreed; the
  prefix and the string in `ISome.tag` were settled later the same session (below).
- **Backslash escapes inside backticks**, the same set as in `"…"` (§3). The user's call, on the
  recommendation: one escape grammar for the whole language, and every string representable, which
  the round-trip property of `Id.path` needs. Rejected: doubling as in SQL (``` `my ``key`` ``` for
  ``my `key` ``) — a second mechanism beside the backslashes of `"…"`, and with raw newlines
  forbidden a key holding a control character could not be written at all; both forms at once —
  two spellings of one thing. Both quotes being escapable in both forms follows from the one set.
- **Comments are `//` and nesting `/* */`; `/** */` is reserved for doc comments** (§3). The user's
  call. Reasons: C-style comments suit the user's taste better than `#`, which reads as a scripting
  language, and Psi is meant as a base for DSLs, which may want documented configuration — `/** */`
  is the familiar form, `#` has none (`##` would be weak). Comments open only outside an unquoted
  scalar, so URLs need no quotes, at the price of `a: 1//x` being one scalar; values starting with
  `//` or `/*` (globs) need quotes. Block comments nest, as in Scala. Doc comments are positioned
  and kept as tokens from v1, so that giving them meaning later breaks nothing. Rejected: `#`, the
  session 1 choice, and with it the "deliberately one comment form" rule.
- **`ISome.tag` holds the tag's name only**, without the prefix or the quotes (§5). The user's call:
  both are syntax of the format and meaningless in the value. `impl/yaml` drops the leading `!` to
  match, a breaking change to `cfg-yaml`. Rejected: keeping YAML's `!` (the string would depend on
  the syntax that wrote it); letting Psi and YAML differ.
- **Tags need a prefix.** Rejected: `key: tag value`, unambiguous since an unquoted scalar holds no
  whitespace, because it takes over the position the "stray word is an error" rule guards:
  `greeting: Hello world` with forgotten quotes would become tag `Hello`, `[a b]` with a forgotten
  comma one tagged element, and in `level: debug info` a reader cannot tell which word is the tag.
  **Reopened** by the user later the same session, then closed again: the prefix stays. Tags may
  become several per node, as CSS classes are, and `a b value` cannot be read with more than one.
- **Tags take `@`, directives take `#`** (§5, §6). The user's proposal, swapping the session 1
  directive prefix. Tags are metadata on a value, as annotations are on a class (`@a @b value` reads
  as stacked annotations); directives are expanded during parsing, as by the C preprocessor
  (`#include`). Values starting with `@` or `#` are quoted, which the user accepted. `@name(` is an
  error for now, so arguments to tags stay available. Rejected: `!` (YAML's, no merit left once the
  tag string dropped it), `#` for tags with `@` directives, and no prefix (above). Pasted YAML
  comments fail loudly either way.

### Session 6 — 2026-10-26

- **The reason behind all of the below** (§5): the low-level protocol gives tags no meaning; the
  meaning of tags and how different tags merge are left to the higher level, so the rules here are
  the neutral, lossless ones. The user's statement, recorded so it is not forgotten.
- **Several tags per node, as a set** (§5). The user's call. Order carries no meaning, as with CSS
  classes; `ISome.tag: Option[String]` becomes `ISome.tags: Set[String]` in step 2 of the plan,
  together with the `!` change, so the API breaks once. Rejected: the user's own candidate from
  session 5, a tag that is one collection-valued annotation (`@[a, b]`) merging as a sequence does
  — it keeps "later wins" and order, and the user preferred classes that accumulate.
- **A merge takes the union of tags** (§7), replacing session 3's "the later tag if written, the
  earlier one otherwise", which existed only because a node had one slot.
- **A graft adds the written tags to the grafted node's** (§9). The user's call: the same union as
  in a merge.
- **`:=` is the only way to drop an inherited tag in v1; `@-name` is reserved** for a finer way
  (§5). The user's call. Free because a bare tag name cannot start with `-`; a backticked one can
  and stays an ordinary tag.
- **A tag written twice on one value collapses into one** (§5). The user's call; a set has no
  duplicates to report.
- **A concatenation carries only the tags written before it** (§9). The user's call: it is a new
  value, not a graft, so the tags of the referenced scalars do not reach it.

### Session 7 — 2026-10-30

- **A directive name follows the key rule** (§6): bare when it matches `Id.SafeKeyPattern`, in
  backticks otherwise. The user's call — the same rule as for every other name.
- **`${` is a substitution in every kind of scalar, quoted ones included; a literal one is `\${`**
  (§3). The user's call, after a misunderstanding earlier in the session recorded the opposite.
  Rejected: HOCON's rule, under which `${` inside quotes is literal and a substitution has to stand
  outside them (`"jdbc:"${host}`). `\$` joins the shared escape set; backticks hold no
  substitutions. `"""…"""` substitutes too; its literal `${` is left to the revision of multi-line
  strings.
- **Substitutions stay as they are in v1; templating to be looked at in the direction of
  StringTemplate 4** (§13). The user's call.
- **A byte order mark at the start of a source is skipped** (§3). The user's call. Rejected: an
  error — editors on Windows still write one, and it carries no meaning in UTF-8.
- **A tag may precede a multi-line string** (§5). The user's call; the grammar in §4 allowed it
  already, the plan asked to confirm it.
- **Multi-line strings reopened** by the user, to be thought through again before step 2 lexes
  them (§13).
- **The final newline of a multi-line string is explicit** (§5): the newline before the closing
  `"""` is dropped, symmetrically with the one after the opening, and a value that ends in `\n`
  has an empty line before the delimiter. The user's idea. Rejected: YAML's `|` / `|-` / `|+`
  markers, and Java's rule that the closing `"""` on its own line keeps the newline — both make an
  invisible detail decide the value.
- **Multi-line strings keep `"""`** (§5). Considered at the user's suggestion: a single `"` followed
  by the end of the line opening a multi-line string, since that newline is dropped anyway.
  Rejected, with the user's agreement: a `"` in the value would then close it, so the string either
  needs `\"` and loses being raw (a regex's `\d` becomes an error, §3) or cannot hold a `"` at all;
  a stray `key: "` would swallow the file up to the next quote instead of failing on its own line
  (§3, §11); and a space after the `"` would decide the mode invisibly.
- **Two forms of `"""`, block and one line** (§5). A block needs the end of the line right after the
  opening delimiter; text there makes a one-line string, which must close on the same line, and a newline in it is an error. The user's call, on a proposal: a first
  line after the delimiter has no column in common with the rest, which is what forces
  `stripMargin` on Scala and `dedent` on Python, while forbidding the one-line form outright would
  leave a regex with no way to be written without escapes. **Both forms substitute `${…}`** — the
  user's point: a one-line form that did not would mislead. They differ in layout only; the literal
  `${` stays open for both (§13).
- **A run of more than three quotes closes at its last three** (§5), Scala's rule, so a value can
  end in `"`. The user's call, on a proposal.
- **Whitespace between the opening `"""` of a block and the end of its line is an error** (§5), not
  fatal. The user's call, replacing the proposal to ignore it: it is invisible, and the slot after
  the delimiter is wanted for markers (§13).
- **Indent markers after the opening `"""` of a block** (§5): `<` strips all leading whitespace, `|`
  the first line's indent, `=` none, `>` folds as in YAML; no marker is Java's rule, the closing
  delimiter's column counting. The user's idea and choice of symbols. Rejected: `>` for keeping
  whitespace, which a YAML reader would take for folding. **Superseded in session 9:** `>` keeps
  the indent, `+` folds, `=` is gone.
- **The newline rules hold under every marker** (§5) — markers change leading whitespace only. The
  user's call, on a proposal.
- **Indents compare character by character; a mismatch is an error** (§5), including a line under
  `|` indented less than the first. The user's call, on a proposal.
- **An explicit indent width as in YAML** (§13): the user's pick over the closing delimiter's column
  for keeping a deliberate indent — which is kept as well, as the rule for no marker.
- **Folding under `>` follows YAML** (§5): the indent stripped as with no marker first, a deeper
  indented line left unfolded, an empty line a newline. The user's call, on a proposal.
- **The width counts from the indent of the opening line**, as in YAML (§5). The user's call, after
  an example: an absolute width — that many characters stripped — changes the value silently when a
  formatter shifts the block. The proposal's argument for it, that leading whitespace outside a
  string must carry no meaning (§13 item 2), was overstated: that constraint keeps v1 files from
  changing meaning once indentation arrives, and the indent of the opening line stays its indent
  under either syntax. Written `"""N` or `""">N`, N from 1 to 9 as in YAML.
- **Markers do not combine**, but for a width after `>` (§5). The user's call, on a proposal.
- **`"""|` dropped** (§5), on the user's question of how it differs from no marker. It gives the same
  value except where the first line is deeper than the rest (an error under `|`) or the closing
  delimiter stands left of the text (ignored under `|`): a weaker copy of the rule for no marker,
  whose only gain — independence from the closing delimiter — the explicit width gives more
  robustly. The user's call. `|` stays free, being an unknown marker; the candidate for it later is
  a margin as in Scala's `stripMargin`, each line starting with `|` after its indent.
- **Trailing whitespace is stripped from every line of a block** (§5), as Java does — under every
  marker, `=` included, which is about leading whitespace only; before folding; in the source text,
  so a substituted value keeps its own. The one-line form keeps it. The user's call: a value that
  needs trailing whitespace is exotic, and invisible spaces are a common source of bugs.
- **A CRLF in a block becomes LF in the value** (§5), as in Java; a substituted value keeps its own.
  The user's call. Rejected: keeping the file's newlines, which makes the value depend on the
  editor and on git's `core.autocrlf`.
- **A triple-quoted string takes no part in a concatenation** (§5, §9), in either form and on either
  side; what may follow it is what follows any value. The user's call: a substitution goes inside
  the quotes, which substitute anyway, and nothing else was seen to need the slot. An error with
  that hint, rather than silence, so the slot stays free.
- **`\"""` is the single escape of a triple-quoted string** (§5), as in Java; any other `\` stays
  literal. The user's call, on a proposal. Its cost: a `\` right before the closing delimiter
  escapes it, so a value ending in `\` needs the block form or `"…"`.
- **`\${` is the second escape of a triple-quoted string** (§5), the same as in `"…"`. The user's
  call, on a proposal. Turning substitution off for a whole block, which a script full of `${`
  wants, is left to templating (§13 item 4). With this §13 item 3 is closed.
- **Options inside `${…}` noted for later** (§13 item 4): `${array; null="-1", separator=", "}`. The
  user's idea, explicitly not in v1.
- **Indices in substitution paths** (§9): `${x.y[7].z}` is valid. The user's call, closing a gap of
  step 4. The rest follows from the round-trip promise of §3 — a path inside `${…}` is exactly what
  `Id.path` renders, so an index has no sign and no leading zeros; an index out of range is treated
  as a missing key.
- **The `description` of every location** (§10): a concatenation lists its parts as a reference
  does, comma-separated in one pair of parentheses; the environment and system properties are named
  in words. The user's call, on a proposal, at the start of step 1. Rejected: one part per line,
  which makes `description` multi-line.
- **Values never join into a concatenation** (§9): a concatenation is one scalar with `${…}` inside,
  and `"jdbc:"${host}"/db"` is an error. The user's call at the start of step 2b: the form reads
  badly and `"jdbc:${host}/db"` says the same. Supersedes session 2's touching parts and session 7's
  rule for `"""`, which now follows from the general one.
- **A `"` or `` ` `` inside an unquoted scalar is an error** (§3), with a hint to quote the value.
  The user's call: kept, `abc"def"` would silently carry its quotes.
- **A source that is not UTF-8 ends as unreadable** (§3), at the first bad byte. The user's call.
  Rejected: replacing it with U+FFFD, which corrupts a value silently.
- **A comment counts as whitespace** (§3), so a newline inside `/* … */` does not separate. The
  user's call. Rejected: such a comment counting as a newline.
- **No whitespace inside `${…}`** (§9): `${ a }` is an error with a hint. The user's call, on the
  recommendation — the path is written as `Id.path` renders it.
- **A lone CR is an error** (§3), anywhere in a source, quotes included; a value that needs a CR
  writes `\r` in `"…"`. The user's call, on a proposal giving the reasons. Tools disagree on what a
  lone CR is — VS Code and IntelliJ show a line break, `cat`, `less`, `grep`, `wc` and `git diff`
  one line — and a newline separates and ends a `//` comment, so any reading but an error makes
  `// old\rkey: v` a field to one reader and a comment to another. A terminal returns the carriage
  on a CR, so under `cat` the text after it overwrites the text before: `x: evil\r// harmless`
  displays as a comment. Line numbers (§10) would disagree with the editor or with `grep -n`,
  whichever reading were taken. Real sources hold a lone CR only from Mac OS 9 or from damage — a
  botched conversion, a split CRLF — which the error catches. And an error can be relaxed later
  without breaking a v1 file; the reverse cannot. Rejected: a newline, as in YAML 1.2, for the
  disagreement above; whitespace, as in JSON, which hides a field in the comment before it while the
  editor shows two lines; an ordinary character, which leaves an invisible CR in a value.

### Session 8 — 2026-11-04

- **Whitespace is space and tab; every other whitespace character outside quotes and comments is an
  error** (§3), read as whitespace once reported. The user's direction: whitespace as close to Scala
  3, YAML and Python as possible, with indentation as an alternative to braces coming later (§13).
  All three take a tab between tokens. Scala 3 and Python reject NBSP and the like, which YAML keeps
  in a value invisibly — the class of mistake that `a: foo bar` being an error guards against (§3),
  since `foo<NBSP>bar` would otherwise pass as one scalar. Form feed, whitespace to Python, is left
  out with YAML: a config has no use for it.
- **A tab in indentation is an error already in v1** (§3), rather than only once indentation is
  significant (§13). YAML's rule; Scala 3 and Python accept tabs and reject only a mix that cannot be
  compared. Made an error now because v1 must give leading whitespace no meaning that indentation
  could later contradict (§13), and a rule can be relaxed after v1 without breaking a file, not
  tightened. Blank and comment-only lines are exempt, as they take no part in indentation.
- **NEL, U+2028 and U+2029 are errors anywhere** (§3), quotes and comments included, for the reasons
  that make a lone CR one (session 7): VS Code shows them as a line break, `cat` and `grep` do not,
  so `// old<U+2028>key: v` is a field to one reader and a comment to another. The user's call. In
  `"…"` and backticks they are written with `\u`, and in `"""…"""` too, by the entry below. Other
  whitespace stays content inside quotes: NBSP has uses in text and breaks no line.
- **`"""…"""` takes the escape set of `"…"`** (§3, §5), `\\`, `\n`, `\r`, `\t` and `\u` included;
  a `\` outside the set is an error, as in `"…"`. The user's call, superseding session 7's two
  escapes (`\"""`, `\${`) and raw content. The trigger was the entry above: a raw string has no
  way to hold a lone CR, NEL, U+2028 or U+2029, and a `\u` on its own would have left no way to
  write a literal `\u0041`. One escape grammar for every kind of string, as session 5 chose for
  backticks. Java's text blocks work the same way, escapes included, and so does the order:
  escapes are interpreted after the layout rules, so `\t` at the end of a line and `\n` under
  folding survive. Its cost: a backslash is doubled, in regexes and Windows paths above all —
  `"""^\\d+"$"""` — but a forgotten one is an error, not a silent change. The cost of session 7,
  a value unable to end in `\` right before the delimiter, goes away: it is `\\"""`.
- **Raw strings `'…'` and `'''…'''`, as Perl's single quotes** (§5): no substitutions, and `\'` and
  `\\` the only escapes, any other `\` ordinary. The user's call, on a proposal following the entry
  above: escapes in `"""` cost regexes and Windows paths their backslashes, and a raw string beside
  the escaped one, as in Python and Rust, gives them back. `'` at the start of a value was reserved
  already (session 2), so no v1 file changes meaning, and YAML's `'x'` now means what it means
  there. Substituting nothing settles the common case of §13 item 4, a script full of `${`; it does
  not mislead as a one-line `"""` without substitutions would have (session 7), the delimiter
  itself saying so. Departing from Perl, `'…'` holds no raw newline, as `"…"` holds none, so an
  unterminated one is reported on its own line; `'''…'''` is the multi-line form, with the layout of
  `"""`. Rejected: YAML's and SQL's `''` for a quote, which leaves a backslash before the closing
  delimiter unwritable; Perl's escapes also give `'''` a literal `\'''`. The cost of Perl's rule:
  `\\` collapses, so a UNC path doubles its leading backslashes.

  **Superseded in session 9:** the one-line form of `"""` and `'''` is gone; `'^\d+"$'` is how a
  regex is written now.

**Confirmed after session 8** — proposed during the review of step 2b and written into the spec
then; confirmed by the user on 2026-11-06, before step 2c:

- **`'…'` on one line only**, departing from Perl, whose single quotes span lines (§5). The reason
  is the one for `"…"`: an unterminated quote reported on its own line. Rejected: Perl's rule as it
  stands, with `'''…'''` then only for the layout rules.
- **Escapes in `"""…"""` interpreted after the layout rules** (§5), Java's order (JEP 378): the
  layout reads an escape as the characters written, so what it produces is kept. `\t` at the end of
  a line survives the stripping of trailing whitespace, `\u0020` at the start of one is not indent,
  and `\n` survives folding. Rejected: escapes first — `\t` would then be stripped, leaving no way
  to end a block line in whitespace; `\u0020` would count as indent; and `\n` would be folded into
  a space or, under no marker, start a line with no indent and drop the common indent of the whole
  block to zero.
- **Whitespace other than space and tab inside `${…}`**: `${a<NBSP>b}` is one
  `InvalidSubstitution`, and the NBSP is skipped with the rest of the broken substitution, with no
  error of its own (§3, §9). One error per mistake, as §11 wants. Rejected: reporting both.

### Session 9 — 2026-11-06

- **No one-line form of `"""` and `'''`** (§5): a triple-quoted string is always a block. The
  user's call, finding the rules around the one-line form too tangled — which text after the
  delimiter is a marker and which a value, whether an unclosed line is fatal — for what it bought:
  a short value with quotes is written in `"…"` with `\"`, or in `'…'`, which needs no escape for
  `"` or `\`. Supersedes session 7's two forms, and with them the example `"""^\\d+"$"""`.
- **Whatever follows the opening delimiter on its line is the marker** (§5): one that is not known
  — `|`, `abc`, `a b`, `abc"""` — is an `UnknownMarker`, not fatal, and the block is read as with no
  marker. The user's call, against making any of it fatal: a fatal error ends the source for good,
  while a `"""` with a wrong marker is likely to be closed further down and read. Whitespace after
  an unknown marker is part of that one error; after a known marker or none, an error of its own.
- **`>` keeps the indent, `+` folds; `=` is gone** (§5). The user's call: `<` and `>` read as a
  pair — the text pushed to the left edge, or left where it stands — which is more logical than
  YAML's `>` for folding, and compatibility with YAML is not a goal for markers. Reverses session 7,
  which rejected `>` for keeping whitespace as misleading to a YAML reader. The width goes with
  folding: `"""+2`; `"""<2` and `""">2` are errors, as `"""=2` was. `=` becomes an unknown marker,
  free again; keeping it as a synonym of `>` was rejected — one spelling per rule. Noted when the
  symbol was picked: YAML's `|+` means keeping trailing newlines, so `+` does not read as folding
  to a YAML reader either.

**Open after session 9** — the user is thinking about markers and about the characters a string may
hold; to be settled before #134 (step 2c) merges:

- **Markers.** The set now: none (Java's rule), `<` strips all of the indent, `>` keeps it, `+`
  folds, a width `N` or `+N`; markers do not combine but for `+N`, `<N` and `>N` being errors; any
  other text after the delimiter an `UnknownMarker`, not fatal. Free and noted for later: `|`, the
  candidate for a margin as in Scala's `stripMargin`, and `=`, freed in this session. Open whether
  the set changes again, and how a marker that is not about indent — turning substitution off, or
  changing its syntax (§13 item 4) — would combine with the indent markers, which do not combine
  today.
- **Characters in strings.** As it stands: lone CR, NEL, U+2028 and U+2029 are errors anywhere,
  dropped once reported, and in `"…"`/`"""…"""` written as escapes; a raw string cannot hold them
  at all. Other whitespace (NBSP, FF, VT, …) and tabs are content inside quotes. **Other control
  characters — U+0001 and the like — are not restricted anywhere**, in quotes or out: the lexer
  only catches those Java counts as whitespace (FF, VT, U+001C–U+001F). Open whether they become
  errors, inside quotes, outside, or both, and how they are written then. **Settled in session 10**
  (below).

### Session 10 — 2026-11-07

- **Control characters other than tab are errors anywhere** (§3), quotes and comments included:
  C0 but tab, LF and the CR of a CRLF, then DEL and C1. The user's call. Until then the line ran
  where Java's `Character.isWhitespace` happens to draw it: FF, VT and U+001C–U+001F were errors
  outside quotes and content inside, while U+0001 or ESC passed anywhere. So `a: a<SOH>b` was a
  scalar of three characters that reads as two, contrary to session 8's rule against invisible
  differences; a NUL reached a value and failed far downstream; and an ESC in a value quoted by an
  error message drove the terminal. JSON forbids raw C0 in strings, TOML every control character
  but tab, YAML everything outside its printable set. Handled as a lone CR is: reported where it
  stands, read as whitespace outside quotes, dropped inside; inside `${…}` reported on its own
  beside the `InvalidSubstitution`, the invisible character being the cause. Escapes still write
  one — an escape is visible — and a raw string cannot hold one, as it cannot hold a lone CR. FF
  and VT move from `ForeignWhitespace` to the new `ControlCharacter`, and inside quotes from content
  to errors. Rejected: forbidding them only outside quotes, where they matter least — a value in
  quotes hides one as well as an unquoted one does.

- **The format becomes Ψ — Psi — once step 2 is done** (plan step 2d), everything renamed: the
  format, the loader object, the `Def*` types, `impl/def`, the sbt project, the package and the
  artifact, and this document and the plan with them. The user's call: just the letter, no
  acronym — the idea is the soul of an application, its configuration. Ψ is a valid identifier and
  no keyword, so the `defg` workaround of session 3 goes: `impl/psi`, sbt project `psi`, artifact
  `cfg-psi`, package `h8io.cfg.impl.psi`. A Greek letter also matches the user's `xi`. Three
  extensions are accepted — `.Ψ`, `.ψ` and `.psi`, the ASCII one for whoever cannot type the
  others — and a search for a file tries them in that order. Supersedes session 3's DEF and
  `.def`. Noted against it: PSI is also IntelliJ's Program Structure Interface, the API an IntelliJ
  plugin for the format would be written in, and `.psi` has minor binary uses (PrimalScript help,
  Psion audio, PhotoScape images) and ParaSail's interface files.

- **Markers are an open set, as directives are** (§5, §6). The user's idea: the loader is built
  with marker handlers as it is with directive handlers —
  `Psi.builder.withMarkers(markers: (String, MarkerProcessor)*).withDirectives(directives: (String,
  DirectiveProcessor)*)` — the format fixing only the syntax of a marker, and names may be longer
  than one character. `<`, `>`, `+` and the width become handlers registered by default, as
  `#include` is the built-in directive; a name not registered is an `UnknownMarker`, as now.
- **A marker handler works on the layout** — the user's pick of three stages. It gets the lines of
  the block with escapes and substitutions already read and opaque, and gives lines back; this is
  the stage the built-in markers work at. Rejected: the lexical stage, which could turn
  substitution off or change its delimiters (§13 item 4) but would expose the lexer, the common
  case being settled by `'''…'''` anyway; the value stage, transforming the finished string, which
  would run only after substitution (§8) and give a value a meaning, the job of tags and decoders at
  the higher level (session 6).
- **`withMarkers` and `withDirectives` replace the built-in set**, not add to it: the user's call. A
  loader that wants `<` or `#include` alongside its own handlers registers them again, so the
  built-in handlers have to be public values.
  **Superseded in session 12** for markers and **in session 13** for directives: both are added to
  the built-in ones, and nothing is replaced.

- **Markers are written as directives are called** (§5): a name — a run of symbols or a name by
  the key rule — and named arguments in parentheses, `name = literal`, markers separated by
  whitespace. The user's pick of three: a bare list with a number glued to a name (`"""2 +`)
  leaves no room for an argument that is not a number; `name=value` pairs would be a third syntax
  beside `key: value` and `arg = literal`. One call syntax for both extension points. With it,
  agreed with the user: symbols and letters do not mix in a name, so `<+` is one unknown marker,
  not two; arguments are named only, as a directive's (superseded below); the width is `width(n = N)`, any positive integer
  rather than one digit, and `"""2` and `"""+2` are gone (`"""width(n = 2) +`); markers apply left
  to right, so `"""+ <` and `"""< +` differ, Java's rule first when none sets the indent, and a
  second one setting it is an error. Any error in the markers reads the block as with no marker,
  as an unknown marker did in session 9; whitespace around them is reported and the markers kept.
  `/` stays out of the symbols for a comment after the markers. Implemented in 2c, the built-in
  set hard-coded until step 10.
- **A marker the user gave as an example: `replace(str1, str2)`**, replacing text in the lines of a
  block — at the layout stage, so in the written text only, escapes and substitutions being opaque
  there. Not to be implemented now. Written as the user wrote it, it takes positional arguments,
  which the entry below allows.
- **Positional arguments are allowed**, for markers and directives alike (§5, §6), the call syntax
  being one: the user's call, raised by `replace(str1, str2)`, which reads better than
  `replace(from = str1, to = str2)`. Scala's rules: positional arguments go to the parameters in
  the order the handler declares them, and come before the named ones; `width(2)` is
  `width(n = 2)`. New errors: `ExtraArgument`, a positional argument past the last parameter, and
  `PositionalAfterNamed`; an argument given by position and again by name is a
  `DuplicateArgument`. Supersedes "named only" above, the same session. A handler's parameter order
  becomes part of its public contract, which the API of step 10 has to carry.

- **Bidirectional formatting and invisible characters are errors anywhere, as control characters
  are, and `\uXXXX` still writes them** (§3): the user's call, on the question left open by the
  entry on control characters. The set — U+202A–U+202E, U+2066–U+2069, U+200B, U+2060–U+2064 and
  U+FEFF past the start — was drawn on the proposal: the explicit bidirectional formatting of
  Unicode's bidi algorithm, the characters CVE-2021-42574 ("Trojan Source") is about and Rust lints
  against, and the zero-width characters with no use in text. U+FEFF past the start moves from
  `ForeignWhitespace`, an error outside quotes only, to the new `InvisibleCharacter`. Not taken:
  the whole of Unicode's Cf category, which would forbid the joiners U+200C and U+200D that Persian,
  the Indic scripts and emoji sequences need, the marks of direction U+200E and U+200F that
  right-to-left text uses, the soft hyphen, and the tag characters of flag emoji.

Nothing is left open before #134 (step 2c) merges.

Settled for step 2d, the rename, later in session 10:

- **The loader object is `Psi`.** The user's call. Rejected: `PSI`, which follows `HOCON` and
  `YAML` but makes an acronym of a letter; `Ψ` itself, legal in Scala but hard to type at a call
  site.
- **On a case-insensitive file system `.Ψ` and `.ψ` are one extension**, and the spec says so: on
  Windows and macOS by default the two files cannot sit side by side, a search for `.Ψ` finds a
  `.ψ`, and the order between them holds only where case matters, as on Linux. Likewise a `.PSI`
  is found as `.psi` there. Agreed with the user; nothing in the format works around it.

### Session 11 — 2026-11-08

- **Indentation runs to the first token of a line, comments included, and a line is what newline
  tokens delimit** (§3). The user's pick of two, on a review that found `/*c*/<TAB>x` accepted while
  `<TAB>x` was an error. The lexer looked for a tab only among the leading spaces, and asked for the
  tab and the token to share a physical line, so `<TAB>/* c⏎*/ x` passed too, while a token after a
  comment spanning lines was already taken as continuing the line before it. One reading now
  throughout: a comment is whitespace, a newline inside one is no newline, and the first tab
  skipped before the first token of a line is reported if a token follows. Rejected: physical
  lines, which would make `x` in `a /* c⏎*/ x` start a line, against the rule that a newline inside
  a comment is none.

- **A directive handler takes its arguments as a `Node` and gives back a fragment, not a `Node`**
  (§6). The user's idea was a function from `Node` to `Node`; the input half is taken as proposed,
  the arguments as a `Node.IMap` keyed by parameter name, positional ones laid out by declared order
  as markers' are, every value a string — so `true` and `"true"` arrive the same and parsing them
  is the handler's job. The output half cannot be a `Node`: directives expand in phase 2 (§8), and
  what `#include` returns must keep its `${…}` unresolved until phase 4, which a `cfg` scalar,
  always a finished string, cannot hold. So a handler returns an opaque fragment of Psi's own tree,
  made either by parsing a source through its context or from a `Node`, which then holds no
  substitution and whose ids are dropped. Agreed with the user. Rejected: a `Node` out — it would
  take a substitution node into the `cfg` protocol that every loader and consumer would see, or
  directives expanded after substitution, so that an included file could not take part in it, or
  substitutions banned from what a directive returns, which rules out `#include` itself; and a
  `Node` in that is the value the directive applies to, which is the value stage rejected for
  markers in session 10.

### Session 12 — 2026-11-09

Spec gaps of plan step 3, the parser of the core syntax, settled with the user before coding it.

- **A value starts on the line of its `:` or `:=`, and the value of a tag on the tag's line** (§4):
  `a:⏎5` and `a: @t⏎{ … }` are errors in v1. The lexer allowed `:` at the end of a line, and nothing
  said whether the value could follow on the next one. Scala 3 allows it — after `=` at the end of a
  line a deeper-indented line opens a region holding one expression, which means the same as the
  expression written with or without braces — and the same reading would work for Psi: after `key:`
  a region holding one value, which is a map when it holds fields. But v1 promises that leading
  whitespace means nothing (§13), so `a:⏎5` would have to be legal at any indentation, and under
  the indentation syntax the same text at the key's column is an empty region. The user's pick of
  three: forbid it now, which a later version can lift without breaking a v1 file. Rejected:
  allowing it only on a deeper-indented line, Scala's rule, consistent but taking a decision that
  belongs to the parked indentation question (§13) and giving indentation a say in v1's validity;
  allowing it at any indentation, which breaks the promise of §13.
- **`key @t { … }` is an error with a hint, `key: @t { … }`** (§4). The block form is sugar with no
  place for a tag; a second spelling of a tagged block buys nothing.
- **Every syntax error says how to fix it, where there is a fix** (§4). The user's general rule,
  restated here; plan step 12 tests the texts.
- **Dotted keys have no whitespace or comment around their dots** (§4): `a . b` and `a. b` are
  errors, `Id.path` writing none.
- **A run of newlines with at most one `,` is one separator, wherever the `,` stands** (§3), so a
  leading comma, `a: 1⏎, b: 2`, separates as a trailing one does; an empty element is two `,` with no
  value between them. Rejected: making the leading comma an error, which would forbid a common style
  of lists for no gain.
- **Newlines after `[` or before `]` are no element** (§3), as after `{` and before `}`.
- **Until step 4, `${…}` and a directive are reported as not supported yet** by the parser (plan step
  3), as `"""` was by the lexer until step 2c.

Settled with the user after step 3 was coded (#150):

- **In a sequence a second word needs a separator** (§3): `[foo bar]` is a missing `,`, while
  `a: foo bar` in a block is a value to quote. Two elements are the likelier intent in a sequence;
  in a block a second word that does not start a field can only be part of the value.
- **Of two doc comments in a row the first is the error** (§3): it stands before a doc comment, not
  before a field, an element or a directive — what the rule already said, now spelled out.

Settled for plan step 4, the parser of substitutions and directives, later in session 12:

- **`#name` alone is a call with no arguments** (§4, §6). The user's call, against the proposal to
  require `#name()`: markers already drop empty parentheses (§5), and a pasted YAML or HOCON
  comment still fails loudly — `# text` has no name, and `#text` is an unknown directive.
- **The `(` touches the directive's name** (§6), as a marker's does; `#name (…)` and a `(` on the
  next line are errors with a hint.
- **Arguments are separated by a block's rules** (§3, §6), across lines included.
- **A parameter name may be written in backticks** (§6), the rule for every other name; markers
  took bare names only.
- **Triple-quoted strings are no argument literal in v1** (§6). Lifting it later breaks no file.
- **Integer arguments keep the markers' syntax for now** (§6): an optional `-`, no leading zeros,
  `-0` left as it is.
- **A duplicate argument is an error** (§6), as for markers: by name twice, and positional after
  named, in the parser; by position and by name where the parameters are known, in step 10.
- **Tags before a value-position directive are added to the produced node's** (§6), the union of
  a graft (§9).

- **A named argument is `name: literal`, not `name = literal`** (§4, §5, §6), for directives and
  markers alike: `#include(file: "x")`, `width(n: 2)`. The user's call, raised by the hint planned
  for `#x(a: 1)`: since session 11 a handler gets its arguments as a `Node.IMap`, so they are a map
  and are written as one, and Psi keeps one way to pair a name with a value. The `:` follows the
  rules of a field's (§3), whitespace after it; `=` is an error with a hint to write `:`.
  Supersedes session 2's "directive arguments keep `name = literal`, distinct from fields", whose
  reason — a call, as in Scala, not a block — the map of session 11 took away, and the `=` of
  session 10's marker syntax. Noted against it: a positional argument then looks like a field with
  no key, which a block does not allow; `#include("x")` and `width(2)` still read well. Not taken
  now: values other than literals, which `:` makes natural to add later, substitutions excepted.

- **The built-in markers `<`, `>`, `width` and `+` are reserved** (§5): always present, never
  replaced or removed, and `withMarkers` adds to them; registering one of their names is an error
  when the loader is built. The user's call: what a file's `"""<` means cannot depend on the loader
  reading it, and the layout rules — one marker that sets the indent, Java's rule first when none
  does — rely on knowing which markers set it. All four, `+` included, being layout the spec
  defines. Supersedes, for markers, session 10's "`withMarkers` and `withDirectives` replace the
  built-in set". Directives keep that rule: a loader for untrusted input has reason to leave out
  `#include`. **Superseded for directives in session 13**: `#include` is reserved too, and is left
  out by disabling it.

### Session 13 — 2026-11-11

- **A directive or marker name is registered once** (§5, §6): a second registration of a name is
  an error when the loader is built, for markers and directives alike, whether the two come in one
  call or in several. The user's call, for safety: a handler that silently shadows another, a
  sandboxed `#include` replaced by one that is not, is found by nobody.
- **`#include` is reserved, as the built-in markers are** (§6): `withDirectives` adds to it, and
  registering `include` is the second registration above. What `#include(file: "x")` means then
  does not depend on the loader reading the file.
- **`disableDirective(name)` turns a directive off for good** (§6, §11): a call of it is a
  `DisabledDirective`, not an `UnknownDirective`; registering a disabled name, before or after
  disabling it, is an error when the loader is built, so a handler cannot bring it back. A name
  with no handler can be disabled too. The user's call; it replaces session 10's way of leaving
  `#include` out, a `withDirectives` that replaced the built-in set. Both restrictions are for now:
  lifting either breaks no v1 file.

### Session 14 — 2026-11-11

Spec gaps of plan step 5, error recovery in the parser, settled with the user before coding it
(§11). §11 named one synchronisation point, a field's; the four questions were where the others
are, and what the tree holds where an item was skipped.

- **A sequence and a directive's arguments recover as a block does**: a broken element or argument
  is skipped to the next separator at its depth or to its closing bracket, and the rest are read.
  Rejected: dropping the whole sequence at its first broken element, which hides the errors in the
  others.
- **A directive call with an error in it is left out whole**, after every error in it is reported:
  a handler run on the arguments that survived would report a missing one that is not missing.
  The lexer's errors count, `#x(1.5)` and `#` alone included.
- **A closing bracket that closes another than the innermost open one ends the source**,
  as unbalanced brackets at the end of input already did. Proposed first: skip a bracket that no
  open one of its kind waits for, and go on. Implementing it showed that inside a container this
  makes a cascade — after `a: [1}` the next field is read as elements of the sequence — so only a
  bracket at the top level with nothing open is skipped. A bracket closing one further out, as in
  `x { a: [1, 2 } b: 3 }`, is reported as the inner one unclosed, as proposed: guessing where it was
  meant to close would make errors of its own after a wrong guess.
- **A skipped item leaves a hole, not a node** (§11). An error node would serve an IDE some day;
  adding it then breaks no file.
- **What the lexer finds in skipped text is forgotten, a fatal error excepted.** Not raised with
  the user: decided while implementing, from §11's rule against invented errors — the parser skips
  in a mode it guesses, and an error of that guess is no error of the source. Skipping switches
  modes as the parser does: in a block a key at the start of an item and a value after `:`, `:=`
  or `=`, a value in `[…]`, an argument in `(…)`; the broken item itself goes on in the mode the
  parser read last. Reading a block in value mode alone, as the first version did, takes `b{c:`
  for one word and ends the block at the wrong `}` (found in review of #154).

### Session 15 — 2026-11-11

Spec gaps of plan step 6, merging, settled with the user before coding it (§7, §10).

- **A merged map carries every place it was defined, the latest first**, in a new
  `MergedLocation(at, earlier)`; a map defined once keeps its `SourceLocation`, and `:=` starts the
  list anew. Rejected: the first site, which points at `reference.psi` when the application file
  is what the reader edits; the last alone, as `impl/yaml` gives, which hides the other sites. The
  order was the user's question: latest first, because `description` leads with where the reader
  goes first (§10), the head then agrees with `impl/yaml`'s one site, and a merge adds a site at the
  head. Definition order matched reading order only.
- **A map made by a dotted key stands at its key segment**: `a.b.c: 1` puts `a` at `a` and `b` at
  `b`, and a later `a { … }` adds its `{` to `a`'s sites.
- **Keys keep the order they first appeared in**, as in `impl/yaml`; a key replaced, by `:=` too,
  keeps its place. Rejected: moving a replaced key last, as removing it and adding it again would.
- **`null` merges as a scalar does**: it replaces a map with `:`, and a map replaces it. It follows
  from last-wins; §7 named scalars and sequences only.
- **Directives are expanded before merging**, as §8 orders; one reaching the merge is a bug of the
  loader. Not raised with the user: plan step 7 said a directive "reaching this phase", the wrap,
  is reported as not supported yet; it is reported before merging instead, in step 7, which then
  needs no place in the merged tree for a directive in statement position.

### Session 16 — 2026-11-11

Spec gaps of plan step 7, the node views and the loader, settled with the user before coding it.

- **The root of a load given no source is at `NoSourceLocation`**, `no source` (§10): the empty map
  was written nowhere, and §10 gives what is no file a location of its own instead of a fake file
  position. Rejected: `SourceLocation("", 1, 1)`, that fake position.
- **A source can be loaded from a string**, `Psi.parse(name, text)`, besides `Psi(urls*)`; `name` is
  the `<source>` of its locations (§10). It costs nothing and serves tests, the docs' mdoc snippets
  and configs built in code. What a relative `#include(file: …)` resolves against in such a source
  is for plan step 11, with `Path` and `File` inputs. Rejected: URLs only, as `impl/yaml` and
  `impl/hocon` have.
- **Until directives and substitutions land, each is a `NotSupportedYet`**, a temporary error gone
  before the release (plan step 14): a directive with the syntax errors, before merging (session
  15); a substitution, alone or in a concatenation, with the merge errors, as phase 4's would be.
- Taken as they stood, not raised as questions: the extension of a URL passed to the loader is not
  checked, as in `impl/yaml`; errors come in the order of the sources, each source's in the order
  found in the first batch and by line and column in the second, since merging and resolving go by
  key and would otherwise put a later source's error first (found in review of #157); an
  unreadable source does not keep the others' errors from being reported.

### Session 17 — 2026-11-11

Spec gaps of plan step 8, the renderer, settled with the user before coding it. Correctness does
not depend on them — a rendered tree parses and merges back to itself, its locations aside, which a
property test checks — only how the output reads does.

- **A field per line**, nested maps indented by two spaces, LF, a newline after the last field; for
  documents and `toString` alike. A one-line form, for error messages if they need one, is plan
  step 12's to decide. Rejected for now: one line, and both behind a switch.
- **Nested maps in the block form**, `key { … }`, `key {}` when empty; `key: @t { … }` with tags,
  which the block form has no place for (session 12). A chain of maps stays nested, never written
  as a dotted key. Rejected: `key: { … }` always, uniform but less idiomatic.
- **A scalar is quoted only when it has to be**: bare when the lexer reads it back alone as the same
  unquoted scalar, with no error, and it is not `null`; in `"…"` otherwise, with what may not stand
  raw escaped (§3) and a literal `${` as `\${`. Text over several lines is written with `\n`, not as
  `"""`, whose markers and indent buy only looks. A concatenation is always written in `"…"`.
  Rejected: quoting every scalar, simpler and harder to read.
- **Tags in alphabetical order**, the set having none; tags and keys written as `Id.path` writes a
  key, bare or in backticks.
- **`toString`** writes the root as a document, any other node as a value with its tags.
- **No public renderer yet.** `Psi.render` for a node of any backend would make a converter from
  HOCON or YAML almost for free, but it is API to keep from the first release, and its right shape
  shows only with use: after step 14, or with it. Rejected for step 8: making it public now.
- Not raised with the user, decided while implementing: a sequence stands on one line, `[a, b]`,
  unless it holds a map or a sequence, and then takes a line per element.

### Session 18 — 2026-11-12

Spec gaps of plan step 9, substitutions, settled with the user before coding it (§9).

- **`${?x}` as an element of a sequence drops the element** when it does not resolve, as it drops a
  field. Rejected: an error, and a `null` in its place.
- **`${?x}` as a part of a concatenation reads as the empty string.** Rejected: an error, and
  dropping the whole scalar.
- **A grafted node's id is its new position**, confirmed; the graft is made in the merged tree and
  the views give it its ids while wrapping.
- **System properties are a tree of strings, split at the dots; the environment is a map of
  strings, not split.** Looked up by the whole path each. Structure out of a string, splitting a
  value, is left to a directive if it is ever needed. The user's call, splitting the properties
  included. Proposed by me and not yet confirmed: the environment not split, and a property name
  that is both a value and a prefix losing the value to the map, as in HOCON.
- **Meta-fields (`${path^meta}`, §13 item 6) stay out of step 9**: they are after v1.
- Not raised with the user, decided while implementing: a cycle is reported once, at the reference
  that reaches a node still being resolved, naming the path that reference was written with; an
  optional part of a concatenation that reads as empty has no entry in its `ConcatenationLocation`,
  and a concatenation left with none is located at its own site alone; `null` in a concatenation is
  a non-scalar like a map or a sequence; a sequence is looked into by the position its elements end
  up at, so only the optional substitutions before the index are resolved to find it.

