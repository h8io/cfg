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

**Status:** implemented in `impl/psi` and published as `cfg-psi` from v0.1.0; the user's guide is
on the docs site, `docs/psi/`. This document stays the design notes:
§2 lists what is settled, §13 what is still open, and §14 logs how each decision was reached so a
later session does not reopen a question that was already argued through. Add to §14 rather than
silently editing §2.

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
- `impl/yaml` — closer: scalars as written, and explicit tags read through `Node.isResolved`, public
  since snakeyaml-engine 3.2 (when this was written, a `Composer` subclass reading
  `parser.peekEvent` recovered them). What YAML has no place for is the rest of §2: substitutions,
  directives, `:=`, and errors collected rather than thrown.

Writing the parser removes the impedance mismatch by construction: the scalar is the exact substring
of the source, the tag is present iff written, and `Location` can carry source, line and column
instead of a foreign description string. It also removes the last external dependency from the
loader path.

## 2. Decisions already taken

| Decision | Value |
|---|---|
| Syntax shape | Nested blocks with braces (HOCON/JSON-like) or significant indentation, mixed freely, as in Scala 3 (§4) |
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
| Location coordinates | Source by its full URL — as passed to the loader, or as an include argument resolves; line and column 1-based, column in code points (§10) |
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
  and the AST keeps it on the node it precedes from the start.
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

  **Outside quotes a `$` always opens a substitution** (session 25): `${path}`, or `$path` with
  no braces (§9). One that opens none — no path after it, as in `5$` or `$ x` — is an error with
  a hint to quote the value, and is read as text. Inside quotes nothing changes: only `${` opens a
  substitution, and a `$` alone is text, `"$5"`.
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
  marker (§4): `- 5` can never be a scalar, `-5` and `"- 5"` always are.
- **`null`** — the one reserved word. An unquoted scalar whose trimmed text is exactly `null`
  becomes `Node.INull`; `"null"` in quotes stays a scalar. Tags are kept: `@secret null` is an
  `INull` with `tag = Some("secret")`. `true`, `false` and every other word remain plain scalars.
  `null` is reserved only in value position — as a key it is an ordinary bare key.

## 4. Grammar sketch

```
document   = block-body
block      = "{" block-body "}"
block-body = { field | injection }
field      = key { "." key } ( ( ":" | ":=" ) value | block )
value      = [ tag ] ( scalar | block | seq | directive | substitution-expr ) | injection
seq        = "[" [ element { sep element } [ sep ] ] "]"
element    = value
injection  = ( substitution | directive ) "*"
sep        = "," | newline
tag        = "@" name
directive  = "#" name [ "(" [ arg { sep arg } [ sep ] ] ")" ]
arg        = [ name ":" ] literal
scalar     = null | bare-scalar | quoted-scalar | triple-quoted
```

Fields are separated by a newline or a `,` (§3). `key { … }` needs no `:`, and the `{` must be on the
same line as the key: `key` followed by `{` on the next line is an error. That keeps a key at the end
of a line free for the indentation syntax (below). A value after a `:`, `:=` or tag that ends its
line stands in a block indented deeper below it (Indentation); `a:⏎5`, with nothing deeper, is an
error. Inside `[…]`, where indentation means nothing, the value a tag precedes goes on the line of
the tag.

A dotted key (`server.tls.enabled: true`) is sugar for nested blocks. Its keys and dots touch, with
no whitespace or comment between them, as `Id.path` writes them: `a . b` and `a. b` are errors. The
block form `key { … }` means `key: { … }`, so it merges; replacing a block is written
`key := { … }`. It has no place for a tag: a tagged block is written `key: @t { … }`, and
`key @t { … }` is an error that says so.

Every syntax error names the fix where there is one, as these do: the error for `key @t { … }`
shows `key: @t { … }`, the one for `a:⏎5` says to indent the value or move it up.

### Indentation

Decided in session 28, implemented in session 32.

A source can be written in two ways, with braces or with significant indentation, as in Scala 3, and
the two mix freely, in one file and in one block. Indentation is always significant: there is no
switch to turn it off, as there is no warning for a switch to fall back to.

```
server:
  host: example.com
  ports:
    - 80
    - 443
  tls { enabled: true, protocols: [TLSv1.3] }
```

- **Where it counts:** at the top level of a source and inside `{…}`, as Scala 3 has it in braces.
  Inside a `{…}` the first field that starts a line sets the block's width, and no line of the block
  starts left of it. Inside `[…]` and a directive's `(…)` it means nothing, as in Scala 3's
  parentheses, until a `{` opens a block there. A `}` closes every indented block opened inside its
  own, and a line it starts may stand in any column, as in Scala 3: its indentation is not checked
  against the widths of the blocks it closes (session 31).
- **What opens a block:** `key:` or `key :=` at the end of a line, tags allowed after it
  (`routes: @ordered`), followed by a line indented deeper. The block is a map, a sequence, or one
  value. A field or an injection, `$b*` or `#d(…)*`, alone on its line makes it a map
  (`block-body`, §4, §9); `- ` elements make it a sequence (below), every line of it starting with
  one, `- $b*` an injection of a sequence's elements; and one value alone, a directive or a
  substitution included, is the value of the key, as YAML and Scala 3 both allow — `a:⏎  x`,
  `a:⏎  """…"""`, `a:⏎  [x, y]`, `a:⏎  $b`, `a:⏎  #env(name: X)`. A value means on the next line
  what it means on the line of its `:`, so `a:⏎  $b` is `a: $b`, a graft, and `a:⏎  $b*` is
  `a: $b*`. Two or more injections alone are a map, each finding or giving one (§9, session 42).
  Mixing fields and `- ` elements in one block is an error, and so is a second value, a value among
  fields or elements, or a substitution or a call with no `*` alone among fields or alone on its
  line among elements.
- **The tag after `key:` is the block's value's**: of the map, of the sequence, as here, or of
  the one value, which may not have a tag of its own written before it: `a: @s⏎  @t x` is
  `a: @s @t x`, two tags on one value, an error (§5).

  ```
  key: @tag
    - x
    - y
    - z
  ```

  It merges or replaces as `{…}` after the same `:` or `:=` would (§7). A line indented deeper than
  the one before it, where nothing opens a block, is an error: Psi has no continuation lines. A bare
  key alone on a line stays one.
- **Any deeper indent opens a block**, no step fixed per file, as in Scala 3. A line indented less
  must land exactly on the width of a block still open; one that does not is an error, and every
  following line is reported until one lands on such a width again (§11).
- **A sequence element is `- ` at the start of a line.** Unquoted scalars hold no whitespace (§3),
  so `- 5` is always an element holding `5`, while `-5` and `"- 5"` are scalars, and bare keys
  cannot start with `-`, so the marker is never a key. The elements sit deeper than their key, as a
  block does in Scala 3: YAML's `a:⏎- x`, with the marker in the key's column, is an error. A map
  element is written as in YAML, its first field on the marker's line and the rest aligned under that
  field's key; the column after `- ` is the element's width.

  After `- ` stands any value: a scalar, `[…]`, `{…}`, a substitution, a map written as above, or
  another element, `- - x` a sequence in a sequence, as in YAML. A value means after `- ` what it
  means after a key, one node: `- $a` grafts one element, and `- $a*` puts the elements of a
  sequence (session 42); a substitution alone on its line among the elements, with no `- `, is an
  error. An element is a map only when a field follows on the lines aligned under it, or written in
  braces, `- { $a* }`. A `-` with
  nothing after it on its line
  takes the value in a block below, deeper than the marker, as `- @t` does. A tag goes before the value, and a
  map element has no place for one before its first key: `- @t host: a` is an error, its hint
  showing `- @t { host: a }` or `- @t` with the map in a block below, deeper than the marker.

  ```
  servers:
    - host: a
      port: 1
    - host: b
      port: 2
  ```
- **Blank lines and comment-only lines** take no part in indentation. A tab in indentation is an
  error (§3). Inside `"""…"""` leading whitespace is content (§5).
- **No `end` markers** for now. A line shifted by exactly one level is still valid and changes
  parent, a mistake braces make loud and the width rule above catches only when the line is
  misaligned; Scala 3 answers it with optional `end` markers, which may come later.
- **`key:` at the end of a line with nothing deeper after it** is an error, as a missing value is:
  an empty map or sequence is written `{}` or `[]`.
- **The top level starts at column 1.** A source whose first line is indented is an error, the shift
  most likely an accident; Scala 3 takes the first line's width instead, which would hide it.
- **Separators** stay `,` and newline (§3): `a: 1, b: 2` on one line of an indented block is two
  fields, and `- ` elements are separated by their newlines.
- **`Render`, and so `toString`, writes indentation**: a map as fields in a block, a sequence as
  `- ` elements, braces left for the empty `{}` and `[]`, so that what a tree prints looks as a
  source is usually written.

Breaks a braced source whose lines do not keep to its blocks' widths: before 1.0, compatibility is
not kept.

A value on the line of its `:`, the rule of the braced syntax above, stays allowed: the next line is
only one more place for it.

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
marker handlers it knows (§14, session 19), each under the name it is registered by (§6, session
38). **A loader's own markers are a pipeline over the
block's finished value**: the first gets the string the block stands for — its layout applied, its
escapes read, its substitutions resolved — and each next one what the one before it gave; what the
last gives is the value. They run with the substitutions (§8, phase 4), so a block refers to a value
of the tree and still goes through them, and whatever grafts the block gets what they gave. A
handler declares its parameters as a directive's does (§6), gets its arguments as a `Node.IMap` and
the string, and gives back the new string or an error of its own, reported at the marker as its
failure;
the block then stands for its value as with none of the loader's markers. A value overridden at
launch (§9) replaces the block, its markers included. The built-in markers below shape the lines,
before any escape is read, and so come first: one of them after one of the loader's is an error.
**The
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
and was not given; a second marker that sets the indent; and a built-in marker after one of the
loader's. Markers that cannot be read at all — `"""2`, `"""<x`, `"""abc"""` —
are one error, where they stop fitting, and the rest of the line is not looked at. Whitespace after
the delimiter or after the last marker is an error as well, but the markers are applied.

`<` and `>` are a pair: the text pushed to the left edge, or left where it stands.

**Folding** under `+`, as YAML does under `>`: a newline between two lines with no indent becomes a
space, an empty line becomes a newline,
and a line indented deeper is not folded — neither it nor the newlines around it — so a list or a
code sample inside prose survives.

**An explicit width** is `width(n: N)`, N a positive integer in whatever literal it is written: an
argument has no type until a handler reads it (§6), and the built-in markers read theirs as any
handler does, so `width("2")` and `width('2')` are `width(2)` (session 46). As in YAML it counts
from the indent of the line holding the opening delimiter: that indent and then N whitespace characters are
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
`ISome.tag` holds a name only — the prefix and the quotes are syntax of the format, not part of
the tag: `@duration` gives `duration`, ``@`tag:yaml.org,2002:int` `` gives `tag:yaml.org,2002:int`,
so there is no need for `!!`. A tag may precede any value, a multi-line string included:
`query: @sql """…"""`.

`impl/yaml` follows the same rule and drops the leading `!` of a tag (`!postgres` gives `postgres`, `!!int` still `tag:yaml.org,2002:int`) — a breaking change to
`cfg-yaml`, made in step 2a of the plan. It merges YAML's `!foo` and `!<foo>`, two different tags in
YAML, into one name; the protocol accepts that.

Only tags written in the source reach `ISome.tag`; nothing is ever inferred, and there is no
non-specific tag that resolves to nothing. This matches the rule already settled for `impl/yaml`.

**A value has at most one tag** (session 37): `ISome.tag: Option[String]`, `None` when nothing was
written. Two tags written on one value are an error, `@a @b x`, and so is one written twice,
`@a @a x`: there is nothing to choose between, only a mistake. `impl/hocon` always gives `None`, and
`impl/yaml` the one tag YAML allows a node.

**Tags mean nothing at this level**: what a tag means is for the level above — whatever consumes
the tree, and the planned Psi schema. But where values meet, in a merge, a graft or a directive's
result, this level says which tag the result keeps, and it is **the one written last, the one
before it when none was written** — as values themselves go, the later over the earlier. A tag is
never inherited by a value that replaces another, only by a map merged into one (§7). So the
author of the config, not the application, settles which tag a node ends with: with several tags
united, a conflict made by merging two sources could only be refused by the application, and
fixed in the config by nothing short of `:=`.

**`@-name` is reserved** for removing an inherited tag (§7) and is an error in v1; with one tag to a
value, its name may turn out to be unneeded, which is left for when it is designed. It costs nothing:
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

**A directive and a substitution follow one rule of where they stand** (session 35): a call does
wherever it stands what a substitution standing there does, what the call gives in place of what
the substitution finds, and the other way round. What sets them apart is when: a call is expanded
before merging, a substitution resolved after it (§8), and only what follows from that differs —
a call may stand at the root, where an injection would refer to the tree it builds (§9). What is
merged with either at its key merges once it is known, as `:` merges (§7, session 36). The
one exception is a string: a substitution concatenates in a scalar, `jdbc:$host/db`, and a call
does not, `#` inside an unquoted scalar being ordinary.

A directive may appear in two positions:

- **statement position**, inside any block or at the top of a file, marked with a `*` as an
  injection is (session 42): `#include(file: "x")*` must produce a map, which is merged into the
  enclosing block at the point where it appears, so fields below it override it. Unmarked there it
  is an error, as `$a` is;
- **value position**, on the right of `:` or as a sequence element — it produces a single node,
  after a key, after a `- ` and as an element of `[…]`, whatever it gives. Marked, `- #d(…)*` and
  `[…, #d(…)*, …]` put the elements of a sequence it gives, and `a: #d(…)*` a node of what it gives,
  as a substitution with a `*` does (§9).

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

**The loader is built by a builder** (session 19): `Psi.builder.withDirective(name, handler)
.disableDirective(…).withSystemProperties(…).build`, the markers joining in plan step 10b and the
formatters in session 38 (§9). **The name a handler is called by is given where it is registered**
(session 38), `withDirective("yaml", Yaml)`, `withMarker(name, handler)`,
`withFormatter(name, handler)`, not held by the handler: the name is the syntax of the files, and the
files are the loader's, so the author of a handler does not dictate it, and one handler may be
registered under two names. A handler's own message therefore does not name it; the report puts
the name it was called by in front (§11). The built-in names stay fixed. `build`
gives every error of the configuration at once — a name registered twice, a disabled name
registered, a handler declaring a parameter twice — or the loader, with `apply(urls*)` and
`parse(name, text)` as `Psi` has; `Psi` itself is the loader with nothing added. The overrides of
§9 come from `sys.props`, read at every load, unless the builder is given a map, an empty one
turning them off, as a loader for input it does not trust would.

**A handler's contract** (sessions 11, 19). A handler declares its parameters, each by name and
whether it is required, and gets the arguments as a `Node.IMap` keyed by them, positional
arguments already laid out over the parameters in order, as markers' are (§5); so the map never
mixes the two. An argument to no parameter, given twice, or a required one missing, is reported as
for a marker, and the handler is not called. Every value is a scalar string, as everywhere in
`cfg`: `true` and `"true"`, `2` and `"2"` arrive the same, and reading one, and reporting an
argument it cannot read, is the handler's job. It gets as well a *context*: the call's location,
the name and URL of the source the call stands in (a source given as text has no URL), and a way to
parse another source; and, from session 39, `#include`'s search for a target — the parameters
`file`, `resource`, `url` and `optional`, and from the arguments given for them each candidate in
turn, a file or a resource with no extension with each of the extensions the handler names, opened
by a function the handler gives, the first it opens the result; a candidate known not to be there,
a file, not opened, and one not known to be, a URL from elsewhere, not there when opening it fails
as not found, the next then opened, so that a file its permissions deny, which Java reports as not
found too, is told from one that is not there; nothing for an optional target with no candidate
there, `#include`'s error otherwise — and a way to read a URL's bytes through the load's read limit
(§11).

What a handler gives back is an error of its own, reported as the directive's failure at the call, or
*nothing*, `Fragment.None` (session 34), or a *fragment*: Psi's own unexpanded tree, opaque to the handler, which it gets in one of
two ways:

- from its context, by parsing another source — what `#include` does. A `${…}` in that source stays
  unresolved until phase 4, so an included file may still refer to the file that included it (§8).
  A source that cannot be read is handed back to the handler to decide on, so that an optional
  include can give nothing; every other error in it, and a cycle, goes with the fragment, which
  comes with what was read, and is reported, with the errors of every source, where the fragment is
  used. A fragment is a value: a handler may keep one and give it back again, in the load it was
  made in or a later one, and its errors come back with it; one parsed and not given back reports
  nothing. An error found twice in one load, as through a source included twice, is reported once;
- from a `Node` it built — such a fragment holds no substitution. Its ids are dropped: where the
  fragment lands is the directive's position. Its locations are kept as origins behind the call,
  as a reference keeps them behind its site (§10): `app.psi:1:4 (#yaml from db.yaml:3:8)`.

In statement position, `#d(…)*`, the fragment must be a map, merged into the block where the call
stands as the fields it holds would be if written there; in value position any node, standing as one,
and a sequence putting its elements among the elements of a sequence when the call is marked,
`- #d(…)*` (§9). *Nothing* adds no field
in statement position and, in value position, drops the field or the element, as `${?…}` with
nothing to graft does (§9).

**Cycles, for every directive** (session 19). Every cycle runs through the context's way to parse
another source, which the framework keeps a stack of. Two sources are the same when their keys are:
a URL's is its URI, normalized, and for a `file:` one the file's real path, so `..` and symbolic
links do not hide a cycle; a source given as text is keyed by its name. Parsing a source already on
the stack is a `DirectiveCycle` at the call that closes it, naming the chain; and a stack 64 deep is
an error, as a backstop for a key that changes on every step (`?n=1`, `?n=2`, …), fixed in v1. A
source parsed twice not through itself, a diamond, is no cycle: it is parsed again each time.

**A tag before a directive in value position** replaces the tag of the node it produces, which keeps
its own when none is written, as a tag before a whole-value `${…}` does with the grafted node's (§9):
`a: @t #include(…)`. Before an injection among fields or elements, `#d(…)*`, a tag is an error, the
fields and elements keeping their own, and after a key, `a: @t #d(…)*`, it goes to the node made (§9).

`#include` is the only built-in, reserved and possibly disabled (above):

```
#include(file: "conf.d/extra")*             // relative to the including file's URL
#include(file: "local", optional: true)*    // missing file is not an error
#include(resource: "reference")*            // classpath, JVM only
#include(url: "https://…")*
```

Exactly one of `file` / `resource` / `url` is required, and `optional` is `true` or `false`, `false`
when not given (session 20):

- **`file`** is a path. A relative one resolves against the URL of the including source, whatever
  that URL is — a file, an entry of a jar, a web page — and never against the working directory,
  with one exception: a source given as text has no URL, and a relative path in it resolves against
  the directory the application runs in. A source it includes has a URL, so the paths in that one
  resolve against it again. An absolute path is a file of the file system. A path is read as the
  system the program runs on reads one, its last segment included (session 30); resolved against a
  URL, it is a URL's path, separated by `/`.
- **`resource`** is a name on the class path, read through the thread's context class loader, or
  the one that loaded Psi when the thread has none.
- **`url`** is an absolute URL, read as it is written.
- **Reading a URL times out** (session 24), whether `#include` reads it or the loader is given it:
  10 seconds to connect, 30 for each wait for the next bytes. A target that goes past either is
  unreadable, an error, optional or not; with no timeout, a server that never answers held the load
  forever. The read timeout bounds a wait, not the whole read; what bounds the whole read is the
  read limit (§11).
- **A `file` or a `resource` with no extension** — no `.` in its last segment — is looked for as
  `.Ψ`, `.ψ` and `.psi` in that order, and the first found is read; they are one format under
  three names, so nothing is merged. A `url` is never extended.
- **`optional: true` tolerates only a target that is not there**: no file, no resource, a page not
  found. A target that is there and cannot be read, or is not UTF-8, or does not parse, is an
  error, optional or not.
- **Its failures are errors of its own**, carried by the `DirectiveFailed` at the call: the target
  not found, unreadable, or not a path or URL at all, and the arguments not giving exactly one
  target or an `optional` that is neither `true` nor `false`.
- The source it reads is named in locations by the URL it resolves to, extension included (§10).

Include cycles are reported as an error rather than looping, as every directive's are (above).

**`#prop` and `#env`** (session 25) are built in too: a value set at launch, read where the call
stands, which is the only way one reaches a source (§9).

- **`#prop(name)`** gives the system property `name`; **`#env(name)`** the environment variable
  `name`. A name with a dot is quoted, `#prop("app.port")`, as any argument that is not a bare
  name. The value is a scalar, a string with no tags of its own, located at the call with the
  property or the variable behind it (§10).
- **`#prop` gives a map when no property has the name**: every property whose name starts with
  `name.`, the rest of its name split at its dots into keys, in order of their names —
  `#prop("java")` gives `java.version` as `version` and `java.vm.name` as `name` in a block `vm`.
  A property that is the name exactly wins over those under it; under it, one that has a value
  and properties under it too, so that it would be a scalar and a map at once, is an error. In
  statement position, `#prop("java")*`, the map's keys stand in the block, as a fragment's do. `#env` gives only a
  scalar: an environment variable's name has no structure.
- **A value not set is an error**, unless the call says otherwise: `optional: true` gives nothing,
  which drops the field — so that an earlier definition of the key stands, and `port: 80`
  followed by `port: #prop(port, optional: true)` is a default — and `default: …` gives the
  default, located where it is written. Both at once are an error, as is an `optional` that is
  neither `true` nor `false`.
- The values come from `sys.props`, read at every load, and `sys.env`, or from the maps the builder
  is given, `withSystemProperties(map)` and `withEnvironment(map)`. A loader for input it does not
  trust turns `#env` off, or gives it an empty map: the environment often holds secrets, and a
  value read is one a program may show.

**Importing YAML and HOCON** (session 39): a YAML or a HOCON file read where the call stands, its
tree what the call gives, by directives from two modules of their own, `cfg-psi-yaml` and
`cfg-psi-hocon`, each built on its format's loader and on `cfg-psi`. Psi itself depends on nothing
but `cfg`, and a program that reads YAML or HOCON alone takes no Psi with it.

- **One directive a format**, `#yaml(…)` and `#hocon(…)`, registered under a name of the loader's
  choosing (`withDirective("yaml", …)`), not `#include` choosing a reader by the extension.
- **The target is written as `#include`'s is** (above): exactly one of `file`, `resource` and `url`,
  a file relative to the source the call stands in, and `optional: true` for a target that is not
  there, with the same errors. The context gives every handler that resolution (below), so that no
  handler writes it again. A `file` or a `resource` with no extension is looked for as `.yaml` and
  then `.yml` by `#yaml`, as `.conf`, `.json` and `.properties` by `#hocon`, the syntaxes HOCON
  takes; the first found is read, where Typesafe Config's own search merges every one it finds.
- **What it gives is the file's whole tree**, a fragment made from a `Node`: no Psi substitution
  in it, its own resolved inside it; its locations behind the call's (§10), `app.psi:1:4 (#yaml from
  db.yaml:3:8)`; every value a string. Where it stands it does what any call does (above): its map's
  fields in statement position, `#yaml(file: "db")*`, any node in value position.
- **A YAML file may hold any node at its root** in an import, a sequence or a scalar as well as a
  mapping, where `cfg-yaml`'s own loader takes a mapping alone; the rest is that loader's: the core
  schema, merge keys, an alias that makes a node contain itself an error, 256 levels at most, and
  the tags written, which become Psi's. Its bytes are read through the load's read limit (§11), in
  any encoding YAML allows.
- **A HOCON file is resolved on its own**: its substitutions inside it, with no system property, no
  environment variable and no `reference.conf` — not `ConfigFactory.load`, which lays the system
  properties over a file and would set values at launch past `#prop` (§9, session 25). Typesafe
  Config reads it, and the files its `include`s name, itself, past the load's read limit; and it
  recurses with no limit (§13 item 10), so an import of HOCON is for trusted input only, the one
  exception to Psi's rule that no input overflows the stack. HOCON has no tags.
- **Its errors are one**: a file that cannot be read or parsed is the directive's failure at the
  call, its message what the reader said of it, line included.

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

**Tags in a merge** (session 37). When two maps merge, the result carries the tag of the later if
one is written there, and the earlier's otherwise. Being untagged is not a reason to drop a tag:

```
routes: @ordered { a: 1 }
routes { b: 2 }            // @ordered { a: 1, b: 2 }
routes: @sorted { c: 3 }   // @sorted { a: 1, b: 2, c: 3 }
```

A replaced value carries only its own tag; with `:=` that holds for maps too. So
`timeout: @duration 5s` followed by `timeout: 10s` leaves `timeout` untagged: a scalar is replaced,
not merged. `:=` is also the only way to drop an inherited tag in v1 — it drops the fields with it.
A finer tool is reserved, not designed: `@-name` (§5).

**Explicit replacement — `:=`.** A field written with `:=` discards whatever the key held before and
takes the new value as is, with its own tag:

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

**A merge with a substitution waits for it** (session 36; sessions 3 to 35 rejected it). Merging runs
in phase 3 and substitutions resolve in phase 4 (§8), so when one side of a `:` merge is a whole-value
`${…}`, phase 3 cannot know whether it is merging two maps or replacing one value with another. It
keeps both as they are, layers at the key, and merges them once resolved, as `:` merges anything:
maps merge, the later tag if written, and anything else replaces what was before it. A call merges so
already, expanded before merging (§6); a substitution now does the same, only later.

```
a: ${server}
a { port: 1 }      // the fields of server, port 1 over its port; port 1 alone when server is no map

b { port: 1 }
b: ${server}       // server's fields over port 1 when it is a map; server alone when it is not

c: ${x}
c: 5               // 5, whatever ${x} turns out to be, as before
```

The same holds for `${?x}`, which finding nothing puts nothing, so that `a: ${?x}` and `a.port: 1`
give `a { port: 1 }`, and `a: 1` then `a: ${?x}` gives `a: 1`, whatever stood before it (session 44), and for a block of injections alone (§9), calls in it included, and `a: $b*`:
what each injection finds or gives is known only once it is resolved. It holds at every depth of a recursive merge, and
`a.port: 1` is `a { port: 1 }` (§4). What a value that is no map replaces, layers before it
included, is not resolved, as what a value written over it is not; a lookup through the key, `${a.port}`,
merges its layers first. `:=` replaces whatever stood there, as it always did. A block written over
such a layer is kept as written and merged once the layer is resolved, field by field, as it would be
where the substitution stands: a `:=` in it replaces at any depth, and a value in it replaces what is
before it in turn.

There is deliberately no append operator. Appending to a sequence or concatenating values is a job for
an expression in value position — `a: ${b} + ${c}` — which can be added later (§13); replacement
cannot be expressed any other way.

## 8. Phase order

1. Parse each source URL into a syntax tree, directives unexpanded.
2. Expand directives depth-first. `#include` parses the referenced source recursively and merges it
   positionally. Cycle detection lives here.
3. Merge the input sources, last wins.
4. Resolve substitutions over the merged tree, format what a formatter is given, merge what each
   injection finds into the block it stands in, and run the loader's own markers over the blocks
   that have them (§5, §9). A formatter's arguments are taken in phase 1 (§9).
5. Wrap the result as `Node.IMap[Id.Root]`.

Nothing overrides the merged tree from outside (session 25): a value set at launch comes in where
a `#prop` or `#env` stands, in phase 2.

The consequence worth stating out loud: because 2 runs before 4, an included file may reference
values from the file that included it and vice versa — but an include target can never be computed
from a substitution.

**A program that names no source** (session 40) loads the class path's, as Typesafe Config's
`ConfigFactory.load()` does. `standard()` reads every `reference` on the class path, a library's
defaults, then over them the first `application` found, the program's own; `withReferences(urls*)`
reads the references, then the URLs it is given over them, for a program whose configuration is a
file — Psi reads no `config.file`, nothing set at launch reaching a load on its own (session 25).
`apply()` with no URL stays an empty tree, so that an empty list of files never reads the class path.

- `application` is looked for as `#include(resource: application)` looks: `.Ψ`, `.ψ` and `.psi`,
  the first found read, one file; with none, the references alone, and no error.
- `reference` is every copy of every extension on the class path: each library ships its own.
  The lowest go first, `.psi`, then `.ψ`, then `.Ψ`, so that `.Ψ` wins as it does in a search, and
  each in the order of the class path, which nothing fixes: two libraries setting one key are
  settled by the order of their jars, silently, as in Typesafe Config. It cannot be an error,
  libraries overriding each other's defaults on purpose.
- They are all sources of one load, merged in order and resolved once (phases 3 and 4): a
  reference refers to what the application sets, and the application to what a reference sets.
  A reference need not resolve on its own, as Typesafe Config requires of `reference.conf`; one
  that refers to a key only the application gives fails, without it, at the reference.
- A reference is read by the program's loader, its directives, its disabled ones and its limits:
  a library's own directive in its `reference` is unknown unless the program registers it.
  Resources are looked for with the thread's context class loader, as `#include`'s.

## 9. Substitutions

- `${path}` in place of a whole value grafts the referenced node, whatever it is — scalar, block or
  sequence — after a key, after a `- ` and as an element of `[…]`. Marked with a `*`, `$path*`, it
  is an injection (below); alone among fields or alone on its line among `- ` elements, unmarked, it
  is an error.
- **A path is written as `Id.path` renders one** (§3), right after `${` or `${?` and right up to
  `}`, with no whitespace: keys joined by dots, quoted in backticks when
  not bare, and indices into sequences in brackets — `${x.y[7].z}`, `${a[0][1]}`,
  ``${server.`odd key`[0]}``. An index is a decimal with no leading zeros, as `Id.path`
  prints it, so `[07]` is an error. An index past the end of a sequence resolves to
  nothing, as a missing key does: an error for `${…}`, an omitted field for `${?…}`.
- **An index after a `-` counts from the end** (session 41): `${hosts[-1]}` is the last element,
  `[-2]` the one before it. The one thing a path takes that `Id.path` never renders: the path is
  kept as written, and errors name it so, `hosts[-1]`, never the position it resolved to. `[-0]`
  and `[-01]` are errors, and `[-n]` before the first element is nothing, as past the last. Counting
  from the end needs every element: an injection or an optional substitution anywhere in the
  sequence is resolved, and the lookup counts as deep as that walk (§11). So an injection that needs
  the reference to know where the end is — the reference itself, `a: [x, $a[-1]*]`, or one it is
  found through, `a: [x, $b*]` with `b: $a[-1]` — is a cycle, reported as one of counting from the
  end. An element that is no injection is one element whatever it finds (session 42), so
  `a: [x, $a[-1]]` refers to itself, a cycle like any other. An index from 0 needs only the elements
  before it, and `a: [x, $a[0]]` stays `[x, x]`. No slices: a part of a sequence is a new value,
  which is value expressions (§13 item 1).
- **Outside quotes the braces may be left out**, `$path` and `$?path` meaning `${path}` and
  `${?path}` (session 25): `a: $b`, `url: jdbc:$host/db`. With no `}` to end it, the path takes all
  it can — a key, then `.key` and `[index]` as long as they follow — and stops before anything
  else, which is text again: `$host/db` is the path `host` and the text `/db`, but `$host-name`
  is the path `host-name`, a `-` being part of a key, and `$name.txt` the path `name.txt`. A `.`
  not followed by a key, and a `[` not followed by an index, end the path and are text. Braces end
  a path where it would go on: `${name}.txt`. Inside quotes the braces stay, a `$` alone being
  text (§3).
- A tag written before a whole-value `${…}` replaces the grafted node's own, which it keeps when
  none is written: with `b: @x { … }`, `a: @y ${b}` gives `a` the tag `y`, and `a: ${b}` the tag `x`.
  The rule is the one of a merge (§7), so a graft and a merge never disagree about tags.
- A concatenation builds a new scalar, which carries only the tag written before it: in
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
- **Nothing set at launch reaches the sources on its own** (session 25): no system property
  overrides a value, and the environment is not read, but where a `#prop` or an `#env` stands
  (§6). The config names what it takes from the launch, and nothing it holds is replaced behind
  its back.
- **`${path}` is looked up in the merged tree only.** A path the sources do not define does not
  resolve, whatever the system properties or the environment hold; a value from either has to be
  given a key in the sources first, by `#prop` or `#env`.
- **A grafted node takes its new position**, the id of the field it is grafted at, not that of the
  node it came from; its location keeps the origin (§10). The graft is made in the merged tree,
  before the nodes are wrapped, so `Node` never hands a container a foreign id.
- The graft keeps `null`: `b: ${a}` with `a: null` gives an `INull` at `b`, which is not the same
  as `${?a}` over a missing `a`.
- Cycles are an error naming the path. Self-reference (`a: ${a}" x"` reading the pre-merge value) is
  **not** supported in v1; it is reported as a cycle.

**Injections** (sessions 25, 26, 31, 34 and 42) put what a node holds where a node would stand: the
fields of a map among the fields of a block, the elements of a sequence among the elements of a
sequence. **An injection is marked with a `*`** right after the substitution (session 42): `$a*`,
`$?a*`, `${a*}`, `${?a*}`. Unmarked, a substitution is always one node, so that what a block or a
sequence holds can be read off the text, whatever the substitutions find: `[$b, $c]` has two
elements, and `[$b*, $c]` the elements of `b` and then `c`.

| Written | What it is |
|---|---|
| `a: $b` | a graft: `b`'s node whole, its tags included |
| `a: $b*` | a node of `b`'s contents: a map of its fields or a sequence of its elements |
| `$c*` alone among fields | `c`'s fields, `c` a map |
| `- $c*`, `[…, $c*, …]` | `c`'s elements, `c` a sequence |
| `- $c`, `[…, $c, …]` | one element |
| `$c` alone among fields | an error |
| `$c` or `$c*` alone on its line among `- ` elements | an error |

- **Among the fields of a block, as if the map's fields were written there**: `$a*`, alone among the
  fields, in braces or on a line of a block opened by indentation. What stands after the injection
  overrides it, what stands before it is overridden, and they merge as `:` merges (§7), maps deep
  and anything else replaced; a `:=` after it replaces. With `a { x: 1, y: 2 }`,
  `b { $a*, x: 3, z: 4 }` is `{ x: 3, y: 2, z: 4 }`. A map over a substitution is the error `:`
  gives there (§7), the substitution maybe no map.
- **What it finds is the map as it ends up**, merged from every source and resolved, as every
  substitution finds; what is merged into the block after it, from a later block or a later
  source, comes after it. Its fields keep where they came from behind the injection (§10), and
  their own tags; the map's tags are not brought along.
- **Among elements, after a `- ` or in `[…]`, the elements of the sequence it finds in its place**:
  with `a: [x, y]`, `b:⏎  - p⏎  - $a*⏎  - z` and `[p, $a*, z]` are `[p, x, y, z]`, and `- $a` and
  `[p, $a, z]` `[p, [x, y], z]`. Every line of a sequence written with `- ` starts with one: a
  substitution alone on its line there, marked or not, is an error.
- **After a key, a node of what it finds** (session 42): `a: $b*` is a map holding `b`'s fields when
  `b` is a map, a sequence holding its elements when it is a sequence, and an error when it is
  neither. It differs from the graft `a: $b` in two things: `b`'s tags are not brought along, a tag
  written before it, `a: @t $b*`, tagging the new node; and what it finds must be a map or a
  sequence. What is written elsewhere at `a` merges with either once it is resolved (§7, session 36).
  On the next line it means the same, `a:⏎  $b*` being `a: $b*` (§4).
- **Two or more alone in a block opened by indentation**, `a:⏎  $b*⏎  $c*`, stand among fields, a
  line with no `- ` being a field's place: the block is a map, and each must find one. A sequence of
  several is written with dashes, `a:⏎  - $b*⏎  - $c*`. With none finding anything the map is empty,
  as `a { $?b*, $?c* }` is; only one alone, `a: $?b*`, leaves its key out.
- **What it finds must be a map among fields and a sequence among elements**, an error naming where
  it is defined otherwise: the place says what the author meant, so a mismatch is the author's
  mistake.
- **No tag before it among fields or elements**: it would have no node to go to, the fields and
  elements keeping their own, so `@t $a*` alone among fields, `- @t $a*` and `[@t $a*]` are errors.
  After a key, `a: @t $b*`, the tag goes to the node it makes.
- **An optional injection finding nothing puts nothing**, among fields and among elements alike;
  after a key, `a: $?b*`, the key is left out, as an optional graft's is.
- **The `*` marks a substitution that is the whole item**, a value, an element or a line among
  fields. In a concatenation it is an error, `x$b*` and `$dir*.log`, with a hint to write `${dir}*`
  for the text: a glob written `$prefix*` must not turn into an injection unnoticed. In quotes it is
  text, as everything is. With braces the `*` goes inside, `${b*}`; `${b}*` is the substitution and
  the text `*`, a string after a key or a `- `, and an error alone among fields, where a scalar
  holding more than the substitution is one.
- **An index into a sequence counts the elements its injections put**, and resolves only those
  before the element it names: in `a: [x, $b*, $a[0]]`, `$a[0]` is `x`, and `$b` is not resolved to
  find it. An element that is no injection is one element, and is never resolved to count.
- **A directive injects as a substitution does, with the same `*`** (§6, session 42): `#include(…)*`
  among fields puts a map's fields, `- #d(…)*` and `[…, #d(…)*, …]` a sequence's elements, and
  `a: #d(…)*` a node of what it gives. Unmarked, a call is one node, and alone among fields or alone
  on its line among `- ` elements it is an error.
- **Not at the root, nor into the block it stands in**: every path goes through the root, so an
  injection among its fields would refer to the tree it builds, an error of its own; one that refers
  to the block it stands in, or one around it, is a cycle. A source included in a block puts its
  injections in that block. A field of the block may refer to another of it, the injections merged
  before either is resolved.
- It copies what it puts, and counts against the size limit as a graft does, and against the depth
  limit where its contents stand (§11).

**Formatters** (session 38) turn what a substitution finds into a string:
`${path; name(arguments)}`, as ST4's `<hosts; separator=",">`. With `hosts: [a, b]`,
`url: "jdbc:${hosts; join(",")}/db"` is `jdbc:a,b/db`. A formatter is a function of its arguments
giving a function from a node to a string, and the set of them is open, as directives and markers
are (§5, §6).

- **Written in braces only**: `${path; name}` or `${path; name(…)}`, `${?path; …}` optional. After
  the path come `;`, whitespace if any, and a call written as a marker is (§5): a name by the key
  rule, bare or in backticks, then, if it takes any, its arguments in parentheses touching it, as a
  directive's are (§6), literals only, `name: literal` or a literal alone. No whitespace before the
  `;` nor before the `}`, as none is allowed in a path; one line, as `${…}` always is. `$path`, with
  no braces, takes no formatter, its `;` being text.
- **One formatter in a substitution**, the user's call: a second `;` is an error. A chain, each
  next one given a scalar of what the one before gave, can be added later without breaking a file.
- **In quotes the call is read by its own rules**, the user's call, as Scala reads `s"${f("x")}"`:
  a `"` or a `}` inside an argument's literal closes neither the string around it nor the
  substitution. `"hosts: ${h; join(", ")}"` is one string, `${a; wrap("{", "}")}` one substitution.
  In `"""…"""` likewise; `'…'` and `'''…'''` substitute nothing (§5). An argument in `"…"` has its
  escapes, `${` an error, as a directive's (§6).
- **What it gives is a scalar**, the string, carrying only the tag written before it, as a
  concatenation's does: `a: @t ${hosts; join(",")}` is tagged `t`, `a: ${hosts; join(",")}` has none.
  After a key or a `- ` it is that scalar; in a concatenation, its text; in `[…]`, one element.
  **It never injects**: `${hosts*; join(",")}` is an error, a string being neither fields nor
  elements. What is
  merged at its key merges with it once it is resolved, as with any substitution (§7, session 36): a
  map there replaces it.
- **What it formats is anything the path finds**, scalar, `null`, map or sequence, resolved, as a
  graft would take it; the formatter decides what it can format, and an error of its own reports
  the rest. An optional substitution that finds nothing calls no formatter: the field is left out,
  or the concatenation reads it as the empty string, as without one.
- **A formatter is called in two steps**: with its arguments, when the source is read (phase 1,
  §8), so that a call the loader does not know, an argument to no parameter, given twice, or a
  required one missing, and the handler's refusal of its arguments, are reported with the errors of
  the syntax and the directives (§11); then the function it gave with the node, when the
  substitution is resolved (phase 4). A failure of either is the handler's error, shown under the
  formatter (§11) at its name.
- **`join` is the one built-in**, the user's choice to start with: `join(separator)`, the
  separator required, puts the elements of a sequence one after another with the separator between
  them, `""` for an empty sequence. An element that is no scalar, `null` included, and anything but
  a sequence, are its errors. The name is reserved, as the four built-in markers are (§5): every
  loader has it, and none can register, replace or disable it.

## 10. Locations

Every node carries its own `Location`, with `description` rendered as `<source>:<line>:<column>`.
This is the visible win over `impl/hocon`, where the description is whatever typesafe-config
composed.

**`<source>` is the full URL of the source**, so no two sources share a name and the reader can
open the file by it (session 21):

- a source passed to `Psi(urls*)` — its URL, as the caller gave it (`file:/etc/app/app.psi`);
- a source pulled in by `#include` — the URL its argument resolves to, extension included
  (`file:/etc/app/conf.d/extra.psi`, `jar:file:/…/lib.jar!/reference.psi`, `https://…`), not the
  argument as written. Two `common.psi` included from different directories are two names, and a
  resource found in a jar names the jar. It is the URL as resolved, not the real path a cycle is
  keyed by (§6): a path through a symbolic link is shown as the reader wrote it. The argument as
  written is still visible where it was written, at the `#include` call, whose own errors are
  located in the including source.

A source given as text has no URL and is named by the name its caller gave it. A `file` it includes
resolves against the working directory (§6), and is shown by the absolute URL it resolves to.

A source included twice not through itself, a diamond, is parsed twice (§6) and has one name; its
errors, alike in both parses, are reported once, as every error found twice in one load is (§6).

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
final case class MergedLocation(at: PsiLocation, earlier: ::[PsiLocation]) extends PsiLocation
// the root of a load given no source, an empty map written nowhere
case object NoSourceLocation extends PsiLocation
// a node a directive built from a `Node` and placed at `at`; `origin` is the node's own location
final case class DirectiveLocation(at: SourceLocation, directive: String, origin: Location) extends PsiLocation
// a value `#prop` read from a system property, not from a file
final case class SystemPropertyLocation(name: String) extends PsiLocation
// a value `#env` read from an environment variable
final case class EnvironmentLocation(name: String) extends PsiLocation
```

`origin` is a `Location`, not a `SourceLocation`, so chains fall out on their own: with
`a: ${b}`, `b: ${c}`, `c: 1`, the node at `a` is `ReferenceLocation(a-site, "b",
ReferenceLocation(b-site, "c", SourceLocation(c-site)))`. A system property and an environment
variable get a location of their own instead of a fake file position, behind the `#prop` or the
`#env` that read it: `app.psi:3:7 (#prop from system property db.port)`.

`description` puts the reference site first, because that is where the reader has to go:
`file:/etc/app/app.psi:12:9 (${db.url} from base.psi:3:7)`. The other cases follow the same shape:

| Location | `description` |
|---|---|
| `SourceLocation` | `app.psi:12:9` |
| `ReferenceLocation` | `app.psi:12:9 (${db.url} from base.psi:3:7)` |
| — in a chain | `app.psi:1:4 (${b} from app.psi:2:4 (${c} from app.psi:3:4))` |
| `ConcatenationLocation` | `app.psi:12:9 (${host} from base.psi:3:7, ${port} from system property db.port)` |
| `MergedLocation` | `app.psi:12:1 (also app.psi:7:1, base.psi:3:1)` |
| `NoSourceLocation` | `no source` |
| `DirectiveLocation` | `app.psi:1:4 (#yaml from db.yaml:3:8)` |
| `SystemPropertyLocation` | `system property db.url` |
| `EnvironmentLocation` | `environment variable HOME` |

A part of a concatenation is rendered without its own site, which shares the concatenation's line;
the part's `ReferenceLocation` still holds it.

**A description is cut off at 10 000 characters**, an ellipsis after it (session 27). A location
shares the locations it refers to, so its size is linear in the source; but a concatenation that
refers twice to one built the same way, and so on, describes in a length exponential in the chain's
— 297 bytes of source, 2.4 million characters — and the size limit (§11) counts what values hold,
not what locations describe. The beginning, the site the reader goes to first, is always there.
`toString` is cut off at the same length, and written the way a case class writes itself (session
29): the one a case class generates writes a location shared once for every path to it, 10.6
million characters for the same 297 bytes. The hash of a location is computed once, for the same
reason, and two locations are compared each pair of the locations under them once (session 30).

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
  node of its own standing for it; phases 3–6 never see the tree (below), and a directive's
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
- **Both batches go in the order of the sources**, each source's errors by line and column,
  and the sources directives parsed right after the one that parsed them, in the order they were
  parsed (session 20; the first batch since session 47, in the order found before).
- **Phases are gated.** If phases 1–2 (§8) produced any error, phases 3–6 do not run: substitutions
  over a tree with holes cut by recovery would report references to fields that exist but were lost
  in the skip. Syntax and directive errors come as one batch; substitution errors, if the first
  batch was empty, as the next.
- **Substitution errors report root causes only.** Every unresolved reference, cycle and
  concatenation of a non-scalar is reported, but a node that depends on an already failed node is
  dropped silently: with `a: ${missing}` and `b: ${a}`, only `missing` is named. An injection
  that fails fails its block, and nothing more is merged into it, but what follows it in the block
  still reports its own errors: the injections after it, each on its own, and the fields, merged
  apart from the block and, between two injections, from each other, what an injection would have
  brought being unknown; `b { $missing*, x := $other }` names both. A field that refers into the
  failed block fails with it, silently. An error found twice is reported once, as in every phase
  (§6): a source included in two blocks resolves twice.
- **Depth is limited, to 128 unless the loader is built with another**, `withDepthLimit(n)`, `n` at
  least 1 (session 22; 256 until session 45), so that no input overflows the stack: a
  `StackOverflowError` thrown while a class is being initialised leaves that class broken for the
  rest of the JVM, and every later load, of any config, fails. The limit is on:
  - **the depth of the tree as loaded**, not only as written: each `{`, `[` and `(` and each key
    after a dot of a dotted key is a level, and so is every level of what a directive places or a
    substitution grafts, counted from where it lands. It is reported, as `DepthExceeded`, where it
    is crossed:
    - in a source's text, which ends the source, as an unbalanced bracket does: what follows
      cannot be read without the levels it stands in. A source a directive parses is read a level
      below the call, so expanding includes inside blocks stays within the limit too; among fields
      as well, where its fields open no level in the tree (session 40): its parse takes a level's
      stack, and a chain of 63 includes among fields over 255 levels overflowed a stack of 1 MB;
    - where a directive places a fragment that does not fit there, a failure of the call, which
      contributes nothing: a fragment is a value, and a handler may give one back elsewhere, and
      one built from a `Node` has no depth but the handler's;
    - where a substitution grafts a value that does not fit there, a failure of the reference,
      whose node is dropped;
  - **the depth of resolution** (§9): the steps from the root to the node being resolved, each a
    level descended or a substitution followed. A substitution resolves what it finds whole, and
    the next may stand deep inside it, so a limit on the links of a chain alone bounds nothing:
    each link may descend up to the limit before the next. Going past it is reported at the
    innermost substitution being resolved, which there always is, the tree's own levels being
    within the limit; once, however many branches below it go past, and its node is dropped. A
    path, which may have any number of segments, is looked up and written step by step, not by
    recursion. **A substitution already resolved counts as deep as resolving it went** (session
    25): a node is resolved once and its value used again, and taking it as free let a chain
    written in order, each link finding the one before resolved, go past the limit unbounded,
    where the same chain in reverse failed at it. Whether a load goes past the limit does not
    depend on the order its keys are written in; where it is reported does, at the reference that
    reaches it first in the order resolved — or, reached where it stands in the tree with no
    substitution followed, at the node's own first reference.

  128 is far past any configuration written by hand, which seldom goes a dozen levels deep, and
  holds on a thread with the JVM's default stack, 1 MB on HotSpot, which virtual threads share as
  their limit, with more than half of it to spare: measured in session 45 on each phase, a few
  loads after the JVM started, where C1's frames make it worst, the costliest load at the limit, a
  chain of 128 substitutions each referring to a key written after it, takes about 420 KB, and a
  map of maps 128 levels deep about 370 KB. Every phase that walks the tree — parsing, merging,
  resolving, rendering — recurses by level, and each takes 250 to 420 KB at the limit; a dotted key
  and a chain of substitutions, flat as written, take next to nothing to parse. What a thread's
  stack holds only the application knows, hence the builder's setting: lower for a small stack,
  higher for a large one. The limit of 64 sources deep (§6) stays fixed.
- **What resolution adds to the tree is limited, to 1 000 000 nodes and 10 000 000 characters
  unless the loader is built with others**, `withSizeLimit(nodes, characters)`, each at least 0
  (session 25), so that a source of a few hundred bytes cannot take the heap: a graft copies the
  value it takes, and a concatenation builds a new scalar, so ten grafts of a sequence of ten
  grafts, six times over, make 10⁷ nodes, and a scalar concatenated with itself, 25 times over,
  makes 3·10⁸ characters. The depth limit bounds neither, the tree growing wide. Counted are the
  nodes grafts copy and the characters of the scalars concatenations build, each before it is
  built; the sources' own nodes and text are not, being the input, nor what a loader's marker or a
  loader's formatter builds, being a handler's. The nodes of a fragment a handler built from a
  `Node` are, before the substitutions, each counted as it is built and every use of the fragment
  counted again, a YAML alias as many times as it is used (session 39), and so are the nodes of a
  source a handler parsed, but on its first use in the load that parsed it: a fragment kept and
  given again, in that load or a later one, copies what was read once. The first call to go past
  the limit gives nothing, and its error, `FragmentTooLarge`, is the load's, as the read limit's
  is, so that a handler that drops the fragment it was found in does not drop it; grafts count on
  from there. The built-in `join` (§9) is
  counted as a concatenation, before it is built, and so is a concatenation holding what a
  formatter gave, once the formatter has given it. The first graft or concatenation to go past either
  limit is reported, as `ResolutionTooLarge` or `ConcatenationTooLong`, at its reference or its
  concatenation; nothing is built after it, and the load fails.
- **Reading the sources of a load is limited, to 10 000 000 bytes and 60 seconds unless the
  loader is built with others**, `withReadLimit(bytes, time)`, the bytes at least 0 and the time
  not negative (session 25). The limit is the load's, not a source's: every read is counted, a
  source read twice twice, since an include is read and parsed again wherever it is called, and
  seven files of 300 bytes, each including the one before ten times, made 10⁶ reads and ran out
  of a 1 GB heap; a limit on each source bounds none of them. Counted are the bytes read: of a
  file, of the entry of a local jar, of the whole of a jar fetched from elsewhere; a source given
  as text is not read, and not counted. The time runs from the start of the load and is looked at
  before each connection and each wait, which it cuts to what is left; a connection takes its
  timeouts once, when it is made (measured: a read timeout set later is not applied), so a wait
  already begun runs its course and the time may be outrun by one wait, the read timeout at most;
  the read then fails all the same, the time looked at once more after the wait that found the end.
  A source that blocks without a timeout, a named pipe, the time does not stop. The first read to
  go past either is reported, as `ReadLimitExceeded` or `ReadTimeExceeded`, at the source being
  read; every read after it gives nothing and is not reported, and the load fails. The error is
  the load's, not the fragment's, so that a directive of the program's own that drops the fragment
  does not drop it: to such a directive, the read gives an empty fragment.
- **What a handler reads by itself is not counted** (session 39): the limit counts what goes
  through the context, every Psi source and what a handler reads by its context's way to read a URL,
  as `#yaml` does. `#hocon` leaves the reading to Typesafe Config, its file and the files it
  includes past the limit, as any handler reading by itself does.

**Messages and the report** (session 21):

- **Every `PsiError` has a `message`**, a `def` each error computes from its fields, never a stored
  text: errors stay compared, as for being reported once (§6), by their type and fields.
  `PsiErrors.message` is the report, and so is its `toString`. `cfg`'s `CfgError` stays a marker,
  with no message: the other loaders report by exceptions and have no error types of their own yet.
- **A hint is a field of its own**, `hint: Option[String]`, a `def` as `message` is: what to do,
  apart from what is wrong, so that a tool can show it on its own. The report puts it on the next
  line, under the message, as `hint: …`. `DirectiveError` and `MarkerError` get one too, `None`
  unless a handler gives one; a handler's hint is shown under its directive's or marker's.
- **A message names what was found.** An error that does not carry the text it is about gets it as
  a field. Text longer than about 20 characters is cut with `…`; an invisible or control character
  is shown by its code point and name, `U+00A0 NO-BREAK SPACE`. Found text, and a source's name,
  are made visible first, as `Id.path` writes a key: a control, format or line-breaking character
  and any space but U+0020 as an escape, so an error stays on its lines. A key a hint offers is
  written as `Id.path` writes it, bare or in backticks with its escapes.
- **The report groups errors by source**, in the order above: a line with the source's name, then
  one line per error, `<line>:<column>: <message>`. It opens with a count, `5 errors in 2 sources`.
  Every error stands at a position in a source (session 36): the one at a composite location (§10),
  a merge with a substitution, is gone.
- **No source excerpt in v1**, no line of the source with a caret under the column: the report
  would need the text of every source kept until it is built, and an error that came with a fragment
  from an earlier load has none. Line and column are exact; an excerpt can be added later without
  changing anything else.
- **An error that ends its source says so**, as `the rest of this source is not read`; otherwise a
  reader takes the errors listed as all there are.
- **A handler's error is shown under its directive, marker or formatter**, `#include: file:/…/x.psi
  is not found`: the name it was called by, then the handler's message.
- **Messages are not a contract.** Tests assert their exact texts, but a program reads an error by
  its type and fields; a text may change in any release.

## 12. Implementation notes

- No external dependencies, like `cfg` and `impl/hocon`.
- Unlike the hocon and yaml backends, which wrap a foreign structure lazily, this module owns its
  data: phase 6 builds immutable `Map`/`Vector` structures once, and `MapImpl`/`SeqImpl` are thin
  views over them.
- Cross-built for 2.13 and 3.9, from one source tree in Scala 2 style that Scala 3 accepts.
- Warnings are fatal on every version (`-Xfatal-warnings` on 2.x, `-Werror` on 3), so exhaustiveness
  in the parser's pattern matches is enforced rather than merely intended.
- **Compatibility, while the version is 0.x** (session 22). The errors are case classes, for
  pattern matching: a field added to one breaks its `apply`, `unapply` and `copy`, and an error
  added to the sealed `PsiError` breaks an exhaustive match. Both are accepted until 1.0; the texts
  are no contract at any version (§11). The traits a user implements, `Directive`, `Marker`,
  `DirectiveContext` and the handlers' errors, get new members only with an implementation, which
  breaks no one. MiMa comes with 1.0, not before (session 23): while 0.x promises no compatibility,
  a check that has to fail on every break the policy allows only gets in the way. Plan step 13
  made private what was public by accident.

## 13. Open questions

1. **Value expressions**, e.g. `${b} + ${c}` for sequence concatenation — the replacement for an
   append operator (§7). Not in v1. Whitespace never joins values (§3), so `${b} + ${c}` is an error
   today and the syntax stays free. **Parked** by the user as hard from every angle — do not reopen
   it until it is raised again. Injections (§9, sessions 25, 26, 31, 34 and 42) do the jobs that came
   first: `[$b*, $c*]` concatenates sequences, and `b { $a*, x: 3 }` extends a map.
2. ~~**Indentation as an alternative to braces**~~ — settled in session 28 as in Scala 3 (§4,
   Indentation), implemented in session 32.
3. ~~**Multi-line strings**~~ — closed in session 7: reopened by the user and settled point by
   point (§5, §14).
4. ~~**Templates in the direction of StringTemplate 4.**~~ — closed in session 45, each of its points
   settled or declined. Raised by the user in session 7: substitutions stay as they are in v1, but
   the way forward was to look at ST4's ideas:
   - ~~**Delimiters chosen per template.**~~ — declined by the user in session 45: a syntax of
     substitution for each block would make every reader learn which one is in force where. ST4
     lets a template use `<…>` or `$…$`, whichever does not clash with its content. The case for
     it: a shell script, a Helm template or a Spring config in a `"""` block is full of `${`, each
     of which needs `\${` in v1 — the common case is settled by `'''…'''`, which substitutes
     nothing (§5). A marker on the opening quotes could turn substitution off or change its syntax;
     markers do not combine in v1 (§5), and this one would have to combine with the indent markers.
     Turning substitution off is what `'''…'''` does; changing its syntax stays out.
   - ~~**A template applied to a collection with a separator**~~, `<hosts; separator=",">` — as a
     string, settled in session 38 by the formatter `join` (§9). Joining values into a node, a
     sequence out of two, stays with value expressions (item 1), parked.
   - ~~**Options inside `${…}`**~~ — settled in session 38 as formatters (§9),
     `${hosts; join(",")}`: a call of a handler registered with the loader, not a fixed set of
     options.

5. ~~**Cycles, for every directive.**~~ — closed in session 19, settled in §6.

6. **Meta-fields.** Raised by the user in session 15, **after v1**: a meta-field `id`, the key or
   index of an object, to start with. **Parked** by the user in session 45, for no set time: the
   need that raised it is no longer remembered — do not reopen it until it is raised again, with a
   case it serves. What follows is kept for that day.

   The protocol already gives every node its `Id`; what a meta-field adds is the value inside the
   config text, where it makes templates of substitutions:

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
     `${workers^size}`, `${db^tag}`, `${db.url^source}`. The user had no use for `${field^id}` yet.
   - With the options of item 4, presumably node, meta-field, then options: `${a^size; …}`.
7. ~~**`#define`: a key with no value, expected at launch.**~~ — closed in session 25: `#prop` and
   `#env` stand where a value set at launch is expected, which is what `#define` was to name, and
   their `optional` and `default` say what happens when it is not set (§6).
8. ~~**Importing YAML and HOCON.**~~ — settled in session 39 (§6): directives `#yaml` and `#hocon`
   from modules of their own, `cfg-psi-yaml` and `cfg-psi-hocon`. Raised by the user in session 18.
9. ~~**Two included sources named alike.**~~ — closed in session 21: an included source is named by
   the URL it resolves to (§10). Raised by Codex's review of plan step 11: named as its argument
   was written, two files that two sources each include as `common.psi` gave errors located alike,
   and the second batch (§11), ordered by the name, put both after the first includer.

10. ~~**`impl/hocon` and `impl/yaml` have no depth limit.**~~ — closed in session 25: `impl/yaml`
    limits a file to 256 levels, aliases followed, and `impl/hocon` is documented for trusted input
    only. Found in session 22, on the default 1 MB stack: typesafe-config parses and resolves by
    recursion with no limit of its own, and overflows past about 6 000 levels of `{ … }`, 1 000 of
    `[ … ]`, 3 000 keys of a dotted key or 6 700 links of a chain; snakeyaml-engine 3.2 has no
    nesting limit either, unlike SnakeYAML's `nestingDepthLimit`, and overflows somewhere past
    3 000 levels of `[ … ]` and before 1 000 of block mappings.

11. ~~**Merging over a substitution or a block of injections alone**~~ — settled in session 36: a
    merge with either waits until it is resolved, and merges as `:` does (§7).

12. ~~**Stack headroom.**~~ — closed in session 45 by a lower default limit, 128 (§11). Raised by
    the user in session 40: the depth limit kept no input from overflowing a stack of 1 MB, but
    only from a stack nearly empty when the load starts. Measured in session 43 on a map of maps
    256 levels deep, a few loads after the JVM started, where C1's frames make it worst: 880 KB,
    about 620 KB once the parser's frames were slimmed. Measured in session 45 on each phase, the
    parser was not alone: merging, resolving and rendering each took 450 to 550 KB at 256, and a
    chain of substitutions written in reverse 710 KB, all of it resolving. At 128 the costliest
    load takes about 420 KB. The ways considered:
    - **Fewer and smaller frames** on the path a nested value recurses through (done in session 43):
      `statement`, `indented` off it, the state of `items` and of a block opened by indentation in
      fields rather than locals.
    - **A lower default limit** (done in session 45): one line, every phase relieved at once, a
      source deeper than it needing `withDepthLimit`.
    - **An explicit stack** in place of recursion: depth would cost no stack at all, but only in
      the phases rewritten, and every phase that walks the tree takes as much as the parser; the
      recursive descent, its recovery from errors and indentation, `Merge`, `Resolve` and `Render`
      all rewritten, for a depth no configuration reaches. Not needed, by the user's call.
    - **`scala.util.control.TailCalls`**, the user's suggestion, as that explicit stack without one
      of our own: depth goes to the heap. The catch: a `try`/`finally` or `catch` around a recursive
      call no longer wraps it, the deferred call running after the `finally` — the parser's `dots`,
      `levels`, `open` and `layouts` are restored that way, and `Broken` and `Halt` caught — so those
      go into continuations; and an allocation every step.
    - **A thread of its own** for the load, its stack sized by the limit, opt-in on the builder: the
      only way the promise holds whatever the caller has used, but a handler then runs in another
      thread, `ThreadLocal`s other than inheritable ones not seen, and an interrupt of the caller
      not reaching it.

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
  checked, as in `impl/yaml`; errors come in the order of the sources, each source's by line and column
  (the first batch in the order found until session 47), since merging and resolving go by
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
- **System properties override the sources** — superseded in session 25 by `#prop` and `#env` —
  only the values the sources define (§8, §9): the
  user's call — they exist to change at launch what the file says. **The environment is not read at
  all**, and a `${…}` is looked up in the overridden tree only: the user's call, after rejecting each
  way of naming a variable by a path. The user's reason besides: the environment often holds
  secrets, and a config able to read any variable could hand one out — into a value, a rendered
  tree or an error message. Supersedes, within this session: an environment not split,
  reached by a one-key path (proposed by me, confirmed, then rejected); a variable named from the
  path, `db.url` → `DB_URL` (proposed by me, taken, then rejected); system properties, split at the
  dots into a tree, and the environment as places a `${…}` falls back to, after the tree, as in
  HOCON. Rejected as well: overriding from every property, flooding the root with `java.*` as
  HOCON's `ConfigFactory.load` does.
- Mine, not raised: only a value that is not a container is overridden, at a path through maps only
  — a string cannot stand for a map or a sequence, and an index has no name as a property; the tags
  written in the source are kept.
- **Meta-fields (`${path^meta}`, §13 item 6) stay out of step 9**: they are after v1.
- Not raised with the user, decided while implementing: a cycle is reported once, at the reference
  that reaches a node still being resolved, naming the path that reference was written with; an
  optional part of a concatenation that reads as empty has no entry in its `ConcatenationLocation`,
  and a concatenation left with none is located at its own site alone; `null` in a concatenation is
  a non-scalar like a map or a sequence; a sequence is looked into by the position its elements end
  up at, so only the optional substitutions before the index are resolved to find it.

### Session 19 — 2026-11-12

Spec gaps of plan step 10, the directive and marker framework, settled with the user before
coding it (§6, §10, §13 item 5). The step is split: 10a the directives, 10b the markers as handlers,
whose API is settled when 10b starts. All proposed by me and taken by the user, the last after a
question of its own.

- **A builder**, `Psi.builder…build`, whose `build` gives every error of the configuration at once
  as a value rather than throwing, as the rest of the module does. Rejected: throwing on the first.
- **The launch overrides of §9 are the builder's to set**: `sys.props` at every load by default,
  a map given instead, an empty one turning them off, for input that is not trusted as
  `disableDirective("include")` is for.
- **A parameter is declared required or not**, so the framework reports a missing one, as it does
  for markers, before the handler runs.
- **A handler gives back a message, nothing, or a fragment.** The message becomes the framework's
  own error at the call, so the module's error type stays closed. Nothing drops a value-position
  field or element, as `${?…}` does, and adds nothing in statement position: an optional include
  of a missing file.
- **A fragment has no surface**: made by the context's `parse`, or from a `Node`. The context gives
  the call's location, the including source's name and URL, and `parse`; a source that cannot be
  read is handed back to the handler, any other error in it is reported with every source's.
- **Cycles** (§13 item 5, closed): a source's key is its normalized URI, a file's real path, a
  text source's name; the error at the call closing the cycle names the chain; a stack 64 deep is
  an error, the limit fixed in v1; a diamond is parsed again each time, no cache.
- **A fragment carries its errors**, reported where it is used, each once per load — after Codex's
  review of #161, which found a fragment kept by a handler and given back in a later load losing
  them, since they stayed with the load that made it. Rejected: tying a fragment to its load and
  checking that, which forbids a handler's cache rather than making it work.
- **Nodes of a fragment built from a `Node` keep their location as an origin behind the call**,
  `DirectiveLocation(at, directive, origin)`, as a reference does (§10) — the user's pick, with
  importing YAML (§13 item 8) in mind. Rejected: the call's location alone, simpler but losing the
  line in the imported file. `MergedLocation` takes any location for it, since a block a
  directive built merges as one written does.

Plan step 10b, the markers as handlers, settled with the user before coding it (§5).

- **A loader's markers take the block's finished string and give a string**, in a pipeline left to
  right — the user's call, over my proposal of lines of pieces at the layout stage (written text,
  escapes, opaque substitutions), and supersedes session 10's "a handler maps the lines of a block
  to lines". The first gets the value after the layout, the escapes and the substitutions: the
  user's call, the markers running after phase 5 rather than being refused on a block with a
  `${…}`, or seeing it as text. The built-in markers stay at the layout stage, and one of them
  after a loader's marker is an error, read as with no marker.
- **A failure is an error of the handler's own type**, a `MarkerError`, which the framework's error
  carries whole — the user's call, over a message as a string; the directives' `DirectiveError`
  follows it, replacing step 10a's string, so the two APIs stay alike.
- Mine, taken by the user: a handler declares parameters as a directive's, its argument errors
  are a directive's; a failure is reported at the marker, the block then standing for
  its value as with none of the loader's markers. A marker name is checked when the loader is
  built — a run of symbols or a name by the key rule, as the syntax reads one — and so is one
  registered twice, a built-in included. Markers and directives have names of their own: a marker
  `include` is no `#include`. The built-in markers stay internal, as `#include` does.
- Mine, not raised: a value overridden at launch replaces the block and its markers with it; a
  marker's failure fails no node that depends on the block, the block having a value without it.

### Session 20 — 2026-11-12

Spec gaps of plan step 11, `#include`, settled with the user before coding it (§6, §11). All
proposed by me and taken by the user but one.

- **A relative `file` in a source given as text resolves against the working directory** — the
  user's call, over my proposal of an error, there being no URL to resolve it against. A narrow
  exception to "never the working directory": a source read from a URL resolves against its URL,
  and so does every source a text source includes, so only the first step depends on where the
  application runs.
- **A `file` or `resource` with no extension is looked for as `.Ψ`, `.ψ`, `.psi`, the first found
  read**: one format under three names, unlike HOCON's three formats merged. A `url` is read as
  written.
- **`resource` reads through the thread's context class loader**, or Psi's own, as HOCON does; no
  builder option until one is needed.
- **`optional` tolerates "not found" only**, as plan step 11 proposed.
- **`#include`'s failures are `DirectiveError`s of its own**, public, inside `DirectiveFailed`.
- **`url` takes any scheme**: a loader for input it does not trust disables `#include` whole (§6).
- **Errors located in included sources sort right after the including source's** in the second
  batch, in the order the sources were parsed; before, they sorted after every source the loader
  was given.
- **`optional` asks whether a target is there before reading it** — after Codex's review of #163:
  Java throws one `FileNotFoundException` for a missing file and for one the permissions deny, so an
  optional include skipped an unreadable file, and the extension search went on past one. A file is
  there when the file system says so, a jar entry when its jar does; only for any other URL does a
  failure to read mean "not found", a page not found.
- **Two included sources named alike sort as one**, found by the same review: §13 item 9, unsolved.


### Session 21 — 2026-11-13

Spec gaps of plan step 12, error messages, settled with the user before coding it.

- **An included source is named by the full URL it resolves to** (§10), the user's proposal; this
  reverses session 4's "the argument as written". Two files included as `common.psi` from
  different directories now have different names, so a reader can tell their errors apart and the
  second batch (§11) puts each after its own includer: §13 item 9, closed. Session 4's objections
  weigh less than the ambiguity: the URL is long, but it is what a reader needs to open the file;
  the argument as written is still shown at the `#include` call. Shown is the URL as resolved, not
  the real path a cycle is keyed by, so a symbolic link stays as the reader wrote it. Rejected: the
  include chain beside the written name (long, and still ambiguous for a diamond's two parses); a
  source identity kept apart from the name shown (fixes the order, not what the reader sees).
- **Messages and the report** (§11), proposed by me and taken by the user: what was found is named,
  carried as a field where an error did not carry it; the report groups by source under a count; no
  source excerpt in v1, there being no text kept for a fragment from an earlier load; an error that
  ends its source says so; a handler's error is shown under its directive's or marker's name;
  messages are no contract, types and fields are.
- **A message is a `def` of each `PsiError`, computed from its fields** (§11): the user's condition,
  that the text is not stored but made by each error. `CfgError` stays as it is.
- **A hint is a field of its own, `hint: Option[String]`** (§11), on `PsiError`, `DirectiveError`
  and `MarkerError` alike; proposed by me and taken by the user. Rejected: the hint as part of the
  message, which a tool could not show apart.
- **A source a directive reads from a URL is named by that URL, whatever the directive**: the
  context's `parse(url)` takes no name, so no handler can name a source otherwise. Follows from the
  rule for `#include`; a source given as text keeps the name its handler gives it.

### Session 22 — 2026-11-13

Spec gaps of plan step 13, hardening, settled with the user before coding it; all proposed by me
and taken by the user.

- **Depth limited to 256, nesting and chains of substitutions** (§11). Measured first: with the
  default 1 MB stack, nesting overflowed past 850 levels of `{ … }`, 3 000 of `[ … ]` or of a dotted
  key, and chains past 2 000 links; and a `StackOverflowError` in a class's initialisation made every
  later load in the JVM fail with `NoClassDefFoundError`. Rejected: phases without recursion, a
  rewrite of the whole module for depths no config has; catching `StackOverflowError`, which cannot
  undo a class left broken. 256 over Jackson's 1 000 and SnakeYAML's 50: a margin of three on the
  default stack, and a config has no use for more.
- **Resolution is limited by its depth, levels and substitutions together, not by the links of a
  chain** (§11), found by me while reading `Resolve` and taken by the user: each link resolves what
  it finds whole, so 128 links each 250 levels deep passed a limit on links and on grafted depth
  alike and still nested 32 000 calls. A config's resolution is a few dozen steps deep.
- **Past the nesting limit the source ends; past the chain limit the reference fails, once** (§11).
- **The limit is on the tree as loaded, not as written** (§11), found by me while starting the code
  and taken by the user: an include inside a block lands at the depth of its call, 64 sources deep
  at most, and a substitution grafts a subtree at the depth of its reference, chains multiplying
  it, so a limit on each source's text alone bounds neither. Each place crossing it reports one
  `DepthExceeded`: the source's text ends the source, a fragment placed fails its call, a value
  grafted fails its reference.
- **The limit is the builder's, `withDepthLimit(n)`, 256 by default**: one setting for both, both
  running on the same stack; `n < 1` a `PsiBuildError`. Asked by the user; my proposal had it fixed,
  as 64 sources deep is, which stays so.
- **Realistic configs**: `impl/hocon`'s test resources, nine files and 47 lines that exercise HOCON
  rather than look like a config, ported as their Psi equivalents, and two or three realistic
  configs written for the module. Rejected: a third party's `reference.conf`, for its licence.
- **Large files: a test that one loads, with no assertion on time**, which would make it flaky.
- **Compatibility while 0.x** (§12): errors stay case classes, their fields and the set of them may
  change; traits users implement grow only by members with an implementation; MiMa with the first
  release.

### Session 23 — 2026-11-13

Spec gaps of plan step 14, docs and release, settled with the user; proposed by me and taken.

- **Two pages on the docs site, by reader**: `Psi`, the user's guide, the format alone, with no
  Scala; and `Psi: API`, the programmer's guide, loading, errors as types, the builder, and
  directives and markers of one's own. The user's split, over mine of format-and-loading against
  extending: whoever only writes configs has no use for the API. Every example is run by mdoc, the
  guide's shown as Psi through `mdoc:passthrough`. Rejected: one long page; this document, design
  notes with their log, as the guide.
- **MiMa with 1.0, not with the first release** (§12), reversing session 22: while 0.x promises
  no compatibility, the check would fail on every break the policy allows. Rejected: MiMa now,
  reporting without failing.
- **The release is v0.1.0**, the user's call: Psi is the first format of the project's own. The
  user tags it.

### Session 24 — 2026-11-14

From a review of the module: two fixes the user asked for, and two of its findings settled with
the user; the last, the growth of a tree by substitutions, is still to be discussed.

- **Reading a URL times out** (§6): 10 seconds to connect, 30 for each wait for the next bytes,
  fixed: the user's call to add them, the values mine, no builder setting yet. Not done: a limit
  on the whole read, and on the size of a source.
- Not a change to the format: a column is counted from the surrogate pairs of the source, found
  once, and no longer from its line's code points for every token, which took time quadratic in
  the length of a line holding any character beyond Latin-1 — 9 seconds for a line of 400 KB.
- **A system property names its path as `Id.path` writes it** (§9) — superseded in session 25,
  system properties no longer overriding anything — a key that is not bare in
  backticks: the user's call, on a finding of the module's review that `` `a.b`: 1 `` and
  `a { b: 2 }` were both overridden by `-Da.b=…`, and a key with a dot could not be overridden on
  its own. Rejected: keys joined by dots as they are, which was the rule.
- **Any key the sources define may be overridden** — superseded in session 25 — confirmed by the
  user on the same review: the
  properties of the JVM itself, `user.name` and the like, overriding keys of the same path is a
  consequence accepted, not a bug.

### Session 25 — 2026-11-15

- **A substitution already resolved counts as deep as resolving it went** (§11), the user's choice
  of the two I put: a node resolved once was used again as free, so a chain written in order, each
  link finding the one before resolved, loaded at any length — 100 000 links, its location nested
  as deep, and `hashCode` and `toString` of it overflowing the stack — where the same chain in
  reverse failed at link 257. Now each resolved substitution keeps how many steps deep resolving it
  went, and a use of it fails as resolving it again there would. A chain goes 256 links at the
  default limit in either order, and no location nests deeper than the limit. Rejected: resolution
  with a stack of its own, so that no chain has a limit, which the user had leaned towards — it
  rewrites `Resolve` and needs `equals`, `hashCode` and `toString` of locations without recursion,
  for chains no config has (session 22).
- **What resolution adds is limited: 1 000 000 nodes grafts copy, 10 000 000 characters
  concatenations build** (§11), the user's choice of the three I put, the defaults and the one
  builder method, `withSizeLimit(nodes, characters)`, as I proposed them. Measured first: ten
  grafts of a sequence of ten grafts, six levels, 484 bytes, ran out of a 1 GB heap in 5 s, and
  five levels took 145 MB; a scalar concatenated with itself, 25 links, 485 bytes, ran out of it
  too. Counted before it is built, so nothing past the limit is allocated, and only what
  resolution adds: a large source is the user's own input. Rejected: subtrees shared rather than
  copied, which leaves a concatenation and the traversal of the tree as large, for a rewrite of
  `Resolve`, the views and the locations; a limit on the substitutions grafting containers, as
  SnakeYAML's `maxAliasesForCollections`, which bounds size only roughly and fails a config
  grafting a common block a hundred times.
- **Reading the sources of a load is limited: 10 000 000 bytes and 60 seconds** (§6, §11), the
  user's choices on my proposal: a limit on the load, not on each source, the defaults, one
  builder method, `withReadLimit(bytes, time)`, and the first read past reported alone. Measured
  first: an include read again wherever it is called, seven files of 300 bytes, each including the
  one before ten times, ran out of a 1 GB heap in 31 s, which a limit on each source does not
  bound, nor a cache of what was parsed, the fragment being placed at every call all the same; and
  a read timeout set on a connection already made is not applied, so the time may be outrun by one
  wait. The error is held by the load, not by the fragment of the read, which a directive of the
  program's own may drop.
- **`impl/yaml` limits a file to 256 levels; `impl/hocon` is for trusted input** (§13 item 10),
  the user's choice on my proposal, the limit fixed, with no setting until one is needed. In YAML
  the parser and the scanner keep stacks of their own and only the composer recurses, so a parser
  that counts the starts and ends of mappings and sequences fails past the limit before the
  composer follows; and `validated`, walking the tree as composed, keeps each node's height, since
  an anchor nested deep and used deep makes the tree deeper than its text, 50 aliases to
  collections, snakeyaml-engine's own limit, reaching 50 times the limit. Typesafe-config offers no
  place to stand: its parser, its objects and its resolution all recurse, and its tokenizer is
  internal. Rejected: a lexer of our own counting brackets and dotted keys before it parses, every
  include intercepted, which still cannot measure a chain of substitutions but by their number,
  and that fails the substitutions of an ordinary `reference.conf`; parsing on a thread with a
  large stack, which takes stack in proportion to the input, gigabytes for 10 MB. YAML read
  through `impl/hocon` goes through `typesafe-config-yaml`, which composes alike; its guard
  belongs in that library.
- **Nothing set at launch reaches the sources on its own; `#prop(name)` and `#env(name)` read it
  where they stand** (§6, §8, §9), the user's idea, on the question of what the system properties
  override once a spread, under discussion, brings keys in from elsewhere: "either they override
  everything or nothing". Everything would have meant overriding every grafted node at its new
  position, inside the resolution; the user chose nothing, and an explicit read instead. The
  phase of overrides is gone, and with it the clash of a JVM property, `user.name`, with a key of
  the same path (session 24), the naming of a property by `Id.path` (session 24), and §13 item 7,
  `#define`. The environment is read after all, but only where a config says so: the reason
  session 18 kept it out, secrets a config could hand out, stands, and a loader for input it does
  not trust turns `#env` off. Settled with the user: `#prop`, my proposal over the user's `#sys`,
  "system" reading as the operating system's as well; a map of the properties under a name no
  property has, the user's addition, the property itself winning over those under it, and one
  both a value and a prefix under it an error; `#env` a scalar only; `optional: true` and
  `default: …`, not both; a dotted name quoted, as every argument is, for now. Breaks every config
  that relied on `-D` to override a key: before 1.0, as the user put it, compatibility is not
  kept.
- **Outside quotes a `$` always opens a substitution, and `$path` is `${path}` with no braces**
  (§3, §9), the user's proposal, with `${…}` kept: a `$` that opens none outside quotes is an
  error, read as text. Inside quotes nothing changes, the user's call when I proposed `$path`
  there too, with `\$` for a literal one: only `${…}` substitutes, and `"$5"` stays text. The path
  with no braces is greedy, the user's choice: it takes every key, `.key` and `[index]` that
  follows, so that `$host-name` and `$name.txt` are paths, and braces end one where it would go
  on. A `$path` in a directive's arguments is reported as `${path}` is, and the hint of that error
  says to quote the text. Breaks a value outside quotes holding a `$`, `price: 5$`, now an error.
- **Spreads: the contents of a node where a node would stand** (§9), the user's idea: the fields of
  a map among a block's, as if written there, and the elements of a sequence among a sequence's.
  Settled with the user, point by point: the merge is deep, as `:`'s; a `*` after the path marks a
  spread, optional among a block's fields, where nothing else may stand, and required among a
  sequence's elements, where `$a` grafts; a spread at the root is an error, every path going
  through it, my proposal over reading the root as written up to the spread, which would have
  made such a reference alone see a tree half built. Mine, in the code: a map keeps what is written
  from its first spread on as steps, merged once each spread is resolved, by `Merge`'s own rules,
  so that "as if written there" is the same code; the merge is memoized as a substitution is, its
  height counted (session 25, above), and a block's fields are resolved after it, so that one may
  refer to another. Covers the first jobs of §13 item 1, which stays parked.

### Session 26 — 2026-11-16

- **Spreads become injections, and the `*` is gone** (§6, §9, §13 item 1), the user's decisions:
  `$a`, `${a}`, `$?a` and `${?a}` are the only forms, and where one stands says what it does. Among
  the fields of a block nothing changes. Among the elements of a sequence a sequence found puts its
  elements there and anything else stands as one element, the user's pick, variant B, over
  requiring a sequence there; `[$a]` nests a sequence whole. The cost, told to the user: the shape
  of a sequence depends on what its injections find. A directive follows the same rule, the user's
  point that `$a` among fields does what a directive in statement position already did: among the
  elements of a sequence, a sequence it gives puts its elements, and `[#d(…)]` nests. A tag before
  an injection that finds a sequence, or a directive that gives one, is an error, the user's
  choice of the three I put, over adding it to each element or forbidding a tag there outright; it
  goes to the one element otherwise, as a graft's. "Spread" is renamed "injection" throughout, the
  code and its errors included. Mine, in the code: an index into a sequence resolves only the
  injections before the element it names, so that `a: [x, $a[0]]` is no cycle, as it was none
  before. Breaks `[x, $a, z]` nesting a sequence, and every `*`: before 1.0, compatibility is not
  kept.

### Session 27 — 2026-11-16

- **A description is cut off at 10 000 characters** (§10), the user's choice of the three I put,
  on a review finding: `a0: ""`, `a1: "${a0}${a0}"`, … `a16` loaded in 297 bytes, its values empty,
  and described in 2 425 181 characters, any error naming it as large. Rejected: describing a part
  of a concatenation by its site and path alone, which loses where the text came from; counting a
  description against the size limit, which fails a load for what only a message would hold. The
  hash of a composite location is cached, the hash of a case class walking every path to what it
  shares.
- Fixed with it, on the same review, the format unchanged: the injections of one block each report
  their own errors, the first that fails no longer hiding the rest (§11), though nothing is merged
  into the block past a failed one, a conflict with what it would have brought being no root cause; a map with injections of
  its own, merged with one an injection brings, takes its location (§10), as a map with none
  already did; `impl/yaml` merges a pair of mappings once, its result shared, where aliases
  made it copy a shared node once for every path to it; and its `toString` writes every tag
  written in the source, the user's choice to fix it there: snakeyaml-engine's serializer leaves
  out any tag its resolver would infer, `!!str` on `hello`, `!!seq` and `!!map` always, never
  asking whether it was written, so the text read back without the tags `tagOf` reported; the
  serializer now resolves by the core schema the loader reads with, so that `Null`, `0x10` or `~`,
  which the core schema infers, are not written with a tag either.

### Session 28 — 2026-11-17

- **Indentation, as in Scala 3** (§4, Indentation; §13 item 2 closed), the user's decisions: two
  ways to write a source, braces and significant indentation, mixed freely; compatibility with
  sources written so far is no concern before 1.0, so indentation counts everywhere, inside `{…}`
  too, and session 2's alternative, indentation only inside blocks it opened, is dropped with the
  question it answered. Not inside `[…]`: an indented sequence is written with `- `, as in YAML. No
  `end` markers for now. A block after `key:` may hold one value, as in YAML and Scala 3. Proposed
  by me after Scala 3 and confirmed by the user: any deeper indent opens a block, with no step
  fixed per file; no switch to turn indentation off, Psi having no warnings for one to fall back
  to; `- ` deeper than its key, YAML's marker in the key's column being an error; any value after
  `- `, `- - x` included, and a tagged map element written `- @t { … }` or `- @t` with a block
  below; `key:` with nothing deeper an error; the top level at column 1; `,` still a separator;
  `Render` writing indentation, braces only for what is empty.

### Session 29 — 2026-11-17

Fixed on a review, the format unchanged, nothing put to the user to decide:

- **`toString` of a location is cut off as its description is** (§10): the one a case class
  generates wrote `a0: ""`, `a1: "${a0}${a0}"`, … `a16` in 10 683 117 characters, and the next
  doubling would run a logger out of memory.
- **What follows a failed injection reports its own errors** (§11), fields as well as
  injections: `b { $missing, x := $other }` named `missing` alone. Fields between two injections
  are merged apart, a conflict with what the one between would have brought being no root cause.
- **Errors of resolution are reported once** (§6, §11), as those of the syntax already were: a
  source with `x: $missing` included in two blocks reported it twice, at the same position.
- **`#include(url: …)` reads a `file:` URL with a fragment**, as a source given so is read: asking
  whether the file is there passed the fragment to the path, and the include failed as an invalid
  target.
- **A read whose last wait went past the time fails** (§11): the time was looked at before each
  wait, never after the one that found the end, so a load of 605 ms succeeded under a limit of
  500.

### Session 30 — 2026-11-18

Fixed on a review, the format unchanged, nothing put to the user to decide but the last item:

- **Two locations are compared each pair under them once** (§10): the `equals` of a case class
  compared two built the same way apart once for every path to each pair, so that two loads of
  `a0: ""`, `a1: "${a0}${a0}"`, … `a100` never finished comparing.
- **A grafted node writes the path it was taken by with `Message.path`**, as every path from the
  input is written: `Id.path` recursed as deep as the node, doubling the stack `graft` uses.
- **A remote jar's copy that cannot be deleted is left behind**: the failure to delete it took
  the place of the read's outcome, a success or the error that ended it.
- **The last segment of a `file`, where an extension is looked for, is the file system's** (§6):
  it was cut at `/` alone, so that on Windows `C:\conf.d\db` read as having an extension and was not
  looked for as `.Ψ`, `.ψ` and `.psi`. It is now the file name `Path` gives, which knows the
  separators of the system the program runs on; a `resource` keeps `/`, the class path's on every
  system. The user's call, over writing `/` alone into the spec: what separates a path is the
  system's to say, read by `Path` or `URL`, not by the loader.

### Session 31 — 2026-11-18

- **The `*` comes back, marking an injection** (§9, §6, §4), the user's decision. I raised that the
  rule of the indented syntax, a block's first line saying what it is, read `a:⏎  $b` as
  `a { $b }` and `password:⏎  #env(name: X)` as a directive in statement position, against the
  rule of the same section that the next line is only one more place for a value. The name stays
  "injection", the forms are session 25's, `$a*`, `${a*}`, `$?a*` and `${?a*}`, and the rest are
  the user's choices of what I put, point by point: the `*` is required among fields too, `$a`
  alone among them an error, over session 25's `*` optional there; among elements `$a` grafts one
  element and `$a*` puts a sequence's, so that a sequence's shape no longer depends on what its
  substitutions find, the cost session 26 took knowingly; `$a*` finding anything but a sequence
  among elements, or anything but a map among fields, is an error, over session 26's one element,
  the `*` saying what was meant; no tags before an injection; a directive among elements injects
  with `#d(…)*` and stands as one element without it, while in statement position it puts a map's
  fields with no `*`, so that `#include(file: …)` is written as it was, the asymmetry told to the
  user and taken. In the indented syntax a block holding a field or an injection is a map, and one
  value alone, a substitution or a directive included, is the key's value: `a:⏎  $b` is `a: $b`.
  Breaks every injection written since session 26: before 1.0, compatibility is not kept.
  Implemented with indentation.
- **A line that starts with `}` may stand in any column** (§4, Indentation), my proposal, agreed:
  the width rule read literally checked it against the blocks it closes; Scala 3 does not.
- Fixed with it: a code fence in §4, Indentation, ran into the sentence after it.

### Session 32 — 2026-11-18

Two gaps of §4, Indentation, found implementing it and settled by the user as I proposed them:

- **`- $a*` puts the elements of a sequence**, as `[$a*]` does: a value means after `- ` what it
  means as an element, as after `key:` it means what it means on the line of the `:`. Rejected:
  reading it as a map element `{ $a* }`, as `a:⏎  $b*` is, which would make `- ` the one place a
  lone injection changes meaning. A map element that starts with an injection has a field after it
  or braces.
- **A `-` alone on its line takes its value in a block below**, deeper than the marker, as YAML's
  does and as `- @t` already did: the same case with no tags, as `key:` with a block below.

Indentation implemented (§4), with the `*` of session 31. Mine, in the code, the format unchanged:

- A block below a `:` goes deeper than the block its key stands in, or than the key's line when that
  goes deeper still: `a { b:⏎  c: 1 }` holds `b { c: 1 }`, the `{…}` having no width yet. Below a
  `- ` it goes deeper than the marker.
- A line holding only a doc comment takes no part in indentation, as a comment does not: the doc
  comment goes with the item after it, wherever that stands. A line that starts with a doc comment
  and goes on with its item starts where the doc comment does (session 33).
- Whether a line starts a field is read ahead: keys, then `:` or `:=` with whitespace after it, or
  `{`. `a:⏎  http://host` and `a:⏎  example.com` are values. A value that `Render` writes after a
  `- ` and that would read as a field, `x:` or `x{`, is quoted.
- A line indented deeper than its block, where nothing opens one, is `UnexpectedIndent`, and one less
  indented landing on no width `MisalignedIndent`, each skipped to its end; a line that misses
  reports every line after it, deeper ones as `UnexpectedIndent`, until one lands. A block holding
  elements and anything else is `MixedBlock`, a second value `SecondValue`, a value among fields
  `ValueAmongFields`, or `SubstitutionAmongFields` for a substitution with no `*`, and a tag before
  the first field `TagBeforeField`. A block opened by indentation that holds a map or a sequence
  counts against the depth limit as a bracket does (§11).
- `Render` writes indentation, and `toString` of a node other than the root writes a map as its
  fields and a sequence as its elements, at the top level, a block below its tags when it has any.

### Session 33 — 2026-11-18

Fixed on a review of session 32's code, the format unchanged, nothing put to the user to decide:

- **`- #d(…)*` loads**: a call injected after a `- ` was reported as tagged, tags or none.
- **A line that starts with a doc comment is placed by it** (§4): the item after the comment on its
  line skipped the check of its column, so `/** d */ b: 2` after an indented block stayed in it, and
  `   /** d */ b: 2` at the top level was taken.
- **A line of tags whose value is below counts as a level** (§11): a chain of such lines nested
  blocks that counted nothing, and 1 500 of them overflowed the stack.
- **A doc comment before an injection or a call that a block starts with is kept** once the block
  turns out to be a map, as in braces.
- One test of whether a field starts, the lexer's, where the parser had a second with other rules:
  `a: 1 x:y` is a second word of the value, as `x:y` is a value in a block.
- The hint of an injection out of place shows `${a}*` for a `*` that is text: quoting `$a*`, as it
  said, kept the `$a` as text too.
- `Render` writes an element that starts on the line of its `- ` once, where it copied it again at
  every level, and looks for a field in a scalar only when it holds a `:` or a `{`.

### Session 34 — 2026-11-19

- **The `*` is gone again: where a substitution stands says whether it injects** (§4, §6, §9), the
  user's decision, on the user's point that injections follow the logic of directives, which inject
  in statement position and give one node in value position with no `*`. A key or a `- ` before a
  substitution or a call gives it a place for one node; with none, alone among fields or alone on
  its line among `- ` elements, it injects into the block around it. Session 31's case for the `*`,
  `a:⏎  $b` read two ways, is settled as `#include` already was: one value alone after a key is its
  value, `a:⏎  $b` a graft, and `a:⏎  $b⏎  x: 1` injects. The difference, told to the user before
  deciding and taken: a key that `a:⏎  $b` grafts at cannot take a map merged into it from elsewhere
  (§7), where `a { $b }` can; and the graft brings the map's tags along and takes what is no map.
- **Alone on its line among `- ` elements a substitution puts the elements of a sequence**, the
  user's proposal: `- $b` is one element, `$b` with no marker the elements of `b`, an error when `b`
  is no sequence. It undoes session 32's `- $a*`.
- **In `[…]`, by what it finds**, the user's choice of the two I put (session 26's rule): a sequence
  puts its elements, anything else stands as one element. Taken over brackets always injecting,
  which would make `[$db.host, $cache.host]` an error and leave no way to put a scalar or `null` in
  brackets but a concatenation; and over brackets never injecting, which leaves brackets no way to
  concatenate. Brackets around it nest a sequence whole, `[[$a]]`.
- **Two or more substitutions or calls alone in a block opened by indentation, none tagged, are
  decided by what they find**, the user's choice of the two I put, over an error: the first that
  finds or gives anything makes the block a map or a sequence, and the rest must agree. Mine, in the
  code: a call there runs when the source is expanded, so one that gives a map or a sequence decides
  before any substitution is resolved; a block where none finds anything is nothing, as an optional
  graft that finds nothing is.
- Mine, in the code: a tag before an injection alone on its line is `TaggedInjection`, a call's
  included, where among fields it was `ValueAmongFields`; in brackets, before a sequence found or
  given, `TaggedSequence` or `TaggedDirectiveSequence`; a block of injections alone whose first
  finds or gives neither a map nor a sequence, `InjectionNotContainer` or `DirectiveNotContainer`.
  `SubstitutionAmongFields` and `InjectionOutOfPlace` are gone. `Render` writes a merged tree's
  injections as they are written: alone on their lines, and a sequence holding one from `[…]` in
  brackets.
- **A handler gives back `Either[DirectiveError, Fragment]`**, nothing being `Fragment.None` (§6), the
  user's proposal, over `Option[Fragment]`: nothing is a value of the type, as `Node.None` is a node
  that is not there. Named `None` over `Empty`, the user's choice of the two I put: `Empty` reads as
  an empty map, which `a: #d` gives as `a: {}`, where nothing leaves `a` out.
- Breaks every `*` written since session 31, and `[x, $a, z]`, which session 31 made nest a
  sequence whole, puts its elements again: before 1.0, compatibility is not kept.

### Session 35 — 2026-11-19

- **A directive and a substitution follow one rule of where they stand** (§6, §9), the user's
  principle: a call behaves as a substitution would standing there, and a substitution as a call,
  resolution aside. I went through the rules with it and put to the user what it leaves: the root,
  where a call stands and an injection does not, and a merge with what is written elsewhere at a key,
  which a call's result takes and a substitution's does not — both from when each is done, so within
  the principle — and two that are not.
- **In a block of injections alone, the first as written decides, a call or a substitution** (§9),
  fixing session 34's code, which let a call decide whatever stood before it, being expanded first:
  `b:⏎  $seq⏎  #map` was a map with the error at `$seq`. What a call gives is now kept in the block
  until its substitutions are resolved, and the block is then read in order. A block of calls alone
  is therefore no longer a map before merging: what is written elsewhere at its key does not merge
  with it, as with a block of substitutions alone (§7), the principle taken over the convenience.
- **A string is the exception**, the user's decision: a substitution concatenates in a scalar, a
  call does not.
- **The cost kept, the message made honest** (§7, §13 item 11), the user's choice of the two I put:
  `db:⏎  #include(…)⏎  #include(…)` no longer merges with `db.port: 6432` written elsewhere. The
  error named a substitution where there were calls; it now names a substitution or a block of
  injections alone, and its hint offers braces besides `:=`. Deferring such a merge is §13 item 11.
  The user wants the documentation to gather such traps later, in a section of their own.

### Session 36 — 2026-11-19

- **A merge with a substitution waits for it, and merges as `:` does** (§7, §13 item 11), the
  user's decision, reversing the rule of session 3, which the user had set: `a: ${x}` with
  `a.port: 1` elsewhere, an error since then, merges once `${x}` is resolved — its fields and port
  over them when it is a map, port alone when it is not, replaced. The user's points: `a.port: 1`
  is `a { port: 1 }`, and a call merges so already, a substitution being a call resolved later
  (session 35); a call's map is replaced silently by what is written over it, and a substitution's
  now is too. Settled with the user, as I put them: the map a substitution finds brings its tags,
  united as `:` unites them, unlike an injection, which brings none; what is no map is replaced
  silently, a mistake included; `${?x}` finding nothing puts nothing, `a: ${?x}` and `a.port: 1`
  giving `a { port: 1 }`.
- Mine, in the code: the merge keeps the values at a key as layers, `Tree.Layers`, read at
  resolution from the last back to the first that is no map, which stands alone or is replaced by
  the maps after it, merged in turn; what it replaces is not resolved. `MergeWithSubstitution`,
  and with it every merge error and the keys a failed merge dropped, is gone. It was the one error
  at a composite location, so every error now stands at a `SourceLocation`, and the report writes
  no origin after a message.

### Session 37 — 2026-11-20

- **A value has at most one tag** (§5), the user's decision, reversing session 6's set: an
  application has no way to settle several, and a set united on merge makes a conflict out of two
  sources that only the application could refuse and only `:=` could fix in the config. Kept, in
  the user's words, at least for now. `ISome.tags: Set[String]` becomes `ISome.tag: Option[String]`
  again, in `cfg` and every loader; `impl/yaml` loses nothing, YAML allowing one tag per node.
- **The tag written last wins, the one before it when none is written** (§5, §7, §9, §6), the
  user's rule and session 3's, which session 6 dropped only because a node had become a set:
  `b: @y $a` is tagged `y`, `b: $a` takes `a`'s, and `a: @t { … }` with `a.x: 1` stays `t`. It goes
  wherever a union went, as I put it and the user agreed: a merge of maps, a graft, a tag before a
  directive's result, and session 36's merge with a substitution, its layers read in turn.
  Concatenations, injections and blocks of injections alone are unchanged.
- **Two tags on one value are an error, a tag written twice too** (§5), the user's call over
  collapsing a repeat: with nothing to choose between, a second tag is a mistake. So is the tag
  after `key:` with one before the value in the block below, which §4 added up.
- **A replacing value comes with its own tag, a map by `:=` included** (§7), the user's call of the
  two I put: `timeout: @duration 5s` then `timeout: 10s` leaves `timeout` untagged, a trap the
  user's guide states. Rejected: inheriting the tag on any replacement, which would tag a sequence
  with what was written for the map it replaced.
- **`@-name` stays reserved** (§5), the user's call; with one tag a name may prove unneeded, which
  is left for its design.
- Before 1.0 compatibility is not kept; I noted that going back to a set united on merge later
  would change what a file with conflicting tags means.

### Session 38 — 2026-11-21

- **Formatters** (§9, §13 item 4), the user's design, replacing the options inside `${…}` noted in
  session 7: `${path; name(arguments)}`, a handler registered with the loader as a directive or a
  marker is, a function of its arguments giving a function from a node to a string. The user's
  calls on what I put: one formatter in a substitution, a chain left for later; `join` the one
  built-in for now; in quotes the call read by its own rules, as Scala reads `s"${f("x")}"`, over
  allowing only `'…'` arguments there, which would not have kept a `}` in an argument from closing
  the substitution.
- Mine, put to the user with the design: a string is the boundary, a node given back being value
  expressions (item 1, parked), so a formatter never injects and its result takes only the tag
  written before it, as a concatenation's; the handler may fail at either step, its arguments
  taken in phase 1 with the errors of the syntax and the directives; `INull` reaches the handler,
  an optional substitution finding nothing calling none; `join` reserved as the built-in markers
  are, neither replaced nor disabled; what a loader's formatter gives not counted against the size
  limit, as a marker's is not, `join` counted as a concatenation.
- **The name of a handler is given where it is registered** (§6), the user's decision:
  `withDirective(name, handler)`, `withMarker` and `withFormatter` alike, the name no longer a
  member of `Directive` or `Marker`. The user's reason: the consumer, not the author, says what a
  handler is called in the consumer's files. The built-in names stay fixed, the user's call, the
  built-in handlers not opened for registering under another name. It goes back to session 10's
  shape, `withMarkers(markers: (String, MarkerProcessor)*)`: the code of plan step 10 made the name
  a member, and I found no decision logged for it.

### Session 39 — 2026-11-21

- **Importing YAML and HOCON** (§6, §13 item 8), the user's calls on what I put: directives in
  modules of their own, `cfg-psi-yaml` and `cfg-psi-hocon`, over making `cfg-yaml` and `cfg-hocon`
  depend on `cfg-psi`, which I had recommended; one directive a format, not `#include` choosing by
  the extension; the target written as `#include`'s, its resolution given to every handler by the
  context; any node at the root of an imported YAML file.
- **The read limit, a compromise**, the user's pick of what I put: `#yaml` reads its bytes through
  the context and is counted; `#hocon` lets Typesafe Config read, since a HOCON file's `include`s
  resolve against its own URL, which reading it from text would lose, and is not counted (§11).
- Mine, from reading the loaders, put to the user with the design: `HOCON.apply` cannot be
  reused, `ConfigFactory.load` laying the system properties over a file and adding
  `reference.conf`, which would set values at launch past `#prop` (session 25). An import of HOCON
  is for trusted input only, Typesafe Config recursing with no limit (§13 item 10).
- Mine as well, decided in writing the design, not yet put to the user: environment variables
  turned off in resolving a HOCON file, Typesafe Config falling back to them for a substitution the
  file does not define, which would read the environment past `#env`; the extensions looked for,
  `.yaml` then `.yml`, and `.conf`; a file's errors one failure of the directive, the reader's
  message in it; `cfg-yaml` and `cfg-hocon` each given a function of their own for it, a node of
  any kind from bytes, and a file resolved on its own.

Fixed on a review of sessions 36 and 38's code, the format unchanged, nothing put to the user:

- An optional formatted substitution that found nothing dropped the map written before it at its key
  (`a { x: 1 }` then `a: ${?m; join(",")}`): it replaced at once rather than waiting as §9 says,
  as a merge with a substitution does (session 36). It is a layer now.
- An index into a sequence did not count a formatted substitution among its elements, so that one
  finding nothing shifted every element after it: `$s[1]` gave the first.
- A field merged over a substitution could not refer to another of its map, `a: $base` then
  `a { y: $a.x }` a false cycle: the layers' children were resolved inside the mark of their key.
  The layers are merged once, and their children resolved after, as a map's with injections are.
- Each formatter copied the rest of its line, so that a line of 8 000 took seconds: the line's end
  is found once, and read through a view of the source.

- After the review, the user's call: `#hocon` looks for a file or a resource with no extension as
  `.json` and `.properties` too, after `.conf`, both being syntaxes HOCON takes. It reads the first
  found, as `#include` does; Typesafe Config's own search by name merges every one it finds.

Fixed on a second review of sessions 39 and 36's code, nothing of the files' meaning changed:

- **A fragment a handler built from a `Node` counts against the node limit** (§11), where a
  handler's work was not counted: `#yaml` copied every use of an alias, and 316 bytes of YAML gave
  131 056 nodes, 523 bytes took a 64 MB heap. Each node is counted before it is built, the limit
  shared with grafts, which count on from what the fragments brought. Mine, put to the user. The
  default limit, 1 000 000 nodes, does not keep a 64 MB heap from filling, from YAML aliases or from
  Psi's own grafts alike (measured: some 350 000 nodes do); `withSizeLimit` sets it for such a heap.
- **A target's candidates are tried in turn when their presence is known only once read**: the
  context gave the first candidate not known not to be there, and lost the rest, so that over HTTP
  `#yaml(file: data)` asked for `data.yaml` alone. The context's `target` gives way to `find`,
  which opens each candidate by the handler's function, as `#include` did all along.
- **What the calls of a block of injections alone gave refers to itself once merged**, as layers'
  fields do since the first review: `a:⏎  #include(file: base)⏎  #include(file: other)` with
  `y: $a.x` in `other` was a false cycle. A substitution there still finds its node resolved whole
  first, so `$c` with `c { y: $a.x }` among them stays a cycle, as an injection is (§9).

Fixed on a third review of session 39's code, nothing of the files' meaning changed:

- **A fragment a handler kept and gave again was not counted**: the nodes of a source parsed
  through the context came in free on every use. They are free on the first use in the load that
  parsed it, being that source's own, and counted on every other, in that load or a later one.
- **Going past the node limit is the load's error**: reported with the fragment it was found in,
  it went with a fragment a handler dropped, and every call after it gave nothing silently, so a
  required field went missing from a load that succeeded.
- **A merge key that makes a mapping contain itself, `a: &a {<<: *a}`, held `#yaml` for ever**
  inside snakeyaml-engine, which merges keys while composing, before `cfg-yaml` checked for
  cycles. `cfg-yaml` refuses an alias to a mapping or a sequence not yet ended as the parser gives
  it, before the composer sees it; the check after composing had nothing left to find.

### Session 40 — 2026-11-22

- **Loading with no source named** (§8), the user's request, as Typesafe Config's
  `ConfigFactory.load()`: `application` and every `reference` on the class path, with Psi's
  extensions. Typesafe Config, read in its 1.4.2 sources: `application` merges every syntax it
  finds, `.conf` over `.json` over `.properties`, and `config.resource`, `config.file` and
  `config.url` replace it; `reference` is `reference.conf` alone, but every copy on the class path,
  each required to resolve on its own and then merged unresolved below the application, the
  system properties over all.
- Mine, put to the user and agreed with as a whole, the names included: separate methods,
  `standard()` and `withReferences(urls*)`, over `apply()` with no URL, which would make an empty
  list of files read the class path; the first `application` found, as `#include` and `#hocon`
  read, over merging every one; every extension of `reference`, `.Ψ` over `.ψ` over `.psi`, then
  class path order; one resolution over all the sources, with no check that a reference resolves
  alone; no system property choosing the file, as session 25 has it.

Fixed on a fourth review, the code of sessions 32 and 36 and the parser's, put to the user with the fix:

- **A chain of includes among fields overflowed the stack within both limits**: 63 sources each
  including the next among fields, the last 255 levels deep, threw a `StackOverflowError` on a
  stack of 1 MB, breaking classes for the JVM. A source parsed in value position was read a level
  below the call; among fields it was read at the call's own depth, its parse costing stack and no
  level. It is read a level below there too (§11), so that a chain of includes takes no more stack
  than the levels it is charged. Mine; it changes what fits, by a level for each include among fields.
- **The parser's errors were forgotten from a list walked from its head**, the reading ahead and
  coming back dropping a tail each time: 400 KB of `a:⏎  1: x` took 8 seconds. They are kept in an
  array.
- **An index into a sequence walked it from its first element on every lookup**: 100 000 lookups
  of a sequence of literals took 14 seconds. The walk is kept by position and taken up where it
  stopped; a lookup that finds its element already found counts as deep as walking to it would,
  and fails where that would go past the limit, as the walk would, so the limit still does not
  depend on order (§11).

### Session 41 — 2026-11-22

- **Indices from the end** (§9), `${hosts[-1]}`, as Python's: the user's request, for the last
  element of a sequence gathered from several sources. Put to the user and agreed with point by
  point: one element only, no slices, which build a new value (§13 item 1); the path kept and
  reported as written, the one exception to a path being what `Id.path` renders, since it is what
  the source holds; `[-0]` an error, as `[07]` is; an index before the first element nothing, as
  one past the last; a cycle of its own for an element that the end depends on, `a: [x, $a[-1]]`,
  which an index from 0 never makes. Meta-fields (§13 item 6) left as they are.
- Breaks: unbraced, `$a[-1]` was the substitution `a` and the text `[-1]`; it is now an index.
- Mine, put to the user: the reference keeps `Id.Index(-1, …)` in its path, `cfg`'s `Id` taking
  any `Int`, and only the lookup turns it into a position; nothing outside a reference ever holds
  one.

### Session 42 — 2026-11-22

- **The `*` comes back, for good: an injection is marked** (§4, §6, §7, §9), the user's decision,
  final, on the user's point that a reader cannot tell what a block or a sequence holds when the
  place alone decides: in `a: [$b, $c]`, `b` a sequence and `c` a scalar, the reader sees two
  elements where session 34 put three. The user's rules: `a: $b` grafts; `a: $b*` injects one map
  or one sequence, the tags dropped for simplicity; `$c` alone among fields is an error and `$c*`
  there injects; `[b, $c, d]` and `- $c` are always one element, undoing session 34's brackets
  decided by what they find; `[b, $c*, d]` and `- $c*` put `c`'s elements; `$c` alone on its line
  among `- ` elements is an error.
- Mine, put to the user and agreed with as a whole:
  - `$c*` alone on its line among `- ` elements is an error too: every line of a sequence written
    with dashes starts with one, `- $c*` being the injection;
  - a block of injections alone, `a:⏎  $b*⏎  $c*`, is a map, a line with no `- ` being a field's
    place, so session 34's rule that the first to find anything decides is gone; a sequence of
    several is `a:⏎  - $b*⏎  - $c*`. The edge, taken: `a:⏎  $b*` alone is `a: $b*` (§4) and takes a
    sequence, which a second line `$c*` then turns into an error;
  - `a: $?b*` finding nothing leaves `a` out, and an optional injection among fields or elements
    puts nothing;
  - `a: @t $b*` tags the node it makes, which has no tag of its own; a tag before an injection
    among fields or elements stays an error;
  - **a directive takes the same `*`**, by session 35's principle that a call does what a
    substitution standing there does: `#include(…)*` among fields, `- #d(…)*` and `[…, #d(…)*]`
    among elements, `a: #d(…)*` after a key; unmarked, a call is one node, an error alone among
    fields. Taken over session 31's exception for statement position, the reader's case applying to
    calls as much; every `#include` in statement position gains a `*`;
  - **the `*` marks a whole item only**: in a concatenation, `x$b*` or `$dir*.log`, it is an error
    with a hint to write `${dir}*`, so that a glob `$prefix*`, text until now, does not become an
    injection unnoticed; in quotes it is text; with braces it goes inside, `${b*}`, `${b}*` being
    the substitution and the text `*`;
  - `${b*; join(",")}` is an error, a formatter never injecting;
  - the rest stays: no injection at the root, none into its own block, the limits, `Render` writing
    the `*` back, and an index counting what injections put — now resolving only marked ones and
    optional substitutions, an unmarked element being one element.
- **Corrected in the discussion**: I said a graft does not merge with what is written elsewhere at
  its key. Session 36 made it merge once resolved, so `a: $b` and `a: $b*` merge alike; what sets
  them apart is the tags, and that `$b*` must find a map or a sequence.
- Session 41's examples of a cycle from the end gain the marker, `a: [x, $a[-1]*]` and
  `a: [x, $b*]` with `b: $a[-1]`; `a: [x, $a[-1]]`, an element referring to itself, is an ordinary
  cycle now.
- Breaks every injection written since session 34, every `#include` in statement position, and an
  unquoted value ending in `$name*`: before 1.0, compatibility is not kept. The code follows in a
  PR of its own.
- Settled in the code, put to the user: two or more injections alone in a block finding nothing make
  an empty map, as the same in braces does, where session 42's text left the key out; and `${b}*`
  after a `- ` is a string, as after a key.

### Session 43 — 2026-11-22

- **Stack headroom, by fewer and smaller frames** (§13 item 12), the user's choice of the ways I put,
  over a lower default limit, an explicit stack and a thread of its own; `TailCalls`, the user's
  suggestion, recorded there with its catch. Measured with a fresh JVM for each try, a few loads
  in, where C1's frames make it worst: a map of maps 256 levels deep took 880 KB and takes about
  620 KB. The test stays at 1 MB: built with coverage, as CI runs it, the frames are larger, and
  768 KB is not enough.

### Session 44 — 2026-11-22

Fixed on a fifth review, put to the user with the fixes:

- **A block of two or more injections alone, opened by indentation, was a level the parser never
  counted** (session 42): at the limit, resolution stopped where no substitution was being followed,
  reporting nothing, and the load gave back its tree unresolved, `${…}` left in it, the root's other
  fields included. The block opens its level as soon as a second line makes it a map, as a field
  does, and past the limit the source ends there (§11).
- **An index into a sequence resolved a required formatted substitution to count it** (§9), though it
  is always one element, a string: `a: [${a[1]; join(",")}, [z]]`, or `[-1]`, was a cycle. Only an
  optional one, which may put none, is resolved.
- **`cfg-yaml` rendered two anchors named alike as written**: two overlaid files each naming an anchor
  `x` were written with two `&x`, and an alias after the second read back as it, so a value changed
  through `toString`. Each rendering names its anchors anew, a node keeping its own name while no
  node before it has taken it.
- **`a: 1` then `a: ${?x}` left `a` out when `x` was missing** (§7), where `b: 1`, `a: ${b}` and then
  `a: ${?x}` gave 1: a value that is no map was replaced at the merge, before the optional one was
  known to put nothing. The user's call, as I put it: what stands before an optional substitution,
  formatted or not, or an optional injection after a key, stands when it finds nothing, kept below
  it as `Tree.Kept`.

### Session 45 — 2026-11-23

- **Meta-fields parked** (§13 item 6), by the user's call, for no set time: the user no longer
  remembers what they were wanted for, and nothing in the format or the docs waits on them. The
  notes of sessions 15 and 16 stay in §13 for when a case for them comes back.
- **Delimiters chosen per template declined** (§13 item 4), by the user's call: a block that picks
  its own syntax of substitution, `<…>` or `$…$` as in ST4, would leave each reader to find out which
  syntax holds where, a circus of them. A block whose text is full of `${` is written `'''…'''`,
  which substitutes nothing (§5), or escapes each as `\${`. With it the last open point of item 4 is
  gone, and the item is closed: lists joined by `join`, options by formatters (session 38).
- **The default depth limit lowered to 128, from 256** (§11, §13 item 12), my proposal on
  measurements, the user's call: even 32 levels is a lot for a configuration, and 128 is past
  anything reasonable. Measured on each phase in a fresh JVM for each try, a few loads in, where
  C1's frames make it worst: at 256 the parser was not the only cost — merging, resolving and
  rendering each took 450 to 550 KB — and the costliest load, 710 KB, was a chain of substitutions
  each referring to a key written after it, all of it resolving, where a map of maps took 620 KB.
  At 128 they take about 420 KB and 370 KB, with more than half of a 1 MB stack to spare (Java 25,
  without coverage, whose frames are larger). An explicit stack, the user's first favourite, would
  have relieved the parser alone — the chain, which takes nothing to parse, not at all, and a map
  of maps by about an eighth — unless `Merge`, `Resolve` and `Render` were rewritten too; with the
  limit at 128 the user judged none of it critical, and §13 item 12 is closed. `cfg-yaml`
  keeps its 256: its recursion is snakeyaml-engine's, measured on its own (§13 item 10). Breaks a
  source deeper than 128 levels loaded with the default: before 1.0, compatibility is not kept.

### Session 46 — 2026-11-23

- **A built-in marker reads its arguments as text, as a handler does** (§5): `width` took only an
  integer written bare, `width("2")` an error, the one place where the kind of literal written
  mattered — a handler gets each argument as text, a string, an integer and a bare name alike. The
  user's call, as I put it: one rule for the built-in markers and for a loader's, `width("2")` now
  `width(2)`, and the kinds of literal gone from the code; an integer is still checked as written,
  an optional `-` and no leading zeros.

### Session 47 — 2026-11-23

- **The first batch goes by line and column too** (§11), as the second has since session 20: the
  order found put the lexer's errors inside a token before the parser's at its start, so pasted
  HOCON, `url: "jdbc:"${host}"/db"`, was reported at 1:20 and 1:26, then 1:12. The user's call, on
  the docs page comparing Psi with YAML and HOCON. The sources keep the order of the second batch,
  an included source's errors after every error of the source including it, where before they stood
  at the call; the report groups them by source either way. A fatal error, last in the order found,
  now stands at its line and column: an unterminated string before the errors found inside it.
