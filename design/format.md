# DEF — design notes

Working document for the new loader module under `impl/`. The format is **DEF** — *Declarative
Extensible Format*:

| Where | Name |
|---|---|
| Format | DEF |
| Module directory | `impl/def` |
| sbt project | `defg` — `def` cannot be a `val` |
| Artifact | `cfg-def` |
| Package | `h8io.cfg.impl.defg` — *def* + *cfg* |
| Loader object | `DEF`, as `HOCON` and `YAML` |
| File extension | `.def` |

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
| Multi-line strings | Triple quotes, raw, with the common leading indent stripped |
| Special words | Avoided — `#`-prefixed directives instead, as an open, extensible set. `null` is the one exception |
| Error model | `Either`, no Cats; as many errors per run as can be reported honestly (§11) |
| Dotted keys | Sugar for nesting; a quoted key is indivisible, dots included |
| Substitution location | Reference site, carrying the definition site with it (§10) |
| Location coordinates | Source as it was named — the URL passed to the loader, or an include argument as written; line and column 1-based, column in code points (§10) |
| `impl/hocon` | Stays, alongside this module |
| Overriding | `:` merges maps, `:=` replaces whatever was there (§7); no append operator |

## 3. Lexical structure

- Encoding is UTF-8. A newline is LF or CRLF; a lone CR is an error.
- **Comments** are `//` to the end of the line and `/* … */`, which nest, as in Scala: commenting out
  a block that already holds a comment is safe. An unterminated `/*` is reported at its opening, a
  stray `*/` is an error, and `/**/` is an empty ordinary comment. Inside an unquoted scalar `//`
  and `/*` are ordinary characters; anywhere else they open a comment. So `url: http://host` is one
  scalar — and so is `a: 1//x`, the price of the rule — while `pattern: /*.log` opens a comment, and
  a value starting with `//` or `/*` is written in quotes. `#` has no meaning of its own here.
- **Doc comments** — `/** … */` is reserved for documentation that DSLs built on DEF may one day
  want in the source. v1 ignores it, but accepts it only before a field, a sequence element or a
  directive; anywhere else (between a tag and its value, after a value on the same line) it is an
  error. Doc comments carry no meaning for values, so giving them one later breaks no v1 file. That
  will take a field such as `doc: Option[String]` on `ISome` — a change to `cfg`, for `schema` to
  read (generated reference docs, decoder messages) — so the lexer emits a doc comment as a token
  and the Ast keeps it on the node it precedes from the start.
- **Bare keys** match `Id.SafeKeyPattern` exactly — `^[\p{L}_][\p{L}\p{N}_-]*$`. Anything else is
  written as a quoted key. A key that would pass bare may be quoted too: `` `port` `` and `port` are
  the same key.
- **Names in backticks, values in double quotes.** A quoted key — in a field, a dotted key or a path
  inside `${…}` — is written `` `…` ``; `"…"` is only ever a scalar. The role of a token is
  therefore visible to the lexer without context, and `"port": 1` is an error with a hint, not a
  second spelling. Both forms share one escape set: `\"`, ``\` ``, `\\`, `\b`, `\f`, `\n`, `\r`, `\t`
  and `\uXXXX` — the set `Id.quote` produces today, plus ``\` ``. Both quotes are escapable in both
  forms, so there is one escape grammar rather than one per delimiter; `Id.quote` itself only needs
  ``\` `` and `\\`. Any other `\` is an error. Neither form may hold a raw newline — that is what `\n`
  and `"""…"""` are for — so an unterminated quote is reported on its own line rather than at the end
  of the file.
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
  error that says to quote the value. What was read becomes `IScalar.value` verbatim — `1e5` stays
  `"1e5"`, `true` stays `"true"`, `007` stays `"007"`. There is no type inference anywhere in the
  parser. A value with spaces is written quoted: `greeting: "Hello, world"`.

  An unquoted scalar may not start with `'`: YAML's `a: 'x'` would otherwise parse silently to a
  value with the quotes kept. `~` gets no rule — it is the string `~`, and `null` is written out.
- **`:` after a key** must be followed by whitespace or the end of the line, so `a:b` in a block is an
  error with a hint rather than a field. Inside a value `:` is ordinary: `url: http://host:8080`.
  `:=` is the replacing form of `:` (§7) and follows the same rule: `a:=b` is an error.
- **Separators** are `,` and newline, nothing else — whitespace never separates, so `[foo bar]` and
  `a: 1 b: 2` are errors, not two elements or two fields. After any value, scalar or container,
  only a separator, a comment or the closing bracket may follow: `a { x: 1 } b: 2` needs a `,`.
  A `,` followed by a newline is one separator, and blank lines separate nothing extra. An empty
  element (`[a,,b]`, `[, a]`) is an error — there is no empty value, `null` is written out. A
  trailing separator is allowed in sequences and blocks alike.

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
directive  = "#" name "(" [ arg { sep arg } [ sep ] ] ")"
scalar     = null | bare-scalar | quoted-scalar | multiline-scalar
```

Fields are separated by a newline or a `,` (§3). `key { … }` needs no `:`, and the `{` must be on the
same line as the key: `key` followed by `{` on the next line is an error. That keeps a key at the end
of a line free for the indentation syntax (§13). A dotted key
(`server.tls.enabled: true`) is sugar for nested blocks. The block form `key { … }` means `key: { … }`,
so it merges; replacing a block is written `key := { … }`.

## 5. Values

**Sequences** — `[a, b, c]`; a newline also separates, and a trailing separator is allowed.

**Multi-line scalars** — `"""…"""`, taken raw: no escape processing at all, so PEM blocks, regexes
and SQL survive untouched. The newline immediately after the opening delimiter is dropped, and the
longest common leading whitespace across the non-blank lines is stripped:

```
server {
  banner: """
    Welcome.
      Indented line.
  """
}
// value: "Welcome.\n  Indented line."
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
so there is no need for `!!`. `impl/yaml` follows the same rule and drops the leading `!` of a tag
(`!postgres` gives `postgres`, `!!int` still `tag:yaml.org,2002:int`) — a breaking change to
`cfg-yaml`, made in step 2 of the plan. It merges YAML's `!foo` and `!<foo>`, two different tags in
YAML, into one name; the protocol accepts that.

Only tags written in the source reach `ISome.tags`; nothing is ever inferred, and there is no
non-specific tag that resolves to nothing. This matches the rule already settled for `impl/yaml`.

**A value takes any number of tags, and they form a set** — as an element takes CSS classes, which
is the role tags are meant to play for a later DEF schema. They are written stacked, like
annotations: `@pooled @readonly { … }`. Order carries no meaning, `@a @b` is the same as `@b @a`,
and a tag written twice counts once: `@a @a x` is `@a x`, not an error.
In the protocol `ISome.tag: Option[String]` becomes `ISome.tags: Set[String]`, empty when nothing
was written — a change to `cfg` and every loader, made in step 2 of the plan together with the `!`
above, so the API breaks once. `impl/hocon` always gives the empty set, `impl/yaml` at most one
element, YAML allowing one tag per node.

**`@-name` is reserved** for removing an inherited tag (§7) and is an error in v1. It costs nothing:
a bare tag name follows `Id.SafeKeyPattern`, which cannot start with `-`, so `@-` never begins a
legal tag. A tag whose name starts with `-` is written in backticks, ``@`-name` ``, and is an
ordinary tag.

## 6. Directives

The extension point, and the reason `include` is not a keyword. A directive is

```
#name(arg = literal, arg = literal)
```

The `#` is the C preprocessor's, and for the same reason: directives are expanded while parsing,
before merging and substitution (§8). A value starting with `#` is written in quotes
(`color: "#fff"` — unquoted it is a call of an unknown directive `fff`); inside an unquoted scalar
`#` is ordinary. A pasted YAML or HOCON comment fails loudly: `# text` is a directive with no name.

and may appear in two positions:

- **statement position**, inside any block or at the top of a file — it must produce a map, which is
  merged into the enclosing block at the point where it appears, so fields below it override it;
- **value position**, on the right of `:` or as a sequence element — it produces a single node.

Arguments are separated like everything else, by `,` or a newline, so a long directive can be split
across lines. They keep `name = literal`, not `name: literal`: a directive is a call with named
arguments, as in Scala, not a block of fields.

Argument literals are strings, bare identifiers, `true`/`false` and integers. Arguments are
**literal only — no substitutions**. This is not a simplification, it is forced by the phase order in
§8: directives are expanded during parsing, while `${…}` is resolved after every source has been
merged, so a directive argument written as `${base}"/x"` would have to be resolved before the value
it refers to exists.

Directives are an open set. The loader is built with a set of handlers; the format itself only fixes
the call syntax. An unknown directive is an error, never a silently ignored field — otherwise a typo
in a directive name becomes a config silently missing half its content.

`#include` is the only built-in:

```
#include(file = "conf.d/extra")             // relative to the including file's URL
#include(file = "local", optional = true)   // missing file is not an error
#include(resource = "reference")            // classpath, JVM only
#include(url = "https://…")
```

Exactly one of `file` / `resource` / `url` is required. Relative paths resolve against the URL of the
including file, never the working directory. Include cycles are detected on the URL stack and
reported as an error rather than looping.

## 7. Merging

Deep merge, last one wins — maps merge recursively, scalars and sequences are replaced wholesale.
The same rule applies in all three places it is needed, so there is one behaviour to learn:

- duplicate keys within one file,
- a directive's result against the block it lands in,
- the N URLs passed to the loader.

This follows the call already made for `impl/yaml`.

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

A concatenation such as `"jdbc:"${host}` is always a scalar and never triggers the rule; `${?x}` does,
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
- Tags written before a whole-value `${…}` are added to the grafted node's own: with
  `b: @x { … }`, `a: @y ${b}` gives `a` the tags `x` and `y`. The union is the same as in a merge
  (§7), so a graft and a merge never disagree about tags.
- A concatenation builds a new scalar, which carries only the tags written before it: in
  `@t "jdbc:"${host}` the result is tagged `t` alone, whatever `host` carries.
- `prefix${path}suffix` concatenates; the referenced node must then be a scalar, and the result is a
  scalar. The parts touch: `"a" ${b}` with a space between is an error, not a concatenation with a
  space as in HOCON — whitespace never joins or separates values.
- `${?path}` is optional: when it does not resolve, the field is omitted entirely, so a later lookup
  yields `Node.INone` rather than an error.
- Lookup order is the merged tree, then system properties, then environment variables — the HOCON
  behaviour, chosen deliberately over an explicit `${env:…}` form.
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

- a source passed to `DEF(urls*)` — the full URL, as the caller gave it (`file:/etc/app/app.def`);
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
sealed trait DefLocation extends Location

// written here
final case class SourceLocation(source: String, line: Int, column: Int) extends DefLocation
// grafted here by `${path}`; `origin` is where the value came from
final case class ReferenceLocation(at: SourceLocation, path: String, origin: Location) extends DefLocation
// a scalar built by concatenation; one entry per `${…}`, each with its own column
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends DefLocation
// origins that are not a file
final case class SystemPropertyLocation(name: String) extends DefLocation
final case class EnvLocation(name: String) extends DefLocation
```

`origin` is a `Location`, not a `SourceLocation`, so chains fall out on their own: with
`a: ${b}`, `b: ${c}`, `c: 1`, the node at `a` is `ReferenceLocation(a-site, "b",
ReferenceLocation(b-site, "c", SourceLocation(c-site)))`. The environment and system properties get
a location of their own instead of a fake file position.

`description` puts the reference site first, because that is where the reader has to go:
`file:/etc/app/app.def:12:9 (${db.url} from base.def:3:7)`.

**Grafted containers.** With `a: ${server}`, *every* node in the grafted subtree gets a
`ReferenceLocation` — the reference site of `a`, the path it was taken by (`server.port`), and that
node's own origin. Marking only the root would leave an error at `a.port` pointing at `server`'s
line with no hint of how it ended up under `a`.

**Concatenation.** `url: "jdbc:"${host}"/db"` is a new scalar written at the reference site, so its
primary coordinates are that site, and it carries one `ReferenceLocation` per `${…}` in the order
written — every source the value was assembled from.

## 11. Errors

The loader returns `Either[DefErrors, Node.IMap[Id.Root]]`, where `DefErrors` is a non-empty list of
module-local errors and itself a `CfgError` — `Either` is covariant, so it reads as
`Either[CfgError, …]` to anyone who does not care. The list is the module's own type (head + `List`),
because `AndError` and `NonEmptyChain` live in `schema` and Cats is not a dependency here. Every
error carries a `Location`.

The goal is **as many errors per run as can be reported without inventing any**: a user fixing a
config should not have to rerun the loader once per mistake, but a cascade of consequences of one
mistake is worse than stopping.

- **Parsing recovers at synchronisation points.** A broken field is skipped to the next field
  separator (newline or `,`) at the same bracket depth, or to the `}` closing its block; the parser
  tracks bracket depth to find them. The braced syntax is what makes this cheap. Each source is
  parsed independently, so an error in one file never hides errors in another.
- **Some errors end the source.** An unterminated `"…"` or `"""…"""`, or an unbalanced bracket at
  end of input, leaves nothing trustworthy to resynchronise on; the parser reports it and stops
  *that source*.
- **Directive failures are local.** A missing include, a cycle, an unknown directive or a bad
  argument is reported, the directive contributes nothing, and expansion continues.
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
- Warnings are fatal on every version (`-Xfatal-warnings` on 2.x, `-Werror` on 3), so exhaustiveness in the parser's pattern matches is enforced rather
  than merely intended.

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
   - A tab in indentation is an error.
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

   Constraint on v1 meanwhile: leading whitespace must carry no meaning anywhere else, so that
   indentation can claim it later.

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
  language, and DEF is meant as a base for DSLs, which may want documented configuration — `/** */`
  is the familiar form, `#` has none (`##` would be weak). Comments open only outside an unquoted
  scalar, so URLs need no quotes, at the price of `a: 1//x` being one scalar; values starting with
  `//` or `/*` (globs) need quotes. Block comments nest, as in Scala. Doc comments are positioned
  and kept as tokens from v1, so that giving them meaning later breaks nothing. Rejected: `#`, the
  session 1 choice, and with it the "deliberately one comment form" rule.
- **`ISome.tag` holds the tag's name only**, without the prefix or the quotes (§5). The user's call:
  both are syntax of the format and meaningless in the value. `impl/yaml` drops the leading `!` to
  match, a breaking change to `cfg-yaml`. Rejected: keeping YAML's `!` (the string would depend on
  the syntax that wrote it); letting DEF and YAML differ.
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
