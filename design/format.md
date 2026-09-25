# Native config format — design notes

Working document for the new loader module under `impl/`. The format name, the module name and
the file extension are **not decided yet**; this document says `NCF` (native config format) as a
placeholder and marks every place the name leaks into the code.

**Status:** under discussion, expected to span several sessions. §2 lists what is settled, §13 what
is still open, and §14 logs how each decision was reached so a later session does not reopen a
question that was already argued through. Add to §14 rather than silently editing §2.

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
| Multi-line strings | Triple quotes, raw, with the common leading indent stripped |
| Special words | Avoided — `@`-prefixed directives instead, as an open, extensible set. `null` is the one exception |
| Error model | `Either`, no Cats; as many errors per run as can be reported honestly (§11) |
| Dotted keys | Sugar for nesting; a quoted key is indivisible, dots included |
| Substitution location | Reference site, carrying the definition site with it (§10) |
| `impl/hocon` | Stays, alongside this module |

## 3. Lexical structure

- Encoding is UTF-8. A newline is LF or CRLF; a lone CR is an error.
- `#` starts a comment that runs to the end of the line. There is deliberately no second comment
  form.
- **Bare keys** match `Id.SafeKeyPattern` exactly — `^[\p{L}_][\p{L}\p{N}_-]*$`. Anything else is
  written as a quoted key.
- **Quoted keys and quoted scalars** use `"…"` with the escape set `Id.quote` produces: `\"`, `\\`,
  `\b`, `\f`, `\n`, `\r`, `\t` and `\uXXXX`.
- **Dotted keys** — `a.b.c: v` is sugar for `a { b { c: v } }`. The split happens only on dots
  *between* key tokens: a quoted key is indivisible, so `"a.b": v` is one key named `a.b`, and
  `x."a.b".c` is three levels. This is also what `Id.path` produces — a key containing a dot fails
  `SafeKeyPattern` and is rendered quoted — so the round-trip property holds.

  The two rules together buy a property worth keeping: the output of `Id.path` is itself valid
  source. `server."odd key"[0]` renders from a node and parses back to the same address. Any change
  to `Id.quote` has to be mirrored here, and vice versa.
- **Unquoted scalars** contain no whitespace. One runs to the first whitespace, `,`, `}`, `]` or `#`;
  after it only a separator, a comment or the end of the block may follow, so `a: foo bar` is an
  error that says to quote the value. What was read becomes `IScalar.value` verbatim — `1e5` stays
  `"1e5"`, `true` stays `"true"`, `007` stays `"007"`. There is no type inference anywhere in the
  parser. A value with spaces is written quoted: `greeting: "Hello, world"`.

  An unquoted scalar may not start with `'`: YAML's `a: 'x'` would otherwise parse silently to a
  value with the quotes kept. `~` gets no rule — it is the string `~`, and `null` is written out.
- **`:` after a key** must be followed by whitespace or the end of the line, so `a:b` in a block is an
  error with a hint rather than a field. Inside a value `:` is ordinary: `url: http://host:8080`.
- **Separators** are `,` and newline, nothing else — whitespace never separates, so `[foo bar]` and
  `a: 1 b: 2` are errors, not two elements or two fields. After any value, scalar or container,
  only a separator, a comment or the closing bracket may follow: `a { x: 1 } b: 2` needs a `,`.
  A `,` followed by a newline is one separator, and blank lines separate nothing extra. An empty
  element (`[a,,b]`, `[, a]`) is an error — there is no empty value, `null` is written out. A
  trailing separator is allowed in sequences and blocks alike.

  The rule also frees `- ` (dash, space) at the start of an indented line for the sequence element
  marker (§13): `- 5` can never be a scalar, `-5` and `"- 5"` always are.
- **`null`** — the one reserved word. An unquoted scalar whose trimmed text is exactly `null`
  becomes `Node.INull`; `"null"` in quotes stays a scalar. A tag is kept: `!secret null` is an
  `INull` with `tag = Some("secret")`. `true`, `false` and every other word remain plain scalars.
  `null` is reserved only in value position — as a key it is an ordinary bare key.

## 4. Grammar sketch

```
document   = block-body
block      = "{" block-body "}"
block-body = { field | directive } 
field      = key { "." key } ( ":" value | block )
value      = [ tag ] ( scalar | block | seq | directive | substitution-expr )
seq        = "[" [ value { sep value } [ sep ] ] "]"
sep        = "," | newline
tag        = "!" identifier
scalar     = null | bare-scalar | quoted-scalar | multiline-scalar
```

Fields are separated by a newline or a `,` (§3). `key { … }` needs no `:`, and the `{` must be on the
same line as the key: `key` followed by `{` on the next line is an error. That keeps a key at the end
of a line free for the indentation syntax (§13). A dotted key
(`server.tls.enabled: true`) is sugar for nested blocks.

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
# value: "Welcome.\n  Indented line."
```

**Tags** — `!name` before any value, including containers:

```
timeout: !duration 5s
routes: !ordered { a: 1, b: 2 }
```

Only tags written in the source reach `ISome.tag`; nothing is ever inferred, and there is no
non-specific tag that resolves to `None`. This matches the rule already settled for `impl/yaml`.

## 6. Directives

The extension point, and the reason `include` is not a keyword. A directive is

```
@name(arg = literal, arg = literal)
```

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

`@include` is the only built-in:

```
@include(file = "conf.d/extra")             # relative to the including file's URL
@include(file = "local", optional = true)   # missing file is not an error
@include(resource = "reference")            # classpath, JVM only
@include(url = "https://…")
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

## 8. Phase order

1. Parse each source URL into a syntax tree, directives unexpanded.
2. Expand directives depth-first. `@include` parses the referenced source recursively and merges it
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

A node produced by a substitution carries **both** sites: the reference site as its primary
coordinates, and the location of the node it was taken from. `Location` is an open trait in `cfg`, so
this needs no protocol change — the module defines its own hierarchy:

```scala
sealed trait NcfLocation extends Location

// written here
final case class SourceLocation(source: String, line: Int, column: Int) extends NcfLocation
// grafted here by `${path}`; `origin` is where the value came from
final case class ReferenceLocation(at: SourceLocation, path: String, origin: Location) extends NcfLocation
// a scalar built by concatenation; one entry per `${…}`, each with its own column
final case class ConcatenationLocation(at: SourceLocation, parts: ::[ReferenceLocation]) extends NcfLocation
// origins that are not a file
final case class SystemPropertyLocation(name: String) extends NcfLocation
final case class EnvLocation(name: String) extends NcfLocation
```

`origin` is a `Location`, not a `SourceLocation`, so chains fall out on their own: with
`a: ${b}`, `b: ${c}`, `c: 1`, the node at `a` is `ReferenceLocation(a-site, "b",
ReferenceLocation(b-site, "c", SourceLocation(c-site)))`. The environment and system properties get
a location of their own instead of a fake file position.

`description` puts the reference site first, because that is where the reader has to go:
`app.conf:12:9 (${db.url} from base.conf:3:7)`.

**Grafted containers.** With `a: ${server}`, *every* node in the grafted subtree gets a
`ReferenceLocation` — the reference site of `a`, the path it was taken by (`server.port`), and that
node's own origin. Marking only the root would leave an error at `a.port` pointing at `server`'s
line with no hint of how it ended up under `a`.

**Concatenation.** `url: "jdbc:"${host}"/db"` is a new scalar written at the reference site, so its
primary coordinates are that site, and it carries one `ReferenceLocation` per `${…}` in the order
written — every source the value was assembled from.

## 11. Errors

The loader returns `Either[NcfErrors, Node.IMap[Id.Root]]`, where `NcfErrors` is a non-empty list of
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
- Cross-built for 2.13 and 2.12. A hand-written lexer is exactly the kind of code that reaches for
  2.13-only collection methods; `scala-collection-compat` is already wired in for 2.12.
- `-Xfatal-warnings` is on, so exhaustiveness in the parser's pattern matches is enforced rather
  than merely intended.

## 13. Open questions

1. **Name.** Format name, module directory, artifact id, package, file extension and loader object
   name. Deferred by decision; still has to be settled before any code is written.
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
   - A container tag goes after `:`: `routes: !ordered`, block on the next line.
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
