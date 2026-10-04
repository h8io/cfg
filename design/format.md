# Native config format — design notes

Working document for the new loader module under `impl/`. The format name, the module name and
the file extension are **not decided yet**; this document says `NCF` (native config format) as a
placeholder and marks every place the name leaks into the code.

**Status:** under discussion, expected to span several sessions. §2 lists what is settled, §12 what
is still open, and §13 logs how each decision was reached so a later session does not reopen a
question that was already argued through. Add to §13 rather than silently editing §2.

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
| Special words | Avoided — `@`-prefixed directives instead, as an open, extensible set |

## 3. Lexical structure

- Encoding is UTF-8.
- `#` starts a comment that runs to the end of the line. There is deliberately no second comment
  form.
- **Bare keys** match `Id.SafeKeyPattern` exactly — `^[\p{L}_][\p{L}\p{N}_-]*$`. Anything else is
  written as a quoted key.
- **Quoted keys and quoted scalars** use `"…"` with the escape set `Id.quote` produces: `\"`, `\\`,
  `\b`, `\f`, `\n`, `\r`, `\t` and `\uXXXX`.

  The two rules together buy a property worth keeping: the output of `Id.path` is itself valid
  source. `server."odd key"[0]` renders from a node and parses back to the same address. Any change
  to `Id.quote` has to be mirrored here, and vice versa.
- **Unquoted scalars** run to the end of the line, or to the first `,`, `}`, `]` or `#`, with
  surrounding whitespace trimmed. What is left becomes `IScalar.value` verbatim — `1e5` stays
  `"1e5"`, `true` stays `"true"`, `007` stays `"007"`. There is no type inference anywhere in the
  parser.

## 4. Grammar sketch

```
document   = block-body
block      = "{" block-body "}"
block-body = { field | directive } 
field      = key { "." key } ( "=" value | block )
value      = [ tag ] ( scalar | block | seq | directive | substitution-expr )
seq        = "[" [ value { sep value } [ sep ] ] "]"
sep        = "," | newline
tag        = "!" identifier
scalar     = bare-scalar | quoted-scalar | multiline-scalar
```

Fields are separated by a newline or a `,`. `key { … }` needs no `=`. A dotted key
(`server.tls.enabled = true`) is sugar for nested blocks.

## 5. Values

**Sequences** — `[a, b, c]`; a newline also separates, and a trailing separator is allowed.

**Multi-line scalars** — `"""…"""`, taken raw: no escape processing at all, so PEM blocks, regexes
and SQL survive untouched. The newline immediately after the opening delimiter is dropped, and the
longest common leading whitespace across the non-blank lines is stripped:

```
server {
  banner = """
    Welcome.
      Indented line.
  """
}
# value: "Welcome.\n  Indented line."
```

**Tags** — `!name` before any value, including containers:

```
timeout = !duration 5s
routes  = !ordered { a = 1, b = 2 }
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
- **value position**, on the right of `=` or as a sequence element — it produces a single node.

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
  scalar.
- `${?path}` is optional: when it does not resolve, the field is omitted entirely, so a later lookup
  yields `Node.INone` rather than an error.
- Lookup order is the merged tree, then system properties, then environment variables — the HOCON
  behaviour, chosen deliberately over an explicit `${env:…}` form.
- Cycles are an error naming the path. Self-reference (`a = ${a}" x"` reading the pre-merge value) is
  **not** supported in v1; it is reported as a cycle.

## 10. Locations

Every node carries its own `Location`, with `description` rendered as `<source>:<line>:<column>`.
This is the visible win over `impl/hocon`, where the description is whatever typesafe-config
composed.

For a node produced by a substitution, the proposal is to keep the **definition** site — where the
value was actually written — rather than the reference site.

## 11. Implementation notes

- No external dependencies, like `cfg` and `impl/hocon`.
- Unlike the hocon and yaml backends, which wrap a foreign structure lazily, this module owns its
  data: phase 5 builds immutable `Map`/`Vector` structures once, and `MapImpl`/`SeqImpl` are thin
  views over them.
- Cross-built for 2.13 and 2.12. A hand-written lexer is exactly the kind of code that reaches for
  2.13-only collection methods; `scala-collection-compat` is already wired in for 2.12.
- `-Xfatal-warnings` is on, so exhaustiveness in the parser's pattern matches is enforced rather
  than merely intended.

## 12. Open questions

1. **Error model.** `Either[CfgError, Node.IMap[Id.Root]]` keeps the module dependency-free and puts
   a syntax error in the same algebra as a decode error; an exception matches what `HOCON` and `YAML`
   do today; `Validated` would let a whole file's errors accumulate through `AndError` but pulls in
   Cats. Recommendation: `Either`, with a `ParseError` carrying a `Location`.
2. **How to write `null`.** `INull` is a distinct node in the protocol, so the format needs a
   spelling for it, but a bare `null` keyword is exactly the kind of magic word the directive system
   exists to avoid. Candidates: bare `null`, a `!null` tag, an `@null` directive, or an empty value.
3. **Dotted keys.** Kept as sugar above; worth confirming, since they interact with quoted keys and
   with `Id.path` round-tripping.
4. **Substitution location** — definition site or reference site (§10).
5. **Name.** Format name, module directory, artifact id, package, file extension and loader object
   name. Deferred by decision.
6. **Does this retire `impl/hocon`?** It is recorded as a temporary PoC, and this module covers its
   use cases without its scalar-fidelity defect.

## 13. Decision log

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
