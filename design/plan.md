# Ψ (Psi) — implementation plan

Step-by-step plan for implementing the format specified in [`format.md`](format.md). The work is
large, so it is cut into steps small enough to finish in one session each, and this file is what a
new session reads to find out where things stand.

## How to work with this plan

- **One step, one branch, one PR.** Branch `psi/NN-slug` (e.g. `psi/03-parser`), PR against `main`.
  A step is done when its PR is merged; the PR flips the step's status below to `done` in the same
  diff, so `main` always shows the true state.
- **Resuming.** Find the first step that is not `done`. If its status is `in progress`, its branch
  and PR exist — check them out (`gh pr list --head psi/NN-slug`), read the PR description and the
  step's *Progress* notes, and continue from there. Never start step N+1 before step N is merged
  unless the step says it is independent.
- **A step that turns out too big** is split here, in this file, into `NNa`/`NNb`, and only the first
  part is finished in the current PR. Unfinished work is not left uncommitted at the end of a
  session: push what compiles and passes, and write in *Progress* what is left.
- **Every PR is green on `./test.sh`**: formatting, cross-build for 2.13, 2.12 and 3.9, and **100%
  statement and branch coverage** — the build enforces it (`coverageSummary{Stmt,Branch}LowThreshold`),
  so tests are written in the same step as the code, never later.
- **Spec gaps.** Each step lists the questions the spec does not answer yet. They are decided with
  the user *before* coding the part that depends on them, and recorded in `format.md` (§2/§3–§11 plus
  an entry in §14), not here. Do not silently pick an answer in the code.
- **Nothing is published until step 14.** The module is created with `publish / skip := true` and is
  left out of the docs site, so partial work cannot leak into a release.

## Architecture

The pipeline follows the phase order in `format.md` §8. Everything is private to the module except
the loader, the location types, the error types and the directive extension API.

```
URL ──read──▶ Source ──lex/parse──▶ Ast ──expand directives──▶ Ast ──merge──▶ Tree ──resolve ${}──▶ Tree ──wrap──▶ Node.IMap[Id.Root]
                                    (per source, with recovery)       (N sources)   (phase-gated)
```

| Piece | Package member (all under `h8io.cfg.impl.psi`) | Visibility |
|---|---|---|
| Loader | `Psi`: `Psi(urls*)`, `Psi.parse(name, text)` | public |
| Locations | `PsiLocation`, `SourceLocation`, `ReferenceLocation`, `ConcatenationLocation`, `MergedLocation`, `NoSourceLocation`, `SystemPropertyLocation`, `EnvLocation` (§10) | public |
| Errors | `PsiError` (each with a `location`), `PsiErrors` (non-empty, a `CfgError`) (§11) | public |
| Directive API | handler trait + registry passed to the loader (§6) | public, designed in step 10 |
| Source | the text of one URL, with offset → line/column | private |
| Lexer | mode-driven: the parser asks for the next token *in key mode* or *in value mode* | private |
| Ast | syntax tree of one source: fields with key path and operator (`:`/`:=`/block), values, `${…}`, concatenations, directives, every node with a `SourceLocation` | private |
| Tree | merged, operator-free tree; leaves may still be `${…}` | private |
| Node views | `MapImpl`/`SeqImpl` over immutable `Map`/`Vector` (§12) | private |

Why a mode-driven lexer: the same characters mean different things on the two sides of `:` —
`a.b` is a dotted key on the left and the scalar `a.b` on the right, `http://host:8080` is a value
but `a:b` in key position is an error. The future indentation syntax (§13) needs tokens to carry
their column and whether they start a line, so every token records both from the start.

Error accumulation without Cats: a small private result type (value + `List[PsiError]`) with `map`,
`flatMap` and a combinator that collects errors from independent parts. `PsiErrors` is built from it
at the phase gate.

## Steps

| # | Step | Status |
|---|---|---|
| 0 | This plan | done |
| 1 | Module skeleton, locations, errors | done |
| 2a | Protocol: tags as a set, backticked keys, YAML tags without `!` | done |
| 2b | Source and lexer | done |
| 2c | Lexer: triple-quoted and raw strings | done |
| 2d | Rename DEF to Ψ (Psi) | done |
| 3 | Ast and parser: core syntax | done |
| 4 | Parser: substitutions and directives | done |
| 5 | Parser: error recovery | done |
| 6 | Merge | done |
| 7 | Node views and the loader, end to end | done |
| 8 | Renderer and round-trip property tests | done |
| 9 | Substitutions | in progress |
| 10 | Directive and marker framework | todo |
| 11 | `#include` | todo |
| 12 | Error messages and hints | todo |
| 13 | Hardening | todo |
| 14 | Docs and release | todo |

### 0. This plan

`design/plan.md`, plus a pointer to it in `format.md`.

### 1. Module skeleton, locations, errors

- `build.sbt`: `val psi = (project in file("impl/psi"))`, `name := "cfg-psi"`, depends on `cfg` only,
  `publish / skip := true`. Add to `root / aggregate` (needed for coverage), **not** to `pages` yet.
- Location hierarchy from §10 with `description` exactly as specified:
  `file:/etc/app/app.psi:12:9`, and for a reference
  `file:/etc/app/app.psi:12:9 (${db.url} from base.psi:3:7)`. Chains render recursively. `<source>`,
  line and column as decided in `format.md` §10.
- `PsiError` (sealed, every case has a `location`), `PsiErrors` (head + tail, `CfgError`), and the
  private result type. Start with the error cases this step can test; later steps add theirs.
- **Spec gaps:** none left — `<source>` and column counting were settled in session 4, the
  `description` of the other locations and the first error case (an unreadable source) in session 7
  (`format.md` §10, §14).

### 2. Split into 2a, 2b, 2c

Step 2 was split when it started: the protocol changes touch every module and break the API, and are
independent of the lexer, which is large enough on its own; triple-quoted strings, with their layout
rules (`format.md` §5), are a part of their own.

### 2a. Protocol: tags as a set, backticked keys, YAML tags without `!`

- `Id.quote` in `cfg` renders a quoted key in backticks (`format.md` §3), escaping ``\` `` and `\\`
  and no longer `"`. Every path the HOCON and YAML loaders print changes with it, so the release
  notes say so.
- `impl/yaml`: `tagOf` drops the leading `!` of a tag (`format.md` §5), `TagOfTest` updated, the
  YAML section of `CLAUDE.md` with it. A breaking change to `cfg-yaml`, in the release notes too.
- `cfg`: `ISome.tag: Option[String]` becomes `ISome.tags: Set[String]` (`format.md` §5) — `INull`,
  `IScalar`, their extractors, both loaders (`impl/hocon` empty, `impl/yaml` at most one) and their
  tests. Made in the same release as the `!`, so the API breaks once; in the release notes.
- The release notes are the PR description, under *Breaking changes*.

### 2b. Source and lexer

- Read a URL as UTF-8, skipping a byte order mark; LF and CRLF are newlines, a lone CR is an error
  (§3). An unreadable URL is `SourceUnreadable`.
- Tokens: `{ } [ ] ( ) , . : := = @tag #name`, newline, comment (`//`, nesting `/* */`, skipped),
  doc comment (`/** */`, kept), bare key (`Id.SafeKeyPattern`), backticked name, unquoted scalar,
  quoted string with the escape set of §3 (`Id.quote`'s plus ``\` `` and `\$`) and `${…}` inside it,
  `${`/`${?` … `}`. Every token records its column and whether it starts a line.
- Value mode: an unquoted scalar runs to whitespace, `,`, `}` or `]`; may not start with `'`
  (§3) until 2c reads raw strings, and `@` or `#` at its start opens a tag or a directive (§5, §6);
  `@name(` is an error, so is `@-`, reserved (§5). Inside it `//` and `/*` are ordinary characters;
  anywhere else they open a comment. Key mode: `:` must be followed by whitespace or end of line, so must `:=`.
- Whitespace is space and tab; any other whitespace outside quotes and comments, and a tab in
  indentation, are errors, not fatal (§3).
- Fatal lexical errors (unterminated `"…"`/`` `…` ``/`/*`) end the source (§11); others are reported
  and lexing continues.
- `"""` is reported as not supported yet, until 2c.
- Property test: for any string `k`, `Id.Key(k, Id.Root).path` lexes in key mode to exactly one key
  token with value `k` — the round-trip promise of §3.

### 2c. Lexer: triple-quoted and raw strings

- Triple-quoted string (§5): the escape set of `"…"` and `${…}`, escapes interpreted after the
  layout rules; a run of more than three quotes closes at its last three.
- Always a block (no one-line form since session 9): whatever follows the opening delimiter on its
  line is the marker, an unknown one an error, not fatal; whitespace after the delimiter or a known
  marker is an error, not fatal; first and last newline dropped; trailing whitespace stripped; CRLF
  to LF; markers written as directive calls and applied left to right (session 10) — none (Java's
  rule, the closing delimiter's column counting), `<`, `>` (kept), `+` (folding), and
  `width(n: N)` relative to the opening line; mismatched indents are errors.
- An unterminated `"""` ends the source (§11).
- Raw strings (§5): `'…'` on one line and `'''…'''` with the layout of `"""`; no
  substitutions, `\'` and `\\` the only escapes. `ApostropheAtStart` goes; an unterminated raw
  string ends the source, as its double-quoted counterpart does.

**Spec gaps for 2a–2c:** none left. Multi-line strings were reopened and settled in session 7
(`format.md` §5, §14), as were directive names, `${` in and out of quotes, the byte order mark and a
tag before `"""` (§3, §5, §6). Session 9 dropped the one-line form of `"""` when 2c started, and
settled that any text after the opening delimiter is a marker, an unknown one not fatal (§5, §14).

### 2d. Rename DEF to Ψ (Psi)

Decided in session 10 (`format.md` §14), to be done once 2c is merged and before step 3, while the
code is the lexer alone and nothing is published.

- Everything: `impl/def` to `impl/psi`, sbt project `defg` to `psi`, artifact `cfg-def` to `cfg-psi`,
  package `h8io.cfg.impl.defg` to `h8io.cfg.impl.psi`, the `Def*` types, the loader object, the
  name in `format.md`, in this plan and in `CLAUDE.md`, and the branch prefix `def/` to `psi/` from
  step 3 on.
- Extensions `.Ψ`, `.ψ`, `.psi`, searched in that order.
- The loader object is `Psi`; on a case-insensitive file system `.Ψ` and `.ψ` are one extension,
  which the spec states (`format.md` §14, session 10).
- **Spec gaps:** none.

### 3. Ast and parser: core syntax

- Recursive descent over the lexer: document, block, fields, dotted keys (backticked keys indivisible),
  `:`/`:=`/`key { … }`, `{` on the key's line, sequences, separators (`,`/newline, trailing allowed,
  empty element an error, whitespace never separates), tags on every value, `null` in value position
  only, `a: foo bar` an error. Doc comments only before a field, an element or a directive,
  kept on that node in the Ast (§3).
- A value on the line of its `:`/`:=` and of its tag; `key @t { … }` an error with a hint; no
  whitespace around the dots of a key; a leading `,` separates as a trailing one does (§3, §4).
- `${…}` and directives are reported as not supported yet, until step 4.
- In this step the parser stops at the first error; recovery is step 5.
- The Ast keeps operators and source order — merging is step 6.
- **Spec gaps:** none left — settled in session 12 (`format.md` §3, §4, §14).

### 4. Parser: substitutions and directives

- `${path}`, `${?path}`, and concatenations — a scalar with `${…}` inside (`jdbc:${host}/db`,
  `"jdbc:${host}/db"`); two values side by side, `"jdbc:"${host}`, are an error with a hint (§9).
- Directives in statement and value position, arguments `name: literal` separated by `,`/newline,
  literals: strings, bare identifiers, `true`/`false`, integers; `${…}` in an argument is an error
  (§6). The Ast only — nothing is expanded yet.
- Paths inside `${…}` are `Id.path` syntax, indices included (`${x.y[7].z}`, settled in session 7,
  `format.md` §9). Property test: `Id.path` of any `Id` below the root parses back to the same `Id`.
- `#name` without parentheses is a call with no arguments; the `(` touches the name; arguments
  separated by a block's rules; a named one `name: literal`, `=` an error with a hint (session 12);
  parameter names bare or in backticks; strings in `"…"` or `'…'`
  only; integers as markers' (§6). A duplicate named argument and a positional one after a named
  one are parser errors; tags before a value-position directive are kept in the Ast.
- **Spec gaps:** none left — settled in session 12 (`format.md` §4, §6, §14).

### 5. Parser: error recovery

- Synchronisation points from §11: skip to the next field separator at the same bracket depth or to
  the `}` closing the block; unbalanced brackets at end of input end the source.
- Tests: several independent errors in one file are all reported, a broken field does not hide the
  next one, no cascade errors after a recovered one.
- **Spec gaps:** settled in session 14 (`format.md` §11, §14): sequences and arguments recover as
  blocks do; a call with an error is left out whole; a bracket closing another than the innermost
  open one ends the source; skipped items leave holes, not nodes.

### 6. Merge

- Ast → Tree for one source: duplicate keys merge in order, dotted keys merge into nested blocks,
  `:=` drops the earlier value, block form merges (§4, §7).
- Tags in a merge: merged maps take the union of their tags; a replaced value keeps only its own
  (§7).
- No merging over a substitution: a `:` merge with a whole-value `${…}` on one side and a map or
  `${…}` on the other is an error suggesting `:=`, at any depth (§7).
- Merging N trees in order — the same function.
- Locations of merged containers: `MergedLocation`, every site, the latest first.
- **Spec gaps:** settled in session 15 (`format.md` §7, §10, §14): merged maps list their sites,
  the latest first; a dotted key's maps stand at their segments; keys in order of first appearance,
  a replaced key keeping its place; `null` merges as a scalar.

### 7. Node views and the loader, end to end

- `MapImpl`/`SeqImpl` as thin views over `Map`/`Vector` (§12), `Node.INull` for `null`, `-` on maps,
  ids assigned while wrapping.
- `Psi.apply(urls: URL*): Either[PsiErrors, Node.IMap[Id.Root]]`; no URLs gives an empty root map,
  as `YAML` does. The phase gate of §11 is wired here.
- Until steps 9–11 land, a `${…}` reaching this phase, or a directive reaching the merge (which
  takes none, session 15), is reported as a "not supported yet" error, so the loader is total from
  the first version.
- Test resources under `impl/psi/src/test/resources/*.psi`.
- **Spec gaps:** settled in session 16 (`format.md` §10, §14): the root of a load given no source is
  at `NoSourceLocation`; `Psi.parse(name, text)` loads a source held as a string; one temporary
  `NotSupportedYet` error.

### 8. Renderer and round-trip property tests

- Render a Tree (and `MapImpl`/`SeqImpl.toString`) as Psi source: quoted keys and scalars where
  needed, tags, `null`, nested blocks.
- ScalaCheck generator for Trees; property: render → parse → merge gives the same tree. This is the
  main defence against parser corner cases, and it guards §3's `Id.path` promise from the other side.
- **Spec gaps:** settled in session 17 (`format.md` §14): a field per line, maps in the block form,
  scalars quoted only when they must be, tags sorted, the renderer internal for now.

### 9. Substitutions

- Resolution over the merged Tree (§9): whole-value graft of any node including `null`, `${?…}` drops
  the field, concatenation of scalars only, lookup order tree → system properties → environment.
  System properties and the environment are injected as functions, so tests do not touch the real
  ones.
- Tags written before a whole-value `${…}` are added to the grafted node's (§9); a concatenation carries only
  the tags written before it.
- Cycles are errors naming the path; self-reference is a cycle (§9).
- Root causes only: a node depending on a failed one is dropped silently (§11).
- Locations (§10): `ReferenceLocation` on every node of a grafted subtree, chains for chained
  references, `ConcatenationLocation` with one entry per `${…}`, `SystemPropertyLocation` and
  `EnvLocation`.
- Remove the "not supported yet" path for `${…}` from step 7.
- **Spec gaps:** settled in session 18 (`format.md` §9, §14): `${?x}` drops a sequence element and
  reads as empty in a concatenation; a grafted node's id is its new position; system properties are a
  tree split at the dots, the environment a flat map, both of strings.

### 10. Directive and marker framework

- Markers become handlers too (session 10, `format.md` §14): a builder,
  `Psi.builder.withMarkers(…).withDirectives(…)`. `withMarkers` adds to the reserved `<`, `>`,
  `width` and `+`, `withDirectives` to the reserved `include`; registering a reserved name, or any
  name twice, is an error when the loader is built (sessions 12, 13). `disableDirective(name)`
  turns a directive off for good: a call is a `DisabledDirective`, and registering a disabled name
  is an error when the loader is built (session 13). A marker handler maps the lines of a block to
  lines, at the layout stage. Until
  this step the lexer keeps the built-in markers hard-coded, as 2c wrote them.

- The public extension API (§6, settled in session 11): a handler declares its parameters and gets
  its arguments as a `Node.IMap` keyed by them, the location and a context (the including source's
  URL, the URL stack for cycle detection, a way to parse another source), and returns a fragment —
  parsed through the context or built from a `Node` — that is a map (statement position) or any node
  (value position), or errors.
- Unknown or disabled directive, wrong position (a statement directive returning a non-map), bad
  arguments — local errors, the directive contributes nothing, expansion continues (§11).
- Statement-position results merge positionally into the enclosing block, so fields below override
  them (§6, §7).
- **Spec gaps:** the rest of the API shape — it is public and hard to change later, so it is reviewed
  with the user before coding: what a marker handler gets and returns, and the fragment type's own
  surface. What a directive handler gets and returns is settled (§6). The builder settles how the
  loader takes its handlers.
- **Spec gaps — cycles, for every directive, not `#include` alone** (raised in session 12,
  `format.md` §13 item 5). A fragment built from a `Node` holds no directive, so every cycle runs
  through the context's way to parse another source, and the framework can keep the stack there for
  every handler. Open: what makes two sources the same — an opaque key passed with the source, by
  default the normalized URI and a file's real path (`..`, symbolic links, case-insensitive file
  systems, `file:` against `jar:`), a handler's own for a source that is no URL; a depth limit as
  a backstop for a key that changes on every step (`?n=1`, `?n=2`, …), and whether it is
  configurable; a source included twice but not through itself (a diamond), presumably allowed, parsed again
  or cached; where the error points — the directive that closes the cycle, with the chain.

### 11. `#include`

- `file` (relative to the including URL), `resource` (classpath), `url`; exactly one of them;
  `optional: true` tolerates a missing target (§6).
- Cycles are caught by step 10's framework; `#include` passes its source's key (step 10's gap).
  Nested includes; includes inside blocks land at the right path.
- Errors inside an included source are collected along with everyone else's.
- **Spec gaps:** whether `file` without an extension gets one appended — `.Ψ`, `.ψ`, `.psi` in
  that order (session 10), first found or every one found merged, as HOCON does;
  which class loader `resource` uses; whether an optional include swallows only "not found" or also
  I/O and parse errors (it should only swallow "not found").

### 12. Error messages and hints

- Every error message names what was found, where, and what to do: `a:b` → put a space after `:`,
  `a: foo bar` → quote the value, a merge over `${…}` → use `:=`.
- `PsiErrors` renders as one readable multi-line report.
- A test file per category asserting the exact texts.

### 13. Hardening

- Tests on realistic configs ported from `impl/hocon` test resources, deep nesting, large files,
  pathological inputs (unterminated constructs at end of file, deeply nested brackets — no stack
  overflow on reasonable depth).
- Review all public types for binary-compatibility hazards before the first release.

### 14. Docs and release

- A Psi page on the docs site (`docs/`), listed in `pages/sidebars.js`, the module added to `pages`.
- `CLAUDE.md` (module layout and a loader section) and `README.md`.
- `format.md`: status block says what is implemented; §1's note on `impl/yaml` is out of date.
- Remove `publish / skip`.
