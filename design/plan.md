# DEF — implementation plan

Step-by-step plan for implementing the format specified in [`format.md`](format.md). The work is
large, so it is cut into steps small enough to finish in one session each, and this file is what a
new session reads to find out where things stand.

## How to work with this plan

- **One step, one branch, one PR.** Branch `def/NN-slug` (e.g. `def/03-parser`), PR against `main`.
  A step is done when its PR is merged; the PR flips the step's status below to `done` in the same
  diff, so `main` always shows the true state.
- **Resuming.** Find the first step that is not `done`. If its status is `in progress`, its branch
  and PR exist — check them out (`gh pr list --head def/NN-slug`), read the PR description and the
  step's *Progress* notes, and continue from there. Never start step N+1 before step N is merged
  unless the step says it is independent.
- **A step that turns out too big** is split here, in this file, into `NNa`/`NNb`, and only the first
  part is finished in the current PR. Unfinished work is not left uncommitted at the end of a
  session: push what compiles and passes, and write in *Progress* what is left.
- **Every PR is green on `./test.sh`**: formatting, cross-build for 2.13 and 2.12, and **100%
  statement and branch coverage** — the build enforces it (`ThisBuild / coverageSummary*Threshold`),
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

| Piece | Package member (all under `h8io.cfg.impl.defg`) | Visibility |
|---|---|---|
| Loader | `DEF` | public |
| Locations | `DefLocation`, `SourceLocation`, `ReferenceLocation`, `ConcatenationLocation`, `SystemPropertyLocation`, `EnvLocation` (§10) | public |
| Errors | `DefError` (each with a `location`), `DefErrors` (non-empty, a `CfgError`) (§11) | public |
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

Error accumulation without Cats: a small private result type (value + `List[DefError]`) with `map`,
`flatMap` and a combinator that collects errors from independent parts. `DefErrors` is built from it
at the phase gate.

## Steps

| # | Step | Status |
|---|---|---|
| 0 | This plan | done |
| 1 | Module skeleton, locations, errors | todo |
| 2 | Source and lexer | todo |
| 3 | Ast and parser: core syntax | todo |
| 4 | Parser: substitutions and directives | todo |
| 5 | Parser: error recovery | todo |
| 6 | Merge | todo |
| 7 | Node views and the loader, end to end | todo |
| 8 | Renderer and round-trip property tests | todo |
| 9 | Substitutions | todo |
| 10 | Directive framework | todo |
| 11 | `@include` | todo |
| 12 | Error messages and hints | todo |
| 13 | Hardening | todo |
| 14 | Docs and release | todo |

### 0. This plan

`design/plan.md`, plus a pointer to it in `format.md`.

### 1. Module skeleton, locations, errors

- `build.sbt`: `val defg = (project in file("impl/def"))`, `name := "cfg-def"`, depends on `cfg` only,
  `publish / skip := true`. Add to `root / aggregate` (needed for coverage), **not** to `pages` yet.
- Location hierarchy from §10 with `description` exactly as specified:
  `app.conf:12:9`, and for a reference `app.conf:12:9 (${db.url} from base.conf:3:7)`. Chains render
  recursively.
- `DefError` (sealed, every case has a `location`), `DefErrors` (head + tail, `CfgError`), and the
  private result type. Start with the error cases this step can test; later steps add theirs.
- **Spec gaps:** what `<source>` in a location is — the full URL string, or a shortened form (file
  name, path relative to the including file)? Column counting: code points or UTF-16 units, 1-based
  (the YAML backend is 1-based)?

### 2. Source and lexer

- Read a URL as UTF-8; LF and CRLF are newlines, a lone CR is an error (§3).
- Tokens: `{ } [ ] ( ) , . : := = !tag @name`, newline, comment (skipped), bare key
  (`Id.SafeKeyPattern`), unquoted scalar, quoted string with the escape set of `Id.quote`, triple-quoted
  string (raw, first newline dropped, common indent stripped, §5), `${`/`${?` … `}`.
- Value mode: an unquoted scalar runs to whitespace, `,`, `}`, `]` or `#`; may not start with `'`
  (§3). Key mode: `:` must be followed by whitespace or end of line, so must `:=`.
- Fatal lexical errors (unterminated `"…"`/`"""…"""`) end the source (§11); others are reported and
  lexing continues.
- Property test: for any string `k`, `Id.Key(k, Id.Root).path` lexes in key mode to exactly one key
  token with value `k` — the round-trip promise of §3.
- **Spec gaps:** the character set of a tag name (`!` + what?) and of a directive name; how a literal
  `${` is written — is `${` inside an unquoted scalar always a substitution, and is it literal inside
  quotes (as in HOCON)? Byte order mark at the start of a file — skip or error? Can a tag be
  followed directly by a triple-quoted string?

### 3. Ast and parser: core syntax

- Recursive descent over the lexer: document, block, fields, dotted keys (quoted keys indivisible),
  `:`/`:=`/`key { … }`, `{` on the key's line, sequences, separators (`,`/newline, trailing allowed,
  empty element an error, whitespace never separates), tags on every value, `null` in value position
  only, `a: foo bar` an error.
- In this step the parser stops at the first error; recovery is step 5.
- The Ast keeps operators and source order — merging is step 6.

### 4. Parser: substitutions and directives

- `${path}`, `${?path}`, and concatenations whose parts touch (`"jdbc:"${host}"/db"`); a space between
  parts is an error (§9).
- Directives in statement and value position, arguments `name = literal` separated by `,`/newline,
  literals: strings, bare identifiers, `true`/`false`, integers; `${…}` in an argument is an error
  (§6). The Ast only — nothing is expanded yet.
- **Spec gaps:** the path syntax inside `${…}` — dotted keys with quoted segments as in `Id.path`,
  and are indices (`${a.b[0]}`) allowed? Integer literal syntax (sign, leading zeros). Duplicate
  argument names.

### 5. Parser: error recovery

- Synchronisation points from §11: skip to the next field separator at the same bracket depth or to
  the `}` closing the block; unbalanced brackets at end of input end the source.
- Tests: several independent errors in one file are all reported, a broken field does not hide the
  next one, no cascade errors after a recovered one.

### 6. Merge

- Ast → Tree for one source: duplicate keys merge in order, dotted keys merge into nested blocks,
  `:=` drops the earlier value, block form merges (§4, §7).
- Tags in a merge: the later written tag wins, otherwise the earlier is kept; a replaced value keeps
  only its own tag (§7).
- No merging over a substitution: a `:` merge with a whole-value `${…}` on one side and a map or
  `${…}` on the other is an error suggesting `:=`, at any depth (§7).
- Merging N trees in order — the same function.
- Locations of merged containers: decide (see gaps).
- **Spec gaps:** which `Location` a merged map gets — the first, the last, or a new kind listing all
  definition sites? Key order in a merged map (first occurrence, as the YAML backend does, is the
  natural default).

### 7. Node views and the loader, end to end

- `MapImpl`/`SeqImpl` as thin views over `Map`/`Vector` (§12), `Node.INull` for `null`, `-` on maps,
  ids assigned while wrapping.
- `DEF.apply(urls: URL*): Either[DefErrors, Node.IMap[Id.Root]]`; no URLs gives an empty root map,
  as `YAML` does. The phase gate of §11 is wired here.
- Until steps 9–11 land, a `${…}` or a directive reaching this phase is reported as a
  "not supported yet" error, so the loader is total from the first version.
- Test resources under `impl/def/src/test/resources/*.def`.

### 8. Renderer and round-trip property tests

- Render a Tree (and `MapImpl`/`SeqImpl.toString`) as DEF source: quoted keys and scalars where
  needed, tags, `null`, nested blocks.
- ScalaCheck generator for Trees; property: render → parse → merge gives the same tree. This is the
  main defence against parser corner cases, and it guards §3's `Id.path` promise from the other side.

### 9. Substitutions

- Resolution over the merged Tree (§9): whole-value graft of any node including `null`, `${?…}` drops
  the field, concatenation of scalars only, lookup order tree → system properties → environment.
  System properties and the environment are injected as functions, so tests do not touch the real
  ones.
- Cycles are errors naming the path; self-reference is a cycle (§9).
- Root causes only: a node depending on a failed one is dropped silently (§11).
- Locations (§10): `ReferenceLocation` on every node of a grafted subtree, chains for chained
  references, `ConcatenationLocation` with one entry per `${…}`, `SystemPropertyLocation` and
  `EnvLocation`.
- Remove the "not supported yet" path for `${…}` from step 7.
- **Spec gaps:** `${?x}` as a sequence element — drop the element? `${?x}` as one part of a
  concatenation? The Id of a grafted node is its new position, not its origin — confirm. Which names
  are looked up in system properties and the environment: the path as written (`${java.home}`,
  `${HOME}`), and only for single-segment paths in the environment?

### 10. Directive framework

- The public extension API (§6): a handler gets the literal arguments, the location and a context
  (the including source's URL, the URL stack for cycle detection, a way to parse another source) and
  returns a map (statement position) or a node (value position), or errors.
- Unknown directive, wrong position (a statement directive returning a non-map), bad arguments —
  local errors, the directive contributes nothing, expansion continues (§11).
- Statement-position results merge positionally into the enclosing block, so fields below override
  them (§6, §7).
- **Spec gaps:** the API shape itself — it is public and hard to change later, so it is reviewed with
  the user before coding. Whether the loader takes the handler set as a parameter of `DEF.apply`, of a
  `DEF` instance, or both.

### 11. `@include`

- `file` (relative to the including URL), `resource` (classpath), `url`; exactly one of them;
  `optional = true` tolerates a missing target (§6).
- Cycle detection on the URL stack; nested includes; includes inside blocks land at the right path.
- Errors inside an included source are collected along with everyone else's.
- **Spec gaps:** whether `file` without an extension gets `.def` appended (HOCON tries several);
  which class loader `resource` uses; whether an optional include swallows only "not found" or also
  I/O and parse errors (it should only swallow "not found").

### 12. Error messages and hints

- Every error message names what was found, where, and what to do: `a:b` → put a space after `:`,
  `a: foo bar` → quote the value, `a: 'x'` → use double quotes, a merge over `${…}` → use `:=`.
- `DefErrors` renders as one readable multi-line report.
- A test file per category asserting the exact texts.

### 13. Hardening

- Tests on realistic configs ported from `impl/hocon` test resources, deep nesting, large files,
  pathological inputs (unterminated constructs at end of file, deeply nested brackets — no stack
  overflow on reasonable depth).
- Review all public types for binary-compatibility hazards before the first release.

### 14. Docs and release

- A DEF page on the docs site, `docs/directory.conf` navigation, the module added to `pages`.
- `CLAUDE.md` (module layout and a loader section) and `README.md`.
- `format.md`: status block says what is implemented; §1's note on `impl/yaml` is out of date.
- Remove `publish / skip`.
