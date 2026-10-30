# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Compile
sbt compile

# Run all tests on 2.13, 2.12 and 3.9
sbt +testFull

# Run tests for a single module
sbt cfg/testFull
sbt schema/testFull
sbt hocon/testFull
sbt yaml/testFull
sbt defg/testFull

# Run a single test class
sbt "schema/testOnly h8io.cfg.schema.DecoderTest"

# Format source
sbt scalafmt

# Check formatting without modifying (used in CI)
sbt "scalafmtSbtCheck; scalafmtCheckAll"

# Full CI check (format + cross-compile + coverage + docs + site)
./test.sh

# Compile the docs' `scala mdoc` blocks, into pages/target/mdoc
sbt pages/mdoc

# Render pages/target/mdoc with Docusaurus, into pages/target/site
(cd pages && npm ci && npm run build)

# Live preview; pair with `sbt "pages/mdoc --watch"`
(cd pages && npm start)
```

## Architecture

Two-layer design: `cfg` is the low-level protocol (a format-agnostic ADT for config trees); `schema` is the high-level API where direct ADT manipulation is hidden behind decoders and properties.

### Module layout

- **`cfg`** — low-level protocol: core AST and error types. No external dependencies.
- **`schema`** (artifact `cfg-schema`) — high-level API: `Decoder` and `Property` abstractions that hide ADT manipulation. Depends on `cfg` + Cats + `io.h8:reflect`.
- **`impl/hocon`** (artifact `cfg-hocon`) — loads HOCON and YAML into the `cfg` AST using `typesafe-config` + `typesafe-config-yaml`.
- **`impl/yaml`** (artifact `cfg-yaml`) — loads YAML into the `cfg` AST using `snakeyaml-engine` directly, propagating tags.
- **`impl/def`** (sbt project `defg`, artifact `cfg-def`, package `h8io.cfg.impl.defg`) — DEF, the project's own format
  with a hand-written parser and no dependencies beyond `cfg`. Being built step by step: the spec is `design/format.md`,
  the plan with each step's status is `design/plan.md`. `publish / skip` until the plan's last step.
- **root** (artifact `cfg-all`) — aggregate; not published.
- **pages** — documentation site (sources in `docs/`); not aggregated in root. Two halves: sbt runs mdoc and unidoc,
  then Docusaurus (`pages/package.json`, Node ≥ 20) renders mdoc's output. The navigation is `pages/sidebars.js` — a new
  page must be listed there. Docs are CommonMark, not MDX (`markdown.format: 'detect'`), and broken links fail the
  build.

### Core AST (`h8io.cfg`)

`Id` models a path into the config tree:
- `Id.Root` — top of the tree
- `Id.Key(key, parent)` — a named field in a map
- `Id.Index(index, parent)` — an element in a sequence

`Node` is the tree itself:
- `INode[I]` — the base trait, parameterised by the `Id` type at this position
- `Node.ISome` — present nodes (Null, Scalar, Map, Seq)
- `Node.INone` — a missing key; also a `NodeError`
- `Node.INull` — an explicit null value; also a `NodeError`
- `Node.IScalar` — a string value with its tags and a `Location`

Every `ISome` carries `tags: Set[String]` — the tags written in the source, by name only (`@x` in DEF, `!x` in YAML both
give `x`), empty when none was written. `Id.path` renders a key that is not bare in backticks, as DEF writes it.
- `Node.IMap` / `Node.ISeq` — container nodes; function-like (`key => INode`)

`CfgError` / `NodeError` are traits for all error types; nodes that represent errors implement them directly.

### Decoding layer (`h8io.cfg.schema`)

`Decoder[+T]` is a type alias for `Node.Value => Validated[CfgError, T]`. The `Decoder` companion provides:
- `Decoder[T](node)` — summons the implicit decoder and catches non-fatal exceptions as `Decoder.Thrown`
- `>=>` — monadic sequencing (Kleisli-style)
- `||` — alternative: try left, fall back to right, merging errors as `OrError`
- `Monad[Decoder]` and `SemigroupK[Decoder]` instances

`BaseDecoder[T]` is a convenience base class that dispatches `apply` to `parse(scalar)`, `parse(map)`, or `parse(seq)`; override only the cases you support.

`Property[+T]` is `Node.Map => Validated[CfgError, T]` with a `name`. The two concrete implementations are:
- `MandatoryProperty[T]` — looks up `name` and decodes; `INone` and `INull` are errors
- `OptionalProperty[T]` — same but `INone` returns `None` and wraps result in `Option`

Error algebra in `h8io.cfg.schema.errors`:
- `&` on `CfgError` builds an `AndError` (collect-all, used when decoding multiple fields in parallel via `Applicative`)
- `|` builds an `OrError` (tried-alternatives)

Built-in decoders (all `implicit`, mixed in via `decoders` package object):
- `string`, `primitives` (Boolean, numeric types), `numbers`, `time`, `collections` (Vector, Map)

### HOCON loader (`h8io.cfg.impl.hocon`)

`hocon.apply(urls: URL*)` parses and merges the given URLs (HOCON or YAML), loads the result through `ConfigFactory.load` (for substitution resolution), and wraps it as `Node.IMap[Id.Root]`. YAML support comes from `io.h8:typesafe-config-yaml` with `stringsOnly = true` (all scalars become strings). Tags are not set by the HOCON loader (`tags` always empty).

### YAML loader (`h8io.cfg.impl.yaml`)

`YAML.apply(urls: URL*)` composes each URL with `snakeyaml-engine` (YAML 1.2 core schema, so `~`/`Null`/`NULL` are nulls and `<<` merge keys work), overlays them in order, and wraps the result as `Node.IMap[Id.Root]`. Mappings merge recursively; scalars and sequences are replaced outright. No substitution step. A root that is not a mapping is an error; duplicate keys in a mapping collapse onto the last one.

Unlike the HOCON loader this one sets `tags` — but only when the tag was *written* in the source, expanded through the tag handles in effect and without its leading `!` (`!!int` → `tag:yaml.org,2002:int`, `!postgres` and `!<postgres>` → `postgres`); at most one, YAML allowing one per node. A tag the resolver inferred, and the non-specific `!`, both give the empty set. `tagOf` reads this off `Node.isResolved` (public since snakeyaml-engine 3.2); `copyOf` must carry the flag over when it rebuilds a merged mapping.

## sbt 2

sbt 2 differs from sbt 1 in ways that matter here:
- `sbt` starts a thin client that keeps a server running in the background. The client takes one command line, so
  several commands as separate arguments fail to parse; scripts run `sbt --server`, which works like sbt 1.
- `test` is incremental: it skips suites that passed before and have not changed since, even after `clean`. `testFull`
  runs everything.
- Task results, compilation included, are cached on disk machine-wide (`~/.cache/sbt`). A cached compilation bypasses
  the scoverage compiler plugin, so its instrumentation data is never written; `cleanFull` empties that cache (and
  `target`) and is how `test.sh` starts. CI starts with an empty cache anyway.
- Bare settings in `build.sbt` apply to every project, which is what `ThisBuild` used to be for. sbt-dynver's keys are
  the exception: the plugin reads them from `ThisBuild`.
- Every project's `target` lives under the root's `target/out`. `pages` writes mdoc and unidoc output to
  `pages/target` instead, where Docusaurus and `release.sh` read it; `pages/clean` empties it, `cleanFull` does not.

## Conventions

- Scala 2.13 is the main version and the priority, being the one native to current Spark; it is cross-built to 2.12 and
  to Scala 3.9 (LTS). Use `+` prefix in sbt for cross-compilation. Cross-building goes through `crossScalaVersions` and
  `+`, not `projectMatrix`. All three share one source tree, written in Scala 2 style that Scala 3 also accepts;
  idiomatic Scala 3 (`enum`, `extension`, no package objects) is planned for later, with version-specific source
  directories. Every branch on the Scala version in `build.sbt` lists the versions it knows and ends in
  `unsupportedScala`. kind-projector is a compiler plugin on 2.x only.
- Warnings are fatal on every version (`-Xfatal-warnings` on 2.x, `-Werror` on 3), except Scala 3's notice that a
  trailing `_` for eta-expansion is unnecessary — Scala 2 needs it. Scala 3 takes `-Wall`, and Scala 2 its counterparts,
  including discarded values (`-Wvalue-discard`, `-Wnonunit-statement`; `-Ywarn-value-discard` on 2.12). Tests and the
  docs' mdoc snippets silence those two through `-Wconf` (`lenientScalacOptions` in `build.sbt`): a ScalaMock
  expectation, a ScalaTest assertion or an expression shown with its result is a statement whose value is dropped on
  purpose. Tests on Scala 3 also silence the initialization checker, which cannot follow ScalaTest's registration of
  tests during construction.
- Scala 3 idioms in tests: a context bound is a `using` parameter, and 2.x has no `using` at the call site, so tests put
  the instance in implicit scope instead of passing it positionally (named after the built-in one it replaces, so that
  it shadows it on 2.x). A type test on a node is written `case m: Node.IMap[?]`; Scala 3 cannot check
  `Node.IMap[Id.Key]` against an `INode[Id.Key]`. Interpolating a non-`String` takes an explicit `.toString`.
- The site's API docs are built for every version, `scala-3`, `scala-2.13` and `scala-2.12` under `api/`, and the
  navbar's API menu lists them by hand.
- Write Scala 3 syntax. The scalafmt dialect is `scala213source3` and scalac runs with `-Xsource:3`, so use
  `import a.b.*` for wildcards and `import a.b.{C as D}` for renames — never `._` or `{C => D}`. Max line length 120.
- Scalac options include `-Xfatal-warnings`; all warnings are errors.
- Use `def` not `val` for implicit/constant definitions.
- Explicit `Set.empty` for HOCON tags (never omit or infer). Tests match no tags with a stable `NoTags`, since
  `Set.empty` cannot stand in a pattern.
- Tests use ScalaTest + ScalaMock + ScalaCheck; Cats law tests use `discipline-scalatest`.
