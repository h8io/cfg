# AGENTS.md

Guidance for coding agents working in this repository. Claude Code and Gemini read it through `CLAUDE.md` and
`GEMINI.md`, which import it; anything specific to one agent goes in that agent's own file, everything shared goes here.

## Commands

```bash
# Compile
sbt compile

# Run all tests on 2.13 and 3.9
sbt +testFull

# Run tests for a single module
sbt cfg/testFull
sbt hocon/testFull
sbt yaml/testFull
sbt psi/testFull
sbt psiYaml/testFull
sbt psiHocon/testFull

# Run a single test class
sbt "yaml/testOnly h8io.cfg.impl.yaml.YAMLTest"

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

### Checking a change

Run commands from the repository root.

- Start with the affected module, then `sbt +testFull` for anything shared across Scala versions.
- Every PR changing `impl/psi` is green on `./test.sh`, which holds it to 100% statement and branch coverage.
- `impl/psi/src/test/resources/examples/` holds worked examples, each a directory of sources with what they load as,
  written back, in `expected.psi`, or their report in `expected.errors` (`ExamplesTest`). A change to `Render` or to a
  message updates them, and a new feature of the format gets one.
- A change to agent instructions alone needs its links and diff checked, not the Scala tests.
- Report what was checked, and any check that could not be run.

## Working on Ψ (Psi)

Before changing `impl/psi`, read the relevant sections of `design/format.md`, the design notes: it defines the
semantics, lists what is open (§13), and logs how each decision was reached (§14), so a question already argued
through is not reopened. A change to the format is decided with the user before it is coded, and recorded there, in
its section and in §14. The user's guide, `docs/psi/` (one page a topic, traps in `caveats.md`), and the
programmer's, `docs/psi/api.md` and `docs/psi/extending.md`, follow every change they describe. `design/plan.md` is
how the module was built, step by step; every step is done.

## Architecture

`cfg` is the protocol: a format-agnostic ADT for config trees, which the loaders in `impl/` produce.

### Module layout

- **`cfg`** — low-level protocol: core AST and error types. No external dependencies.
- **`impl/hocon`** (artifact `cfg-hocon`) — loads HOCON and YAML into the `cfg` AST using `typesafe-config` + `typesafe-config-yaml`.
- **`impl/yaml`** (artifact `cfg-yaml`) — loads YAML into the `cfg` AST using `snakeyaml-engine` directly, propagating tags.
- **`impl/psi`** (sbt project `psi`, artifact `cfg-psi`, package `h8io.cfg.impl.psi`) — Ψ (Psi), the project's own
  format, with a hand-written parser and no dependencies beyond `cfg`; files `.Ψ`, `.ψ` or `.psi`. Published from
  v0.1.0; its design notes are `design/format.md`.
- **`impl/psi-yaml`**, **`impl/psi-hocon`** (sbt projects `psiYaml`, `psiHocon`, artifacts `cfg-psi-yaml`,
  `cfg-psi-hocon`, packages `h8io.cfg.impl.psi.yaml`, `h8io.cfg.impl.psi.hocon`) — Psi's `#yaml` and `#hocon`, which
  import a file through `cfg-yaml`'s `YAML.parse` and `cfg-hocon`'s `HOCON.isolated`; apart, so that neither Psi nor
  the loaders depend on more (`format.md` §6).
- **root** (artifact `cfg-all`) — aggregate; not published.
- **pages** — documentation site (sources in `docs/`, one page a topic); not aggregated in root. Its own sources,
  `pages/src`, hold what the docs' mdoc blocks share (`Example.psi` shows a Psi source and what it loads as), and are
  left out of the API docs. Two halves: sbt runs mdoc and unidoc, then Docusaurus (`pages/package.json`, Node ≥ 20)
  renders mdoc's output. The navigation is `pages/sidebars.js` — a new
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
- `Node.IScalar` — a string value with its tag and a `Location`
- `Node.IMap` / `Node.ISeq` — container nodes, looked up by a bare key or index (`map("port")`, `seq(0)`); the
  container builds the child's `Id` from its own, so a child, `INone` included, always sits directly below it. `INone`
  carries only its `Id`, not the container it was missing from.

Every `ISome` carries `tag: Option[String]` — the tag written in the source, by name only (`@x` in Psi, `!x` in YAML both
give `x`), `None` when none was written; a value has one at most. `Id.path` renders a key that is not bare in backticks,
as Psi writes it.

`CfgError` / `NodeError` are traits for all error types; nodes that represent errors implement them directly.

### HOCON loader (`h8io.cfg.impl.hocon`)

`HOCON.apply(urls: URL*)` parses and merges the given URLs (HOCON or YAML), loads the result through `ConfigFactory.load` (for substitution resolution), and wraps it as `Node.IMap[Id.Root]`. YAML support comes from `io.h8:typesafe-config-yaml` with `stringsOnly = true` (all scalars become strings). An explicitly passed URL that cannot be read is an error, whatever its syntax; an `include` inside one stays optional unless written `include required(…)`. With no URLs it loads the standard application config instead: system properties, `application.*` and `reference.conf` from the classpath. Tags are not set by the HOCON loader (`tag` always `None`). It is for trusted input only: Typesafe Config recurses with no limit, and deep input overflows the stack (`format.md` §13 item 10).

### YAML loader (`h8io.cfg.impl.yaml`)

`YAML.apply(urls: URL*)` reads each URL with timeouts, 10 s to connect and 30 s for each wait for bytes, composes it with `snakeyaml-engine` (YAML 1.2 core schema, so `~`/`Null`/`NULL` are nulls and `<<` merge keys work), overlays them in order, and wraps the result as `Node.IMap[Id.Root]`. Mappings merge recursively, a pair of them once, its result shared, as aliases share nodes; scalars and sequences are replaced outright. No substitution step. Each file is checked on its own before any overlaying: a root that is not a mapping is an error, and so is a node that an alias makes contain itself (an anchor reused elsewhere is fine); keys that are not scalars snakeyaml-engine refuses itself. Duplicate keys in a mapping collapse onto the last one. A file nests at most 256 levels, aliases followed (`DepthLimit`): `Bounded` counts the parser's events, so that the composer, which recurses, never goes deeper, and `validated` measures the tree as composed.

Unlike the HOCON loader this one sets `tag` — but only when the tag was *written* in the source, expanded through the tag handles in effect and without its leading `!` (`!!int` → `tag:yaml.org,2002:int`, `!postgres` and `!<postgres>` → `postgres`); at most one, YAML allowing one per node. A tag the resolver inferred, and the non-specific `!`, both give `None`. `tagOf` reads this off `Node.isResolved` (public since snakeyaml-engine 3.2); `copyOf` must carry the flag over when it rebuilds a merged mapping. `render`, the views' `toString`, makes the tag of every node not resolved explicit in the serializer's events (`Written`), the serializer itself leaving out any tag its resolver, the core schema as the loader's, would infer.

### Psi loader (`h8io.cfg.impl.psi`)

`Psi.apply(urls: URL*)` and `Psi.parse(name, text)` load Psi sources into `Either[PsiErrors, Node.IMap[Id.Root]]`;
`Psi.standard()` loads every `reference` on the class path and the first `application`, and `withReferences(urls*)`
the references and the URLs given (`format.md` §8); `Psi.builder` builds a `PsiLoader` (from a `Settings`) with
directives, markers and formatters (`${path; name(…)}`) of its own, each registered under the name the files call it
by (`withDirective(name, handler)`), a built-in one disabled, the system properties and the environment `#prop` and
`#env` read from maps rather than `sys.props` and `sys.env`, or other limits, and `build` reports every error of that configuration at once. A load runs in phases (`format.md` §8):
each source is lexed and parsed (`Lexer`, `Parser` → `AST`) and its directives expanded (`Expand`), `#include`, `#prop`
and `#env` among them (`Include`, `Launch`), then the sources are merged (`Merge` → `Tree`; a map keeps what follows its
first injection as `steps`) and their substitutions resolved (`Resolve`, which merges each injection's map by `Merge`'s
rules); nothing set at launch overrides a value; `MapImpl` and `SeqImpl` are the `Node` views over the result, and
`Render` writes a tree back as Psi, which is their `toString`.

Errors are collected, never thrown: every `PsiError` carries a `PsiLocation`, a `message` and a `hint`, computed from
its fields; `PsiErrors.message` is the report, grouped by source. Errors of the syntax and of directives come as one
batch, and substitutions are resolved only when it is empty. A scalar is the text written, a tag is there only when
written, and nothing is typed. Depth is limited (128 by default, `withDepthLimit`) in the tree loaded and in
resolution, so that no input overflows the stack; nothing on a path from the input recurses past the limit, and a
path is written with `Message.path`, never `Id.path`, which recurses. What resolution adds is limited too (1 000 000
nodes grafts copy, 10 000 000 characters concatenations build, `withSizeLimit`), counted before it is built, and so is
reading the sources of a load (10 000 000 bytes, every read counted, and 60 seconds, `withReadLimit`).

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

- Scala 2.13 is the main version and the priority, being the one native to current Spark; it is cross-built to Scala 3.9
  (LTS). Use `+` prefix in sbt for cross-compilation. Cross-building goes through `crossScalaVersions` and `+`, not
  `projectMatrix`. Both share one source tree, written in Scala 2 style that Scala 3 also accepts; idiomatic Scala 3
  (`enum`, `extension`, no package objects) is planned for later, with version-specific source directories. Every branch
  on the Scala version in `build.sbt` lists the versions it knows and ends in `unsupportedScala`. kind-projector is a
  compiler plugin on 2.13 only.
- Warnings are fatal on every version (`-Xfatal-warnings` on 2.13, `-Werror` on 3), except Scala 3's notice that a
  trailing `_` for eta-expansion is unnecessary — Scala 2 needs it. Scala 3 takes `-Wall`, and Scala 2 its counterparts,
  including discarded values (`-Wvalue-discard`, `-Wnonunit-statement`). Tests and the docs' mdoc snippets silence those
  two through `-Wconf` (`lenientScalacOptions` in `build.sbt`): a ScalaMock expectation, a ScalaTest assertion or an
  expression shown with its result is a statement whose value is dropped on purpose. Tests on Scala 3 also silence the
  initialization checker, which cannot follow ScalaTest's registration of tests during construction.
- Scala 3 idioms in tests: a context bound is a `using` parameter, and 2.13 has no `using` at the call site, so tests
  put the instance in implicit scope instead of passing it positionally (named after the built-in one it replaces, so
  that it shadows it on 2.13). A type test on a node is written `case m: Node.IMap[?]`; Scala 3 cannot check
  `Node.IMap[Id.Key]` against an `INode[Id.Key]`. Interpolating a non-`String` takes an explicit `.toString`.
- The site's API docs are built for every version, `scala-3` and `scala-2.13` under `api/`, and the navbar's API menu
  lists them by hand.
- Write Scala 3 syntax. The scalafmt dialect is `scala213source3` and scalac runs with `-Xsource:3`, so use
  `import a.b.*` for wildcards and `import a.b.{C as D}` for renames — never `._` or `{C => D}`. Max line length 120.
- Scalac options include `-Xfatal-warnings`; all warnings are errors.
- A constant is a `def` when calling it creates no object — an `Int`, a `Long`, a `String` literal — and a `val`
  when it would create one on every call: a collection, a `Duration`, a loader, a function.
- Explicit `None` for HOCON tags (never omit or infer).
- Tests use ScalaTest + ScalaMock + ScalaCheck.
