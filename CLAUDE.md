# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Compile
sbt compile

# Run all tests (cross-compiled)
sbt +test

# Run tests for a single module
sbt cfg/test
sbt schema/test
sbt hocon/test

# Run a single test class
sbt "schema/testOnly h8io.cfg.schema.DecoderTest"

# Format source
sbt scalafmt

# Check formatting without modifying (used in CI)
sbt scalafmtSbtCheck scalafmtCheckAll

# Full CI check (format + cross-compile + coverage)
./test.sh
```

## Architecture

Two-layer design: `cfg` is the low-level protocol (a format-agnostic ADT for config trees); `schema` is the high-level API where direct ADT manipulation is hidden behind decoders and properties.

### Module layout

- **`cfg`** — low-level protocol: core AST and error types. No external dependencies.
- **`schema`** (artifact `cfg-schema`) — high-level API: `Decoder` and `Property` abstractions that hide ADT manipulation. Depends on `cfg` + Cats + `io.h8:reflect`.
- **`impl/hocon`** (artifact `cfg-hocon`) — loads HOCON and YAML into the `cfg` AST using `typesafe-config` + `typesafe-config-yaml`.
- **root** (artifact `cfg-all`) — aggregate; not published.

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
- `Node.IScalar` — a string value with an optional YAML tag and a `Location`
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

`hocon.apply(urls: URL*)` parses and merges the given URLs (HOCON or YAML), loads the result through `ConfigFactory.load` (for substitution resolution), and wraps it as `Node.IMap[Id.Root]`. YAML support comes from `io.h8:typesafe-config-yaml` with `stringsOnly = true` (all scalars become strings). Tags are not set by the HOCON loader (`None` always).

## Conventions

- Scala 2.13 is the primary version; 2.12 is cross-compiled. Use `+` prefix in sbt for cross-compilation.
- Scalafmt dialect is `scala213source3` (Scala 3 syntax allowed in 2.13). Max line length 120.
- Scalac options include `-Xfatal-warnings`; all warnings are errors.
- Use `def` not `val` for implicit/constant definitions.
- Explicit `None` for HOCON tags (never omit or infer).
- Tests use ScalaTest + ScalaMock + ScalaCheck; Cats law tests use `discipline-scalatest`.
