# Psi

Ψ, written *Psi* where a name has to be ASCII, is `cfg`'s own configuration format. It is read by a parser written for
`cfg`, so it keeps what other formats lose on the way: a value is exactly the text written, a tag is there only when
the author wrote one, and every error points at the source, line and column it is about. It takes from HOCON what makes
a configuration easy to compose — merging, substitutions, includes — and gives every one of its rules a single
spelling; YAML and HOCON files can be imported into it. Why it exists at all is [Why cfg](../motivation.md).

Sources are files named `.psi`, `.ψ` or `.Ψ`, in UTF-8.

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

## A first source

```scala mdoc:passthrough
Example.psi("""
// a service
server:
  host: localhost
  port: 8080
pool.max: 16
tags: [web, api]
""")
```

What a source loads as is shown here, and on every page after this one, written back as Psi: the same tree, every
value a string, written with [indentation](syntax.md#indentation).

## Writing Psi

- [Syntax](syntax.md) — fields, keys and values, blocks, comments and tags; indentation; text over several lines.
- [Merging](merging.md) — what a key written twice gives, and how sources combine.
- [Substitutions](substitutions.md) — referring to another part of the tree, injecting its fields or elements, and
  formatting it as text.
- [Directives](directives.md) — values set at launch, `#prop` and `#env`; including other sources; importing YAML and
  HOCON.
- [Errors and limits](errors.md) — what a load reports, and how far it goes.
- [Caveats](caveats.md) — what is easy to get wrong.

## From a program

```scala
libraryDependencies += "io.h8" %% "cfg-psi" % "@VERSION@"
```

- [Loading](api.md) — loading sources, reading errors, and building a loader with limits of its own.
- [Extending](extending.md) — directives, markers and formatters of a program's own.
