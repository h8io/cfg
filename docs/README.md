# cfg

`cfg` loads configuration files into one tree, the same whatever the format they were written in: maps, sequences,
scalars and explicit nulls, each node knowing its path in the tree and where in the source it came from.

Three loaders build it: one for YAML, one for HOCON, and one for Psi, a format of `cfg`'s own. How it came to three,
and to a format of its own, is [Why cfg](motivation.md).

## Modules

| Artifact | What it holds |
|---|---|
| `cfg` | [The tree](tree.md) itself — `Node`, `Id`, `Location` and the error traits. No dependencies. |
| `cfg-yaml` | [A YAML loader](yaml.md) on `snakeyaml-engine`. Keeps the tags written in the source. |
| `cfg-hocon` | [A loader on Typesafe Config](hocon.md) for HOCON, JSON, `.properties` and YAML, with HOCON's substitutions. |
| `cfg-psi` | [A loader for Psi](psi/README.md): merging, substitutions and includes, formatters, tags, errors collected rather than thrown, and no dependencies. |
| `cfg-psi-yaml`, `cfg-psi-hocon` | [Directives](psi/directives.md#importing-yaml-and-hocon) that import a YAML or a HOCON file into Psi. |

Every loader depends on `cfg`, so one of them is all a project needs:

```scala
libraryDependencies += "io.h8" %% "cfg-yaml" % "@VERSION@"
```

Every artifact is built for Scala 2.13 and 3.

## Which loader

| | `cfg-yaml` | `cfg-hocon` | `cfg-psi` |
|---|---|---|---|
| Formats | YAML | HOCON, JSON, `.properties`, YAML | Psi, importing YAML and HOCON through `cfg-psi-yaml` and `cfg-psi-hocon` |
| A scalar is the text written | yes | YAML only: HOCON's parser types values first | yes |
| Tags | written ones | none | written ones |
| Substitutions, includes | no | yes | yes, and directives of a program's own |
| Errors | thrown, the first one | thrown, the first one | collected, every one |
| Input it can take | anyone's | trusted only | anyone's |
| Dependencies | `snakeyaml-engine` | Typesafe Config, `typesafe-config-yaml` | none |

`cfg-psi` and `cfg-yaml` limit how deep input nests, 256 levels, so that none overflows the stack. `cfg-hocon` is for
configuration you trust, as Typesafe Config is: it parses and resolves by recursion with no limit (see
[HOCON](hocon.md)). So is a HOCON file Psi imports with `cfg-psi-hocon`, which Typesafe Config reads.

## Reading the docs

- [Why cfg](motivation.md) — how the project came to be what it is.
- [The tree](tree.md) — what every loader gives, and how a program reads it.
- [YAML](yaml.md) and [HOCON](hocon.md) — the two loaders on other libraries.
- [Psi](psi/README.md) — the format, page by page, then [loading it](psi/api.md) and [extending it](psi/extending.md)
  from a program; [importing YAML and HOCON](psi/directives.md#importing-yaml-and-hocon) into it.
