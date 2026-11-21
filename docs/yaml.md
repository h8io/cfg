# YAML

`cfg-yaml` loads YAML with [snakeyaml-engine](https://bitbucket.org/snakeyaml/snakeyaml-engine), keeping what was
written: a scalar is its text and a tag is there when the author wrote one.

```scala
libraryDependencies += "io.h8" %% "cfg-yaml" % "@VERSION@"
```

```scala
import h8io.cfg.impl.yaml.YAML

val cfg = YAML(defaults, local) // a Node.IMap[Id.Root]
```

## What it reads

- YAML 1.2 with the core schema: `~`, `null`, `Null` and `NULL` are nulls, and `<<` merge keys work.
- Each source is one document, rooted at a mapping. Keys are scalars.
- A key repeated in a mapping keeps its last value.
- An alias shares its anchor's node; one that makes a node contain itself is an error.
- No substitutions and no includes: a file is taken as written.

## Overlaying

`YAML(urls*)` reads the sources in order, each checked on its own first, and overlays them: mappings merge key by key,
recursively, and a scalar or a sequence is replaced by the later one outright. With no URLs, the tree is an empty
map.

`YAML.parse(name, bytes)` composes one document already read, its root any node, a sequence or a scalar as well as a
mapping, and its locations named `name`. It is what Psi's `#yaml` imports
([Directives](psi/directives.md#importing-yaml-and-hocon)).

## Tags

`tag` is the tag written on a node, expanded through the tag handles in effect, without its leading `!`:

| Written | `tag` |
|---|---|
| `!postgres x` | `Some("postgres")` |
| `!<postgres> x` | `Some("postgres")` |
| `!!int 5` | `Some("tag:yaml.org,2002:int")` |
| `5` | `None`: the resolver's `int` was not written |
| `! 5` | `None`: the non-specific tag says nothing |

A tag converts nothing: `!!int 5` is the string `5`, tagged.

## Errors and limits

A source that cannot be parsed, holds more than one document, or breaks a rule above throws a `YamlEngineException`;
one that cannot be read, an `IOException`. Every source is checked before anything is overlaid.

Input from anyone is safe to load:

- A source nests at most 256 levels, aliases followed, so that no input overflows the stack.
- A URL that takes more than 10 seconds to connect, or 30 to send its next bytes, fails the load.
- A stream holds at most snakeyaml-engine's 3 MB of code points.
