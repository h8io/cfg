# Psi, YAML and HOCON

```scala mdoc:invisible
import h8io.cfg.impl.psi.Psi
import h8io.cfg.pages.Example
```

Psi takes its ideas from YAML and HOCON — indentation and tags from one, merging, substitutions and includes from the
other — and compatibility from neither ([Why cfg](../motivation.md)). A YAML or HOCON file is not a Psi source, and
most of what one writes differently fails loudly in Psi rather than meaning something else. A file kept in either
format is [imported](directives.md#importing-yaml-and-hocon) as it is.

## At a glance

| | Psi | YAML | HOCON |
|---|---|---|---|
| Comments | `//`, `/* … */` nesting | `#` | `#`, `//` |
| A field | `key: value` | `key: value` | `key: value`, `key = value`, `key { … }` |
| Blocks | braces or indentation, mixed | indentation, or `{…}` in flow | braces |
| Fields on one line | separated by `,` | in flow, by `,` | by `,` |
| A value with spaces | quoted | as written | as written, the words joined |
| Typing | none: every value a string | none in `cfg-yaml`, nulls aside | Typesafe Config's: `1e5` is `100000.0` |
| Null | `null` | `null`, `Null`, `NULL`, `~` | `null` |
| `a.b` as a key | nests | one key | nests |
| A key with a dot in it | `` `a.b` `` | `a.b` | `"a.b"` |
| Tags | `@t`, any node | `!t`, any node | none |
| A key written twice | maps merge, the rest replaced | the last one, in `cfg-yaml` | maps merge, the rest replaced |
| Dropping keys set before | `:=` | — | a value that is no map first |
| Appending to a sequence | `[$a*, x]` | — | `+=`, `${a} [x]` |
| Reuse | `${path}`, `$path*` | anchors and aliases, `<<` | `${path}`, concatenation |
| `${…}` in `"…"` | a substitution | — | text |
| System properties, the environment | `#prop`, `#env`, where written | — | override values, and fill substitutions the tree lacks |
| Includes | `#include(…)`, anywhere | — | `include "…"` |
| Text over several lines | `"""…"""`, indent stripped, escapes; `'''…'''` raw | `\|`, `>` | `"""…"""`, raw, as written |
| Several documents in a file | — | `---` | — |
| Errors | all of them, each where it is | the first, thrown | the first, thrown |
| Input from anyone | safe, within limits | safe in `cfg-yaml` | trusted only |

## Coming from YAML

- **`#` starts a directive**, not a comment: a comment is `//` or `/* … */`.
- **A value with whitespace is quoted**: `name: my service` is an error, `name: "my service"` a value.
- **A tag is `@t`**, not `!t`, and is the name alone in both: `@duration 5s` and YAML's `!duration 5s` give the tag
  `duration`. `!!int`, `tag:yaml.org,2002:int` in YAML, is written ``@`tag:yaml.org,2002:int` `` if it is needed at
  all.
- **`- ` stands deeper than its key**: YAML's `hosts:⏎- a`, the marker in the key's column, is an error. A map element
  is written as in YAML, its first field after the `- `.
- **`null` is the one null**: `~` is the string `~`.
- **`'…'` escapes with a backslash**: `'it\'s'`, where YAML writes `'it''s'`. A `\` in YAML's single quotes is text,
  in Psi's an escape only before `'` or `\`.
- **Block scalars are triple quotes**: YAML's `|` is `"""…"""` or `'''…'''`, its `>` the marker `+`, and its
  indentation indicator `width(n)` ([text over several lines](syntax.md#text-over-several-lines)).
- **Anchors and aliases are substitutions**: `&base` needs nothing, `*base` is `$base`, and the merge key
  `<<: *base` is the injection `$base*`, its fields overridden by those written after it. Unlike an alias, a
  substitution sees the tree as every source left it ([caveats](caveats.md#substitutions-see-the-end)).
- **A key written twice merges** when both are maps, where YAML calls it an error and `cfg-yaml` keeps the last one.
- **What YAML lacks**: substitutions in text, `jdbc:${host}/db`; includes; values set at launch; `:=`.

```scala mdoc:passthrough
Example.psi("""
server:
  name: my service   # the name
  timeout: !duration 5s
hosts:
- a
- b
""")
```

is written in Psi

```scala mdoc:passthrough
Example.psi("""
server:
  name: "my service"   // the name
  timeout: @duration 5s
hosts:
  - a
  - b
""")
```

## Coming from HOCON

- **A field is `key: value`**: `=` is an error. `key { … }` stays.
- **Keys with dots in them go in backticks**: `` `akka.actor`: x ``, where HOCON quotes, `"akka.actor" = x`. Double
  quotes hold values only.
- **A value with whitespace is quoted**: HOCON joins `my service` into one value, Psi reports it.
- **Nothing is typed**: `1e5` stays as written, where Typesafe Config gives `100000.0`. Reading `30s` as a duration
  or `512M` as a size is the program's business, as reading a number is.
- **A substitution goes inside the string**: `"jdbc:${host}/db"` substitutes, HOCON's `"jdbc:"${host}"/db"` is an
  error. Outside quotes `$` always opens one, so a literal `$` is quoted, and the braces may be left out, `$host`.
- **No concatenation of blocks or sequences**: HOCON's `${a} [x]` and `a += x` are written with
  [injections](substitutions.md#injections), `[$a*, x]`, and `${base} { port: 2 }` as `$base*` with `port: 2` after it.
- **No self-reference**: `path: "${path}:/bin"` is a cycle. A value builds on another key, not on its own earlier
  value.
- **Nothing comes from the launch on its own**: no system property overrides a value, and `${HOME}` finds a key `HOME`
  in the tree or nothing. A source says what it takes, `home: #env(HOME)` and `host: #prop("db.host")`
  ([values set at launch](directives.md#values-set-at-launch)).
- **`include` is a directive**: `include "defaults"` is `#include(file: "defaults")*`, and it may stand in any block.
  With no extension it reads the first Psi source it finds, one of `.Ψ`, `.ψ` and `.psi`, where HOCON merges the
  `.conf`, `.json` and `.properties` of that name.
- **A reference is resolved with the application**, not on its own
  ([caveats](caveats.md#a-reference-resolves-with-the-application)).
- **Triple quotes strip the indent and take escapes and substitutions**: HOCON's `"""…"""` is raw and kept as written;
  Psi's `'''…'''` is raw, its indent stripped all the same.
- **`#` starts a directive**: a comment is `//`, as in HOCON, or `/* … */`.

```scala mdoc:passthrough
Example.psi("""
db {
  host = localhost
  url: "jdbc:postgresql://"${db.host}"/orders"
  "pool.size": 16 # per node
}
name: my service
""")
```

is written in Psi, `path = ${HOME}/bin` added, with `HOME` set to `/home/app`

```scala mdoc:passthrough
Example.psi(
  """
db {
  host: localhost
  url: "jdbc:postgresql://${db.host}/orders"
  `pool.size`: 16 // per node
}
name: "my service"
home: #env(HOME)
path: $home/bin
""",
  Psi.builder.withSystemProperties(Map.empty).withEnvironment(Map("HOME" -> "/home/app")).build.toOption.get)
```
