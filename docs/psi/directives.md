# Psi: directives

```scala mdoc:invisible
import h8io.cfg.impl.psi.Psi
import h8io.cfg.pages.Example
```

A directive is `#name(arguments)`, or `#name` with none: a call the loader makes where it stands, which gives a value,
or among fields a block of fields. Psi has three, `#prop`, `#env` and `#include`, and a program can [add its
own](extending.md) or turn one off. Arguments are literals — strings, integers, bare names — positional ones
first, then named ones, `name: value`. Where a directive stands decides what it may give, as for a substitution:
[injections](substitutions.md#injections) say how.

## Values set at launch

Nothing set at launch reaches a source on its own: no system property overrides a value, and the environment is not
read. A source takes such a value where it says so:

| Call | Gives |
|---|---|
| `#prop("db.host")` | the system property `db.host`; a name with a dot is quoted |
| `#prop(java)` | with no property `java`, a block of every property under `java.`: `version`, `vm { name, … }` |
| `#env(HOME)` | the environment variable `HOME` |
| `…, default: localhost` | the default when it is not set |
| `…, optional: true` | nothing when it is not set, so that the field is left out |

A value not set is an error otherwise. Left out, a field leaves an earlier definition of its key standing, which makes a
default of its own. With `-Ddb.host=db.prod`, and no `DB_PORT` in the environment:

```scala mdoc:passthrough
Example.psi(
  """
db {
  host: localhost
  host: #prop("db.host", optional: true)
  port: #env(DB_PORT, default: 5432)
  url: "${db.host}:${db.port}"
}
""",
  Psi.builder.withSystemProperties(Map("db.host" -> "db.prod")).withEnvironment(Map.empty).build.toOption.get)
```

## Including other sources

`#include` reads another Psi source where it stands. Among fields it merges the source's fields into the block, so
the fields after it override them; as a value it makes the source a block.

| Argument | Reads |
|---|---|
| `file: "conf.d/db"` | a file, relative to the source that includes it, |
| `resource: "defaults"` | a resource on the program's class path |
| `url: "https://…"` | a URL |
| `optional: true` | nothing when the target is not there, rather than an error |

A `file` or `resource` with no extension is looked for as `.Ψ`, `.ψ` and `.psi`, in that order. A URL that takes
more than 10 seconds to connect, or 30 to send its next bytes, is an error. Every include is read again, so the limits
on reading below count it each time.

```scala mdoc:invisible
// Writes the two files of the example below, and loads the second
val included = {
  val dir = java.nio.file.Files.createTempDirectory("psi")
  def write(name: String, text: String) = java.nio.file.Files.write(dir.resolve(name), text.getBytes("UTF-8"))
  write("defaults.psi", "server { host: localhost, port: 8080 }\nlog.level: INFO\n")
  write("app.psi", "#include(file: \"defaults\")\nserver.port: 9090\n#include(file: \"local\", optional: true)\n")
  Psi(dir.resolve("app.psi").toUri.toURL).fold(_.message, _.toString)
}
```

With `defaults.psi`:

```
server { host: localhost, port: 8080 }
log.level: INFO
```

and no `local.psi` next to it,

```
#include(file: "defaults")
server.port: 9090
#include(file: "local", optional: true)
```

```scala mdoc:passthrough
println("loads as\n\n```\n" + included.trim + "\n```")
```

## Importing YAML and HOCON

Two modules of their own give a loader directives that read a YAML or a HOCON file where they stand, as `#include`
reads a Psi one: `cfg-psi-yaml` and `cfg-psi-hocon`. Each is registered under the name the files call it by:

```scala
libraryDependencies += "io.h8" %% "cfg-psi-yaml" % "@VERSION@"

val loader = Psi.builder.withDirective("yaml", h8io.cfg.impl.psi.yaml.Yaml).build
```

- The target is written as `#include`'s: `file`, `resource` or `url`, and `optional`. A file or a resource with no
  extension is looked for as `.yaml` and then `.yml` by `#yaml`, as `.conf`, `.json` and `.properties` by `#hocon`,
  the first found read.
- What it gives is the file's whole tree, every value a string. A YAML file may hold any node at its root, a sequence
  or a single value as well as a mapping, and keeps the tags written in it.
- A HOCON file is resolved on its own: its substitutions inside it, with no system property, no environment variable
  and no `reference.conf` reaching it. A value set at launch comes in by `#prop` and `#env` only, as everywhere in Psi.
- Psi's substitutions refer to what an import gives as to anything else, once it is merged; they do not reach into the
  file while it is read.
- A file that cannot be read or parsed is the directive's error, with what the reader said of it.
- A YAML file is read under the limits on reading below. A HOCON file, and what it includes, Typesafe Config reads
  itself, past them, and with no limit on how deep it goes: import HOCON you trust.

```scala mdoc:invisible
// Writes the files of the example below, and loads the Psi one
val imported = {
  val dir = java.nio.file.Files.createTempDirectory("psi")
  def write(name: String, text: String) = java.nio.file.Files.write(dir.resolve(name), text.getBytes("UTF-8"))
  write("db.yaml", "host: localhost\nport: !int 5432\n")
  write("hosts.yaml", "- a.example\n- b.example\n")
  write("cache.conf", "size: 100\nttl: ${size}s\n")
  write("app.psi", "db: #yaml(file: db)\nhosts: #yaml(file: hosts)\ncache: #hocon(file: cache)\ndb.host: db.prod\n")
  Psi.builder
    .withDirective("yaml", h8io.cfg.impl.psi.yaml.Yaml)
    .withDirective("hocon", h8io.cfg.impl.psi.hocon.Hocon)
    .build
    .flatMap(_(dir.resolve("app.psi").toUri.toURL))
    .fold(e => e.toString, _.toString)
}
```

With `db.yaml` holding `host: localhost` and `port: !int 5432`, `hosts.yaml` the list `- a.example` and
`- b.example`, and `cache.conf` holding `size: 100` and `ttl: ${size}s`:

```
db: #yaml(file: db)
hosts: #yaml(file: hosts)
cache: #hocon(file: cache)
db.host: db.prod
```

```scala mdoc:passthrough
println("loads as\n\n```\n" + imported.trim + "\n```")
```
