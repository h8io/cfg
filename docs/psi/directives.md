# Psi: directives

```scala mdoc:invisible
import h8io.cfg.impl.psi.Psi
import h8io.cfg.pages.Example
```

A directive is `#name(arguments)`, or `#name` with none: a call the loader makes where it stands, which gives a value,
or among fields a block of fields. Psi has three, `#prop`, `#env` and `#include`, and a program can
[add its own](extending.md) or turn one off. Arguments are literals — strings, integers, bare names — positional ones
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
