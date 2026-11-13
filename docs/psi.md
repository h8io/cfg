# Psi

Ψ, written *Psi* where a name has to be ASCII, is `cfg`'s own configuration format. It is read by a parser written for
`cfg`, so it keeps what other formats lose on the way: a scalar is exactly the text written in the source, a tag is
there only when the author wrote one, and every node knows the source, line and column it came from. It takes from
HOCON what makes a configuration easy to compose — merging, substitutions, includes — and gives every one of its rules
a single spelling.

```scala
libraryDependencies += "io.h8" %% "cfg-psi" % "@VERSION@"
```

Sources are files named `.psi`, `.ψ` or `.Ψ`, in UTF-8.

## A first source

```scala mdoc:silent
import h8io.cfg.impl.psi.Psi

/** Loads `text` and prints the tree it gives, or the report of its errors. */
def load(text: String): Unit = println(Psi.parse("app.psi", text).fold(_.message, _.toString))
```

`Psi.parse` loads a source held as a string, and `Psi(urls*)` the sources at URLs. Either gives back the tree, an
`h8io.cfg.Node.IMap` read [as any other loader's](README.md), or every error found. A tree prints as Psi:

```scala mdoc
load("""
// a service
server {
  host: localhost
  port: 8080
}
pool.max: 16
tags: [web, api]
""")
```

## Syntax

**Fields** are `key: value`, one a line, or several on a line separated by `,`. Whitespace never separates: `a: 1 b: 2`
is an error, and so is `[x y]`. A trailing `,` is allowed.

**Keys** are bare when they are a letter or `_` followed by letters, digits, `_` and `-`, and are written in backticks
otherwise: `` `spark.executor.memory`: 8g ``. A dotted key nests: `pool.max: 16` is `pool { max: 16 }`, and a key in
backticks is never split.

**Values** are strings; reading one as a number or a duration is up to whoever reads it, so `1e5` stays `1e5` and
`007` stays `007`.

- An unquoted value runs to the first whitespace, `,`, `}` or `]`: `url: http://host:8080/path`. A value with
  whitespace in it is quoted.
- `"…"` takes the escapes `\"`, ``\` ``, `\\`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX` and `\$`.
- `'…'` is raw, as Perl's single quotes: only `\'` and `\\` are escapes, so a regular expression is written as it is:
  `'^\d+$'`.
- `null`, unquoted, is the one reserved word. `true`, `false` and every other word are plain strings.

**Blocks** are `{ … }`, and `key { … }` is `key: { … }`. **Sequences** are `[ … ]`, separated as fields are.

**Comments** are `//` to the end of the line and `/* … */`, which nest. `#` is not a comment: it starts a directive.

**Tags** are names a value carries for whoever reads it, written before the value: `timeout: @duration 5s`. A node's
`tags` hold them.

```scala mdoc
load("""
http { host: 0.0.0.0, port: 8080 }
`odd key`: "a value, with a comma"
pattern: '^\d+$'
timeout: @duration 5s
password: null
""")
```

### Text over several lines

`"""…"""` is a block of text starting on the line after the opening quotes. The indent common to its lines is
stripped, Java's rule, and the newlines just inside the quotes are dropped; `'''…'''` does the same, raw. Markers
after the opening quotes pick another layout: `<` strips all of the indent, `>` keeps it, `width(n: 2)` strips the
indent of the opening line and two more, and `+` folds lines into one, as YAML's `>` does.

```scala mdoc
// a Scala string cannot hold three double quotes: QQQ stands for them
load("""
query: QQQ
  SELECT id, total
  FROM orders
  WHERE status = 'paid'
  QQQ
banner: QQQ+
  folded into
  one line
  QQQ
""".replace("QQQ", "\"\"\""))
```

## Merging

A key written twice is **merged**: maps merge recursively, and anything else — a scalar, a sequence, `null` — is
replaced by the later value. The same rule merges the sources a loader is given, in order, and what `#include` brings
into the block it stands in. `:=` replaces instead of merging, which is the one way to drop keys an earlier definition
gave.

```scala mdoc
load("""
pool { min: 1, max: 10, idle: 30s }
pool { max: 4 }
cache { size: 100, ttl: 1m }
cache := { size: 10 }
""")
```

## Substitutions

`${path}` is replaced by the node at `path`, written as `cfg` writes paths: keys joined by dots, in backticks when they
are not bare, indexes in brackets — `${servers[0].host}`. Substitutions are resolved once every source is merged, so a
value refers to its final state, whichever source set it.

- `${path}` alone, unquoted, grafts the node whatever it is: a map, a sequence, a scalar.
- Inside a string, `jdbc:${host}/db` or `"jdbc:${host}/db"`, it builds a scalar, and the node has to be a scalar.
- `${?path}` is optional: with nothing at `path`, the field is left out, an element of a sequence is dropped, and a
  part of a string is empty.
- `\${` is a literal `${`.

```scala mdoc
load("""
db {
  host: localhost
  url: "jdbc:postgresql://${db.host}:5432/orders"
}
replica: ${db}
user: ${?env.user}
""")
```

**System properties override the sources**: before substitutions are resolved, a value the sources define at
`db.host` is replaced by the system property `db.host`, if there is one. Only values the sources define are
overridden, never a map or a sequence. A loader built with a map of its own reads that instead of `sys.props`, and an
empty map turns overriding off:

```scala mdoc
println(
  Psi.builder
    .withSystemProperties(Map("db.host" -> "db.prod"))
    .build
    .flatMap(_.parse("app.psi", "db { host: localhost, url: \"${db.host}:5432\" }"))
    .fold(_.toString, _.toString))
```

## `#include`

`#include` reads another Psi source where it stands. In a block it merges the source's fields into the block, so the
fields after it override them; as a value it makes the source a map.

| Argument | Reads |
|---|---|
| `file: "conf.d/db"` | a file, relative to the including source |
| `resource: "defaults"` | a resource on the class path |
| `url: "https://…"` | a URL |
| `optional: true` | nothing when the target is not there, rather than an error |

A `file` or `resource` with no extension is looked for as `.Ψ`, `.ψ` and `.psi`, in that order.

```scala mdoc:invisible
// Writes the two files the example below loads, as `app`
val app = {
  val dir = java.nio.file.Files.createTempDirectory("psi")
  def write(name: String, text: String) =
    java.nio.file.Files.write(dir.resolve(name), text.getBytes("UTF-8"))
  write("defaults.psi", "server { host: localhost, port: 8080 }\nlog.level: INFO\n")
  write("app.psi", "#include(file: \"defaults\")\nserver.port: 9090\n#include(file: \"local\", optional: true)\n")
  dir.resolve("app.psi").toUri.toURL
}
```

With `defaults.psi` holding `server { host: localhost, port: 8080 }` and `log.level: INFO`, and no `local.psi`
next to it:

```
#include(file: "defaults")
server.port: 9090
#include(file: "local", optional: true)
```

```scala mdoc
println(Psi(app).fold(_.message, _.toString))
```

A loader for input it does not trust can turn `#include` off: `Psi.builder.disableDirective("include")`.

## Errors

A load reports every error it can find without guessing, each with where it is, what was found and, often, what to do
about it. `PsiErrors.message` is the report:

```scala mdoc
load("""
server {
  host = localhost
  port: 8080 timeout: 5s
}
name: my service
url: "jdbc:${db.host}"
""")
```

Errors in the syntax come first; substitutions are resolved only when there are none, so that a field the syntax lost
is not reported as missing. A program reads an error by its type and fields — `errors` holds them; the texts are for
people and may change in any release.

## Limits

A tree loaded is at most 256 levels deep, counting each `{`, `[` and key after a dot, and resolving a substitution
goes at most 256 steps, each a level descended or a substitution followed. Past either, the load reports an error
rather than overflowing the stack. 256 holds with room to spare on a thread with the JVM's default stack;
`Psi.builder.withDepthLimit(n)` sets another limit, lower for a thread with a smaller stack.

Psi can also be [extended](psi-extending.md) with directives and markers of one's own.
