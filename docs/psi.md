# Psi

Ψ, written *Psi* where a name has to be ASCII, is `cfg`'s own configuration format. It is read by a parser written for
`cfg`, so it keeps what other formats lose on the way: a value is exactly the text written, a tag is there only when
the author wrote one, and every error points at the source, line and column it is about. It takes from HOCON what makes
a configuration easy to compose — merging, substitutions, includes — and gives every one of its rules a single
spelling.

This page is about writing Psi. Loading it from a program, and extending it, is [Psi: API](psi-api.md).

Sources are files named `.psi`, `.ψ` or `.Ψ`, in UTF-8.

```scala mdoc:invisible
import h8io.cfg.impl.psi.{Psi, PsiLoader}

/** Shows `source` and what it loads as, a tree written back as Psi or the report of its errors. Three double quotes,
  * which a Scala string cannot hold, are written QQQ.
  */
def example(source: String, loader: PsiLoader = Psi.builder.withSystemProperties(Map.empty).build.toOption.get): Unit = {
  val text = source.replace("QQQ", "\"\"\"").trim
  val (verb, result) = loader.parse("app.psi", text).fold(e => ("reports", e.message), r => ("loads as", r.toString))
  println("```\n" + text + "\n```\n\n" + verb + "\n\n```\n" + result.trim + "\n```")
}
```

## A first source

```scala mdoc:passthrough
example("""
// a service
server {
  host: localhost
  port: 8080
}
pool.max: 16
tags: [web, api]
""")
```

What a source loads as is shown here written back as Psi: the same tree, every value a string, every map a block.

## Syntax

**Fields** are `key: value`, one a line, or several on a line separated by `,`. Whitespace never separates: `a: 1 b: 2`
is an error, and so is `[x y]`. A trailing `,` is allowed.

**Keys** are bare when they are a letter or `_` followed by letters, digits, `_` and `-`, and are written in backticks
otherwise: `` `spark.executor.memory`: 8g ``. A dotted key nests: `pool.max: 16` is `pool { max: 16 }`, and a key in
backticks is never split.

**Values** are strings. Reading one as a number or a duration is up to the program that reads it, so `1e5` stays
`1e5` and `007` stays `007`.

- An unquoted value runs to the first whitespace, `,`, `}` or `]`: `url: http://host:8080/path`. A value with
  whitespace in it is quoted.
- `"…"` takes the escapes `\"`, ``\` ``, `\\`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX` and `\$`.
- `'…'` is raw, as Perl's single quotes: only `\'` and `\\` are escapes, so a regular expression is written as it is.
- `null`, unquoted, is the one reserved word. `true`, `false` and every other word are plain strings.

**Blocks** are `{ … }`, and `key { … }` is `key: { … }`. **Sequences** are `[ … ]`, separated as fields are.

**Comments** are `//` to the end of the line and `/* … */`, which nest. `#` is not a comment: it starts a directive.

**Tags** are names a value carries for the program that reads it, written before the value: `timeout: @duration 5s`.

```scala mdoc:passthrough
example("""
http { host: 0.0.0.0, port: 8080 }
`odd key`: "a value, with a comma"
pattern: '^\d+$'
timeout: @duration 5s
password: null
""")
```

### Text over several lines

`"""…"""` is a block of text starting on the line after the opening quotes. The indent common to its lines is
stripped, as in Java's text blocks, and the newlines just inside the quotes are dropped; `'''…'''` does the same, raw.
Markers after the opening quotes pick another layout:

| Marker | Layout |
|---|---|
| `<` | all of the indent is stripped |
| `>` | the indent is kept |
| `width(n: 2)` | the indent of the opening line and two more are stripped |
| `+` | lines are folded into one, as YAML's `>` does |

```scala mdoc:passthrough
example("""
query: QQQ
  SELECT id, total
  FROM orders
  WHERE status = 'paid'
  QQQ
banner: QQQ+
  folded into
  one line
  QQQ
""")
```

## Merging

A key written twice is **merged**: maps merge recursively, and anything else — a value, a sequence, `null` — is
replaced by the later one. The same rule merges the sources a program loads, in order, and what `#include` brings into
the block it stands in. `:=` replaces instead of merging, which is the one way to drop keys an earlier definition gave.

```scala mdoc:passthrough
example("""
pool { min: 1, max: 10, idle: 30s }
pool { max: 4 }
cache { size: 100, ttl: 1m }
cache := { size: 10 }
""")
```

A [substitution](#substitutions) standing alone as a value is not merged. What it refers to is known only once every
source is merged, so whether it is a block, which would merge, or anything else, which would be replaced, is not known
yet: a `:` meeting an earlier value of its key is an error when one side is a whole `${…}` and the other a block or
another substitution. `:=` replaces instead, and a value that is no block, such as `a: 5`, replaces whatever the
substitution turns out to be. So a block copied with `${…}` cannot have one of its fields changed:

```scala mdoc:passthrough
example("""
base { port: 1 }
a: ${base}
a { port: 2 }
""")
```

## Substitutions

`${path}` is replaced by the node at `path`, written as keys joined by dots, in backticks when they are not bare, with
indexes in brackets: `${servers[0].host}`. Substitutions are resolved once every source is merged, so a value refers
to its final state, whichever source set it.

- `${path}` alone, unquoted, puts the node there whatever it is: a block, a sequence, a value.
- Inside a value, `jdbc:${host}/db` or `"jdbc:${host}/db"`, it builds a string, and the node has to be a value.
- `${?path}` is optional: with nothing at `path`, the field is left out, an element of a sequence is dropped, and a
  part of a string is empty.
- Inside `"…"` and `"""…"""`, `\${` is a literal `${`. A raw string, `'…'` or `'''…'''`, has no substitutions at
  all, `${` being text. An unquoted value has no escapes, so a value holding a literal `${` is quoted.

```scala mdoc:passthrough
example("""
db {
  host: localhost
  url: "jdbc:postgresql://${db.host}:5432/orders"
}
replica: ${db}
user: ${?env.user}
escaped: "\${not.a.path}"
raw: '${not.a.path}'
""")
```

**System properties override the sources**: before substitutions are resolved, a value the sources define at
`db.host` is replaced by the system property `db.host`, if the application was started with one. Only values the
sources define are overridden, never a block or a sequence. With `-Ddb.host=db.prod`:

```scala mdoc:passthrough
example(
  """
db {
  host: localhost
  url: "${db.host}:5432"
}
""",
  Psi.builder.withSystemProperties(Map("db.host" -> "db.prod")).build.toOption.get)
```

## Including other sources

`#include` reads another Psi source where it stands. Among fields it merges the source's fields into the block, so
the fields after it override them; as a value it makes the source a block.

| Argument | Reads |
|---|---|
| `file: "conf.d/db"` | a file, relative to the source that includes it |
| `resource: "defaults"` | a resource on the program's class path |
| `url: "https://…"` | a URL |
| `optional: true` | nothing when the target is not there, rather than an error |

A `file` or `resource` with no extension is looked for as `.Ψ`, `.ψ` and `.psi`, in that order.

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

## Errors

A load reports every error it can find without guessing, each with where it is, what was found and, often, what to do
about it:

```scala mdoc:passthrough
example("""
server {
  host = localhost
  port: 8080 timeout: 5s
}
name: my service
url: "jdbc:${db.host}"
""")
```

Errors in the syntax come first. Substitutions are resolved only when there are none, so that a field the syntax lost
is not reported as missing: `${db.host}` above is reported once the rest is fixed.

## Limits

A tree loaded is at most 256 levels deep, counting each `{`, `[` and key after a dot, and resolving a substitution
goes at most 256 steps, each a level descended or a substitution followed. Past either, the load reports an error.
A program can set another limit.
