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
server:
  host: localhost
  port: 8080
pool.max: 16
tags: [web, api]
""")
```

What a source loads as is shown here written back as Psi: the same tree, every value a string, written with
[indentation](#indentation).

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

**Blocks** are `{ … }`, and `key { … }` is `key: { … }`, or are written by [indentation](#indentation). **Sequences** are `[ … ]`, separated as fields are.

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

### Indentation

A block is written in braces or by indentation, as in Scala 3, and the two mix freely, in one source and in one block.

- `key:` at the end of a line, tags allowed after it, opens a block on the lines indented deeper below: fields make it
  a map, `- ` elements a sequence, and a value alone is the key's value, as if written after the `:`. A substitution
  or a directive alone on its line among fields or elements is an [injection](#injections), and so are two or more
  alone. With nothing deeper below, it is an error; an empty block is written `{}` or `[]`.
- An element is `- value` at the start of a line, deeper than its key. A map element has its first field after the
  `- ` and the rest aligned under it; `- - x` is a sequence in a sequence; a tag goes after the `- `, with the block
  below it.
- The lines of a block start at its width, which any deeper indent sets. A deeper line opens nothing by itself — there
  are no continuation lines — and a line further left closes blocks until it lands on the width of one still open. The
  top level starts at column 1.
- Inside `{…}` the first line that starts there sets its width, and a `}` stands in any column. Inside `[…]` and a
  directive's `(…)` indentation means nothing.
- A blank line, a comment-only line and a line holding only a doc comment take no part; a line that starts with a doc
  comment and goes on with a field starts where the comment does. A tab in indentation is an error.

```scala mdoc:passthrough
example("""
server:
  host: example.com
  ports:
    - 80
    - 443
  tls { enabled: true, protocols: [TLSv1.3] }
routes: @ordered
  - path: /api
    to: backend
  - path: /
    to: static
greeting:
  QQQ
  Hello,
  world
  QQQ
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

- `${path}` alone, unquoted, after a key or a `- ` puts the node there whatever it is: a block, a sequence, a value.
  Where nothing gives it a place of its own, among fields, on a line of its own among `- ` elements, or in `[…]`, it
  is an [injection](#injections).
- Inside a value, `jdbc:${host}/db` or `"jdbc:${host}/db"`, it builds a string, and the node has to be a value.
- `${?path}` is optional: with nothing at `path`, the field is left out, an element of a sequence is dropped, and a
  part of a string is empty.
- Outside quotes the braces may be left out: `$path` is `${path}`, and `$?path` is `${?path}`. The path then takes
  all it can, so `$host/db` is `host` and the text `/db`, but `$host-name` and `$name.txt` are paths whole; braces end
  a path where it would go on, `${name}.txt`.
- Outside quotes a `$` always opens a substitution, and one with no path after it is an error: a value holding a
  literal `$` is quoted.
- Inside `"…"` and `"""…"""` only `${` opens a substitution, a `$` alone being text, and `\${` is a literal `${`. A
  raw string, `'…'` or `'''…'''`, has no substitutions at all, `${` being text.

```scala mdoc:passthrough
example("""
db {
  host: localhost
  url: "jdbc:postgresql://${db.host}:5432/orders"
}
replica: ${db}
backup: $db.host
user: $?env.user
escaped: "\${not.a.path}"
raw: '${not.a.path}'
""")
```

### Injections

An injection puts what a node holds in its place: the fields of a map among fields, the elements of a sequence among
elements. A substitution injects where nothing before it, no key and no `- `, gives it a place for one node:

- Alone among the fields of a block, the node must be a map, and its fields stand there as if written there: what
  follows overrides them, what comes before is overridden, and blocks merge deep.
- Alone on its line among `- ` elements, the node must be a sequence, and its elements stand there. After a `- ` it is
  one element, a sequence nested whole.
- In `[…]`, a sequence puts its elements there, and anything else stands as one element: `[$hosts, $db.host]` holds
  the hosts and then the host. Brackets around it nest a sequence whole: `[[$hosts]]`.
- Two or more alone in a block below a key, none tagged, take what the first that finds anything finds: the fields of
  maps, merged in turn, or the elements of sequences. One alone there is the key's value, the node itself.
- An injection takes no tags: the fields and elements it puts keep their own. In `[…]` a tag goes to the one element
  a node that is no sequence makes.
- A directive injects where a substitution does, what it gives in place of the node: among fields, the fields of a
  map; alone on its line among `- ` elements, the elements of a sequence; in `[…]`, a sequence's elements or anything
  else as one element.
- The node is taken as it ends up, every source merged. An injection at the top of a source is an error, unless the
  source is included in a block.

```scala mdoc:passthrough
example("""
defaults { host: localhost, port: 5432, pool { size: 10, timeout: 30 } }
db {
  $defaults
  port: 6432
  pool { size: 20 }
}
monitoring { interval: 10s }
audited:
  $db
  $monitoring
hosts: [a, b]
all:
  - primary
  $hosts
  - backup
flat: [$hosts, $db.host]
groups: [[$hosts], [backup]]
""")
```

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
example(
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
goes at most 256 steps, each a level descended or a substitution followed, a substitution used again counting as
deep as it went the first time. So a chain of substitutions goes 256 links, whatever order its keys are written in.
Resolving substitutions adds at most 1 000 000 nodes, those grafts copy, and 10 000 000 characters, those of the
scalars concatenations build; a source's own size is not limited. Reading the sources of a load takes at most
10 000 000 bytes, every read counted, and 60 seconds; a wait already begun runs its course, but a read that ends past
the time fails all the same. Past any of these limits, the load reports an error.
A program can set another limit.
