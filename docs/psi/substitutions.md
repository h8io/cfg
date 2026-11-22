# Psi: substitutions

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

`${path}` is replaced by the node at `path`, written as keys joined by dots, in backticks when they are not bare, with
indexes in brackets: `${servers[0].host}`. An index after a `-` counts from the end, `${servers[-1]}` being the last
element. Substitutions are resolved once every source is merged, so a value refers to its final state, whichever
source set it.

- `${path}` alone, unquoted, after a key, after a `- ` or as an element of `[…]` puts the node there whatever it is: a
  block, a sequence, a value. A tag written before it, `@t ${path}`, replaces the node's own, which it keeps
  otherwise; so does a tag before a directive's result. Marked with a `*`, `$path*` or `${path*}`, it is an
  [injection](#injections) instead, and alone among fields it has to be one.
- Inside a value, `jdbc:${host}/db` or `"jdbc:${host}/db"`, it builds a string, and the node has to be a value.
- `${?path}` is optional: with nothing at `path`, the field is left out, an element of a sequence is dropped, and a
  part of a string is empty. Written over an earlier value of its key, it leaves that value: `port: 80` then
  `port: ${?env.port}` is 80 when nothing is at `env.port`.
- An index past either end finds nothing, as a missing key does: `${s[5]}` and `${s[-5]}` on three elements are
  errors, and `${?s[-5]}` is left out. Counting from the end needs the whole sequence, so a reference inside a
  sequence, or one that an element of it needs, cannot count from that sequence's end:
  `a: [x, $a[-1]]` is an error, while `a: [x, $a[0]]` is `[x, x]`.
- Outside quotes the braces may be left out: `$path` is `${path}`, and `$?path` is `${?path}`. The path then takes
  all it can, so `$host/db` is `host` and the text `/db`, but `$host-name` and `$name.txt` are paths whole; braces end
  a path where it would go on, `${name}.txt`.
- Outside quotes a `$` always opens a substitution, and one with no path after it is an error: a value holding a
  literal `$` is quoted.
- Inside `"…"` and `"""…"""` only `${` opens a substitution, a `$` alone being text, and `\${` is a literal `${`. A
  raw string, `'…'` or `'''…'''`, has no substitutions at all, `${` being text.

```scala mdoc:passthrough
Example.psi("""
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

## Injections

An injection puts what a node holds in its place: the fields of a map among fields, the elements of a sequence among
elements. It is marked with a `*` right after the substitution, `$path*`, `$?path*`, `${path*}` or `${?path*}`, so
that what a block or a sequence holds can be read off the text: `[$a, $b]` has two elements whatever `a` and `b` are,
and `[$a*, $b]` the elements of `a` and then `b`.

| Written | What it is |
|---|---|
| `key: $a` | the node itself, a graft |
| `key: $a*` | a node of what `a` holds: a map of its fields, or a sequence of its elements |
| `$a*` alone among fields | the fields of `a`, a map |
| `- $a*`, `[…, $a*, …]` | the elements of `a`, a sequence |
| `- $a`, `[…, $a, …]` | one element |
| `$a` alone among fields | an error |
| `$a` or `$a*` on a line among `- ` elements with no `- ` | an error |

- Among the fields of a block, the node must be a map, and its fields stand there as if written there: what follows
  overrides them, what comes before is overridden, and blocks merge deep.
- Among elements, after a `- ` or in `[…]`, the node must be a sequence, and its elements stand there. Every line of a
  sequence written with `- ` starts with one.
- After a key, the node must be a map or a sequence, and the key takes a new one holding what it holds, without its
  tags: `key: @t $a*` tags the new node. A graft, `key: $a`, takes the node whole, tags included, and anything else.
- Two or more alone in a block below a key are fields' places, each the fields of a map, merged in turn. A sequence of
  several is written with dashes: `- $a*` on each line.
- An injection takes no tag among fields or elements: the fields and elements it puts keep their own.
- `${?path*}` with nothing at `path` puts nothing; after a key, the key is left out.
- A directive injects with the same `*`, what it gives in place of the node: `#include(file: "x")*` among fields,
  `- #d*` among elements, `key: #d*` after a key. Unmarked it is one node, and alone among fields an error.
- The `*` marks a whole value, element or field line: in a string, `x$a*` or `$dir*.log`, it is an error. For the text
  `*` after a substitution, close the braces first: `${dir}*.log`.
- The node is taken as it ends up, every source merged. An injection at the top of a source is an error, unless the
  source is included in a block.

```scala mdoc:passthrough
Example.psi("""
defaults { host: localhost, port: 5432, pool { size: 10, timeout: 30 } }
db {
  $defaults*
  port: 6432
  pool { size: 20 }
}
monitoring { interval: 10s }
audited:
  $db*
  $monitoring*
hosts: [a, b]
all:
  - primary
  - $hosts*
  - backup
flat: [$hosts*, $db.host]
groups: [$hosts, [backup]]
copy: $hosts*
""")
```

## Formatters

`${path; name(arguments)}` turns the node at `path` into a string with a formatter: `${hosts; join(",")}` puts the
hosts one after another, a `,` between them. A loader may be [built](api.md#building-a-loader) with formatters of its
own ([Extending](extending.md#formatters)); `join` is the one every loader has.

- `join(separator)` puts the elements of a sequence one after another with the separator between them, nothing for
  an empty one. Anything but a sequence, and an element that is no value, `null` included, are errors.
- What a formatter gives is a string, with only the tag written before it. After a key or a `- ` it is the value, in
  a string it is part of it, and in `[…]` it is one element. It is never an injection: `${hosts*; join(",")}` is an
  error.
- `${?path; …}` with nothing at `path` calls no formatter: the field or element is left out, and a part of a string is
  empty.
- The braces are needed: after `$path` a `;` is text. A substitution takes one formatter, and no whitespace before the
  `;` or the `}`.
- The arguments are written as a directive's, literals only. In quotes they are read by their own rules, so a `"` or
  a `}` inside one closes nothing: `"hosts: ${hosts; join(", ")}"` is one string.

```scala mdoc:passthrough
Example.psi("""
hosts: [db1, db2, db3]
url: "jdbc:postgresql://${hosts; join(",")}/orders"
list: @csv ${hosts; join(", ")}
banner: QQQ
  Serving ${hosts; join(" and ")}
  QQQ
""")
```
