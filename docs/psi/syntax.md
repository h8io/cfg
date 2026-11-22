# Psi: syntax

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

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

**Blocks** are `{ … }`, and `key { … }` is `key: { … }`, or are written by [indentation](#indentation).
**Sequences** are `[ … ]`, separated as fields are.

**Comments** are `//` to the end of the line and `/* … */`, which nest. `#` is not a comment: it starts a directive.

**Tags** are names a value carries for the program that reads it, written before the value: `timeout: @duration 5s`.
A value has one tag at most: `@a @b x` is an error.

```scala mdoc:passthrough
Example.psi("""
http { host: 0.0.0.0, port: 8080 }
`odd key`: "a value, with a comma"
pattern: '^\d+$'
timeout: @duration 5s
password: null
""")
```

## Indentation

A block is written in braces or by indentation, as in Scala 3, and the two mix freely, in one source and in one block.

- `key:` at the end of a line, a tag allowed after it, opens a block on the lines indented deeper below: fields make
  it a map, `- ` elements a sequence, and a value alone is the key's value, as if written after the `:`. A
  substitution or a directive marked with a `*` alone on its line is an [injection](substitutions.md#injections) of a
  map's fields, two or more such lines making a map; among elements every line starts with `- `, `- $a*` injecting a
  sequence's elements. With nothing deeper below, it is an error; an empty block is written `{}` or `[]`.
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
Example.psi("""
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

## Text over several lines

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
Example.psi("""
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
