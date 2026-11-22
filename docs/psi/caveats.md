# Psi: caveats

What is easy to get wrong, each with the page that has the rule.

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

## Whitespace does not separate

Fields on one line, and elements in `[…]`, are separated by `,` ([syntax](syntax.md)):

```scala mdoc:passthrough
Example.psi("""
server { host: localhost port: 8080 }
hosts: [a b]
""")
```

## Nothing is typed

`null`, unquoted, is the one reserved word. `true`, `007` and `1e5` are strings, kept as written, and so is `"null"`
quoted ([syntax](syntax.md)). Reading them is the program's business.

## `#` is not a comment

A comment is `//` or `/* … */`. `#` starts a [directive](directives.md):

```scala mdoc:passthrough
Example.psi("""
port: 8080 # the default
""")
```

## An unquoted `$` is a substitution

Outside quotes `$` always opens a [substitution](substitutions.md), and a path takes all it can: `$name.txt` is the
path `name.txt`, not `name` and the text `.txt`. Braces end a path, `${name}.txt`, and quotes make `$` text:

```scala mdoc:passthrough
Example.psi("""
name: report
file: ${name}.txt
price: "$5"
""")
```

## An optional substitution in a string can vanish

`${?path}` with nothing at `path` is left out: a field disappears, an element is dropped, and inside a string it is
empty, so that `"jdbc:${?db.host}/orders"` silently loses its host. `${?path; join(",")}` is no different: with
nothing at `path` no formatter is called, and `[${?hosts; join(",")}]` is an empty sequence rather than one holding an
empty string.

## A replacing value brings its own tag only

Merged maps keep a tag, but a value that replaces another does not inherit one ([merging](merging.md)):

```scala mdoc:passthrough
Example.psi("""
timeout: @duration 5s
timeout: 10s
""")
```

## A value has one tag

A value carries one tag at most, so tags never add up: `@a @b x` is an error, and maps merged keep the later's tag,
not both. Two sources that tag one map differently leave it with the tag of the source loaded last
([merging](merging.md)):

```scala mdoc:passthrough
Example.psi("""
pool: @pooled { max: 10 }
pool: @readonly { min: 1 }
""")
```

## A key written twice merges

`:` merges a map into the one before it, so keys an earlier definition gave stay. `:=` replaces, and is the one way
to drop them ([merging](merging.md)).

## Substitutions see the end

A substitution is resolved once every source is merged, so it refers to a value as the last source left it, not as it
stood where the substitution is written ([substitutions](substitutions.md)):

```scala mdoc:passthrough
Example.psi("""
host: localhost
url: "http://${host}/"
host: example.com
""")
```

## One substitution alone is a value, two inject

Below a key, a substitution alone is the key's value, whatever it is. Two or more alone are
[injections](substitutions.md#injections): their fields, or their elements, are put there, so each has to be a map
or a sequence. A line added below a substitution that worked can break it:

```scala mdoc:passthrough
Example.psi("""
name: report
a { x: 1 }
one:
  $name
two:
  $name
  $a
""")
```

## A field added below a substitution drops its tag

A substitution alone below a key is the key's value, and brings the tag of what it finds, as `key: $b` does. With a
field below it, it is an [injection](substitutions.md#injections), which brings the fields and no tag. The fields come
out the same either way; the tag stays only when the field is merged in from elsewhere:

```scala mdoc:passthrough
Example.psi("""
b: @t { y: 1 }
value:
  $b
merged:
  $b
merged.x: 1
injected:
  $b
  x: 1
""")
```

To keep the tag, write it again, `injected: @t`, or merge the field in rather than write it below.

## A block of substitutions alone is what the first one finds

Below a key, two or more substitutions alone, with no field and no `- `, make a map when the first finds a map and a
sequence when it finds a sequence ([injections](substitutions.md#injections)). Nothing written in the block says
which, so a change where the first is defined, maybe in another source, turns the block into the other kind, and the
error, if any, lands at the second:

```scala mdoc:passthrough
Example.psi("""
m { x: 1 }
s: [a]
block:
  $s
  $m
""")
```

Brackets say what the block is: `{ $m, $n }` takes maps only, and anything else is an error. `[$m, $n]` is a
sequence whatever they find, but one that is not a sequence stands in it as one element rather than failing.

## A formatter needs braces

`${path; name(…)}` formats only in braces ([formatters](substitutions.md#formatters)). With none, `$hosts` is a
substitution and `;join(-)` text after it, a concatenation, and the error says that a sequence is no part of a
string, with no word of a formatter:

```scala mdoc:passthrough
Example.psi("""
hosts: [a, b]
bare: $hosts;join(-)
""")
```

In quotes, `$hosts` with no braces is text, and the value is taken as written:

```scala mdoc:passthrough
Example.psi("""
hosts: [a, b]
quoted: "$hosts; join(-)"
""")
```

## A formatter gives a string, and no tag

A formatted substitution is one string, never the fields or elements of what it finds: alone among fields, or on its
line among `- ` elements, it is an error rather than an injection. Nor does it keep the node's tag, as `${path}`
does: it has only the one written before it:

```scala mdoc:passthrough
Example.psi("""
hosts: @hosts [a, b]
copy: ${hosts}
joined: ${hosts; join(",")}
tagged: @csv ${hosts; join(",")}
""")
```

## An empty block is written

`key:` at the end of a line needs something deeper below it; an empty block is `{}` or `[]`
([indentation](syntax.md#indentation)).

## `#hocon` reads one file

With no extension, `#hocon(file: app)` reads the first of `app.conf`, `app.json` and `app.properties` it finds, and
only that one ([importing](directives.md#importing-yaml-and-hocon)). Typesafe Config's own `include "app"` merges every
one of them, `.conf` over `.json` over `.properties`. To have that, write one `#hocon` for each, with its extension, in
that order from `.properties` to `.conf`, so that the later wins, and `optional: true` on those that may be missing.
