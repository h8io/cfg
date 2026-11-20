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
empty, so that `"jdbc:${?db.host}/orders"` silently loses its host.

## A replacing value brings its own tag only

Merged maps keep a tag, but a value that replaces another does not inherit one ([merging](merging.md)):

```scala mdoc:passthrough
Example.psi("""
timeout: @duration 5s
timeout: 10s
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

## An empty block is written

`key:` at the end of a line needs something deeper below it; an empty block is `{}` or `[]`
([indentation](syntax.md#indentation)).
