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

## An index from the end needs the whole sequence

`$s[-1]` is the last element of `s` once every source is merged, and a key written twice replaces a sequence, never
appends to it: of two sources each setting `hosts`, it is the last one's last host. A sequence gathered from several
is written with injections, `[$a.hosts*, $b.hosts*]`. Counting from the end needs every element, so an element that
counts from the end of its own sequence is an error ([substitutions](substitutions.md)):

```scala mdoc:passthrough
Example.psi("""
hosts: [db1, db2]
hosts: [db3]
last: $hosts[-1]
""")
```

```scala mdoc:passthrough
Example.psi("""
base: [db1, db2]
site: [db3]
hosts: [$base*, $site*]
last: $hosts[-1]
""")
```

```scala mdoc:passthrough
Example.psi("""
all: [x, $all[-1]*]
""")
```

## A second injection below a key makes a map

Below a key, one injection alone is the key's value, `a:⏎  $s*` meaning `a: $s*`, and takes a map or a sequence. A
second line makes the block a map, a line with no `- ` being a field's place, so each injection then has to find a
map ([injections](substitutions.md#injections)). A line added below one that worked can break it; a sequence of
several is written with dashes, `- $s*` on each line:

```scala mdoc:passthrough
Example.psi("""
s: [a, b]
m { x: 1 }
one:
  $s*
two:
  $s*
  $m*
""")
```

## A field added below a substitution drops its tag

A substitution alone below a key is the key's value, and brings the tag of what it finds, as `key: $b` does. With a
field below it, it has to be an [injection](substitutions.md#injections), `$b*`, which brings the fields and no tag.
The fields come out the same either way; the tag stays only when the field is merged in from elsewhere:

```scala mdoc:passthrough
Example.psi("""
b: @t { y: 1 }
value:
  $b
merged:
  $b
merged.x: 1
injected:
  $b*
  x: 1
""")
```

To keep the tag, write it again, `injected: @t`, or merge the field in rather than write it below.

## A `*` after a substitution marks it

`$prefix*`, a whole value, is an [injection](substitutions.md#injections), not the text of `prefix` followed by `*`,
and in a longer value, `$dir*.log`, it is an error. A glob or any other text with a `*` right after a substitution is
written with braces, which end the path before it:

```scala mdoc:passthrough
Example.psi("""
dir: /var/log
logs: $dir*.log
""")
```

```scala mdoc:passthrough
Example.psi("""
dir: /var/log
logs: ${dir}/*.log
prefix: app
pattern: ${prefix}*
""")
```

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

A formatted substitution is one string, never the fields or elements of what it finds: marked with a `*`,
`${hosts*; join(",")}`, it is an error. Nor does it keep the node's tag, as `${path}` does: it has only the one written
before it:

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

## Two references setting one key

`standard()` and `withReferences` merge every `reference` on the class path, those of one name in the order of the
class path ([loading](api.md#from-the-class-path)). Nothing fixes that order, so two libraries that set one key are
settled by where their jars stand, silently, as in Typesafe Config. The program's own configuration, merged over
them, settles it for good.

## A reference resolves with the application

A reference is resolved with everything else, not on its own as Typesafe Config requires: it sees what the
application overrides, and it may refer to a key only the application sets. Loaded without such an application, it
fails at the reference, in the library's jar.

