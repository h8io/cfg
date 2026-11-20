# Psi: substitutions

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

`${path}` is replaced by the node at `path`, written as keys joined by dots, in backticks when they are not bare, with
indexes in brackets: `${servers[0].host}`. Substitutions are resolved once every source is merged, so a value refers
to its final state, whichever source set it.

- `${path}` alone, unquoted, after a key or a `- ` puts the node there whatever it is: a block, a sequence, a value.
  A tag written before it, `@t ${path}`, replaces the node's own, which it keeps otherwise; so does a tag before a
  directive's result.
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
- A directive does wherever it stands what a substitution standing there does, what it gives in place of the node:
  among fields, the fields of a map; alone on its line among `- ` elements, the elements of a sequence; in `[…]`, a
  sequence's elements or anything else as one element; among injections alone, it decides as the first as written
  that gives anything. Only inside a string does a substitution go where a directive does not.
- The node is taken as it ends up, every source merged. An injection at the top of a source is an error, unless the
  source is included in a block.

```scala mdoc:passthrough
Example.psi("""
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
