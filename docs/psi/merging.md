# Psi: merging

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

A key written twice is **merged**: maps merge recursively, and anything else — a value, a sequence, `null` — is
replaced by the later one. The same rule merges the sources a program loads, in order, and what `#include` brings into
the block it stands in. `:=` replaces instead of merging, which is the one way to drop keys an earlier definition gave.

```scala mdoc:passthrough
Example.psi("""
pool { min: 1, max: 10, idle: 30s }
pool { max: 4 }
cache { size: 100, ttl: 1m }
cache := { size: 10 }
""")
```

Merged maps keep the tag of the later if one is written there, and the earlier's otherwise. A value that replaces
another comes with its own tag only, a map after `:=` too: `timeout: @duration 5s` followed by `timeout: 10s` leaves
`timeout` with no tag, which has to be written again.

```scala mdoc:passthrough
Example.psi("""
routes: @ordered { a: 1 }
routes { b: 2 }
pool: @pooled { max: 10 }
pool: @readonly { max: 4 }
timeout: @duration 5s
timeout: 10s
""")
```

A [substitution](substitutions.md) standing alone as a value merges too, once it is resolved: what it refers to is known
only when every source is merged. A block written over it, or under it, merges with it when it turns out a map, its tag
by the rule above, and replaces it, or is replaced by it, when it does not, as `:` does with any value. So a block copied
with `${…}` can have one of its fields changed:

```scala mdoc:passthrough
Example.psi("""
base { host: localhost, port: 1 }
a: ${base}
a { port: 2 }
""")
```
