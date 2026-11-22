# Psi: errors and limits

```scala mdoc:invisible
import h8io.cfg.pages.Example
```

A load reports every error it can find without guessing, each with where it is, what was found and, often, what to do
about it:

```scala mdoc:passthrough
Example.psi("""
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
Resolving substitutions adds at most 1 000 000 nodes, those grafts copy and those a directive such as `#yaml` brings,
every use of a YAML alias counted, and 10 000 000 characters, those of the scalars concatenations build; a source's
own size is not limited. Reading the sources of a load takes at most 10 000 000 bytes, every read counted, and 60
seconds; a wait already begun runs its course, but a read that ends past the time fails all the same. Past any of
these limits, the load reports an error. A program can [set another limit](api.md#building-a-loader).
