# The tree

Every loader gives the same thing: the root of a tree, a `Node.IMap[Id.Root]`. This page is how a program reads it,
whichever loader built it.

## Nodes

A node is one of five, all in `h8io.cfg.Node`:

| Node | What it is |
|---|---|
| `IMap` | a map, its children looked up by key |
| `ISeq` | a sequence, its children looked up by index |
| `IScalar` | a value, always a string: converting it is up to whoever reads it |
| `INull` | an explicit null written in the source |
| `INone` | a key or an index that is not there |

The first four, `ISome`, are in the source: each has a `location` and a `tag`. `INone` is not, and carries only its
path. `INull` and `INone` are also `NodeError`s, so that a program that wanted a value can hand either on as the
error it is.

Each type is parameterised by the `Id` of its position, `Node.IMap[Id.Root]` for the root and `INode[Id.Key]` for what a
map gives; `Node` is an `INode` of any `Id`, and `Node.Map`, `Node.Scalar` and the others are the types of any `Id`.

## Paths

An `Id` is a node's path: `Id.Root`, the top of the tree; `Id.Key(key, parent)`, a field of a map; or
`Id.Index(index, parent)`, an element of a sequence. Its `path` is the path written out, `server.hosts[0]`, a key that
is not a letter or `_` followed by letters, digits, `_` and `-` in backticks, as Psi writes it:
``spark.`executor.memory` ``.

A container builds a child's `Id` from its own, so a node it gives, missing or not, always sits directly below it.

## Reading a file

Take `app.yaml`:

```yaml
server:
  host: localhost
  port: 8080
```

```scala mdoc:invisible
// Writes the file above where the examples below can read it as `url`
val url = {
  val file = java.nio.file.Files.createTempFile("app", ".yaml")
  java.nio.file.Files.write(file, "server:\n  host: localhost\n  port: 8080\n".getBytes("UTF-8"))
  file.toUri.toURL
}
```

A loader takes any number of URLs and overlays them in order, the last one winning; `url` points at the file:

```scala mdoc:silent
import h8io.cfg.Node
import h8io.cfg.impl.yaml.YAML

val cfg = YAML(url)
```

A map is looked up by key and a sequence by index. What comes back is a node like any other, and so is a key that is
not there:

```scala mdoc:silent
def show(node: Node): String =
  node match {
    case scalar: Node.IScalar[?] => s"${scalar.id.path} = ${scalar.value}"
    case nul: Node.INull[?] => s"${nul.id.path} is null"
    case none: Node.INone[?] => s"${none.id.path} is missing"
    case map: Node.IMap[?] => s"${map.id.path} is a map of ${map.size.toString}"
    case seq: Node.ISeq[?] => s"${seq.id.path} is a sequence of ${seq.size.toString}"
  }

val server = cfg("server") match {
  case map: Node.IMap[?] => map
  case other => sys.error(s"${other.id.path} is not a map")
}
```

```scala mdoc
show(server)
show(server("port"))
show(server("secure"))
```

A map and a sequence are also `Iterable`s of their children, those that are there, and `map - key` is a map without
the key.

## Tags

A tag is a name the author gave a value for the program that reads it: `@duration` in Psi, `!duration` in YAML. `tag`
holds it by name, without the syntax that marks it, so both give `Some("duration")`. Nothing is inferred: a tag is
there only when one was written, and a value has one at most. HOCON has none, so its loader always gives `None`.

A tag converts nothing either. What `@duration` means is up to the program.

## Locations

Every `ISome` has a `location`, whose `description` names the source, line and column it was read from, for error
messages that point back into the source:

```scala mdoc
server("port") match {
  case port: Node.IScalar[?] => port.location.description.replaceAll(".*/", "")
  case other => show(other)
}
```

How a location reads is the loader's: YAML's is the file, line and column, HOCON's whatever Typesafe Config says, and
Psi's tells a value a substitution put somewhere apart from where it came from (see [Psi: loading](psi/api.md)).

## Errors

`CfgError` is the trait of every error about a configuration, raised while loading it or by whatever reads it, and
`NodeError` one about a node, which it carries. A program that reads the tree can make its own errors these, so that
whatever reports them knows the path and the place in the source.
