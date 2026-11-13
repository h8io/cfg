# cfg

`cfg` loads configuration files into one tree, the same whatever the format they were written in: maps, sequences,
scalars and explicit nulls, each node knowing its path in the tree and where in the source it came from.

## Modules

| Artifact | What it holds |
|---|---|
| `cfg` | The tree itself — `Node`, `Id`, `Location` and the error traits. No dependencies. |
| `cfg-yaml` | A YAML loader on `snakeyaml-engine`. Keeps the tags written in the source. |
| `cfg-hocon` | A loader on Typesafe Config for HOCON, JSON, `.properties` and YAML, with HOCON's substitutions. |
| `cfg-psi` | A loader for [Psi](psi.md), `cfg`'s own format: merging, substitutions and includes, errors collected rather than thrown, and no dependencies. |

Every loader depends on `cfg`, so one of them is all a project needs:

```scala
libraryDependencies += "io.h8" %% "cfg-yaml" % "@VERSION@"
```

Every artifact is built for Scala 2.13, 2.12 and 3.

## Loading a file

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
not there: an `INone` carrying the path it was looked for at. Scalars are strings; converting them is up to whoever
reads them.

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

Each node also has a `location` naming the file, line and column it was read from, for error messages that point back
into the source.
