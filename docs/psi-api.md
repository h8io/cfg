# Psi: API

How a program loads [Psi](psi.md), reads what it loaded or what went wrong, and extends the format with directives and
markers of its own.

```scala
libraryDependencies += "io.h8" %% "cfg-psi" % "@VERSION@"
```

## Loading

`Psi(urls*)` loads the sources at URLs and merges them in order, and `Psi.parse(name, text)` loads a source held as a
string, `name` naming it in locations. Either gives the tree, an `h8io.cfg.Node.IMap`, read [as any other
loader's](README.md), or `PsiErrors`, every error found: nothing is thrown for anything a source holds. A URL, given
or included, that takes more than 10 seconds to connect or 30 to send its next bytes is unreadable, an error; a load
reads at most 10 000 000 bytes, every read counted, for at most 60 seconds.

```scala mdoc:silent
import h8io.cfg.Node
import h8io.cfg.impl.psi.*

val loaded = Psi.parse("app.psi", "server { host: localhost, port: 8080 }")
```

A node's `location` is a `PsiLocation`: a source, line and column, or for a value a substitution put somewhere, both
where it stands and where it came from.

```scala mdoc
for (root <- loaded)
  root("server") match {
    case server: Node.IMap[?] =>
      server("port") match {
        case port: Node.IScalar[?] => println(port.value + " at " + port.location.description)
        case other => println(other)
      }
    case other => println(other)
  }
```

## Errors

`PsiErrors.errors` holds the errors, each a case class of the sealed `PsiError` with a `location`, a `message` saying
what is wrong and naming what was found, and a `hint` saying what to do, when there is one. A program reads an error
by its type and fields; the texts are for people and may change in any release. `PsiErrors.message`, and its
`toString`, is the report, the errors grouped by source:

```scala mdoc
Psi.parse("app.psi", "a = 1").left.map(_.errors)
Psi.parse("app.psi", "a = 1").left.foreach(errors => println(errors.message))
```

## Building a loader

`Psi` is the loader with nothing added. A builder makes another:

| Method | What it does |
|---|---|
| `withSystemProperties(map)` | gives `#prop` the properties of `map` rather than `sys.props` |
| `withEnvironment(map)` | gives `#env` the variables of `map` rather than `sys.env` |
| `disableDirective(name)` | turns a directive off for good, as a loader for input it does not trust turns off `include` and `env` |
| `withDepthLimit(n)` | limits depth to `n` rather than 256: lower for a thread with a smaller stack than the JVM's default |
| `withReadLimit(bytes, time)` | limits reading the sources of a load, every read counted, rather than to 10 000 000 bytes and 60 seconds |
| `withSizeLimit(nodes, characters)` | limits what resolving substitutions adds, the nodes grafts copy and the characters concatenations build, rather than to 1 000 000 and 10 000 000 |
| `withDirectives(directives*)` | adds directives of its own (below) |
| `withMarkers(markers*)` | adds markers of its own (below) |

`build` gives the `PsiLoader`, with `apply(urls*)` and `parse(name, text)` as `Psi` has, or every error of its
configuration at once: a name registered twice, the built-in directives `include`, `prop` and `env` and the built-in
markers `<`, `>`, `width` and `+` included,
a disabled name registered, a parameter declared twice, a marker named as no marker can be written, a depth limit
below 1, a size or read limit below 0.

```scala mdoc:silent
val untrusted = Psi.builder.disableDirective("include").disableDirective("env").withSystemProperties(Map.empty).build
```

```scala mdoc
untrusted.map(_.parse("input", "#include(file: \"/etc/passwd\")").left.map(_.message))
```

## Directives

A directive is `#name(arguments)`, or `#name` with none, in one of two places: as a value, where it gives a node, or
among fields, where it gives a map merged into the block it stands in. Among elements it is an injection where a
substitution is one: alone on its line among `- ` elements the sequence it gives puts its elements there, and in `[…]` a
sequence does and anything else stands as one element. Arguments are literals — strings, integers, bare names —
positional ones first, then named ones, `name: value`.

A handler declares its parameters and gets the arguments as a map keyed by them, every value a string. It also gets a
context: where the call stands, the name and URL of the source holding it, and a way to parse another Psi source. It
gives back an error of its own or a `Fragment`: a Psi source the context parsed, any `cfg` node, `Fragment(node)`, or
nothing, `Fragment.None`, which adds no field among fields and leaves the field or element out as a value.

This one reads a YAML file with `cfg-yaml`, relative to the source the call stands in:

```scala mdoc:silent:reset-object
import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.*
import h8io.cfg.impl.yaml.YAML

import java.net.URI
import java.nio.file.Paths

final case class YamlUnreadable(file: String, reason: String) extends DirectiveError {
  def message: String = file + " cannot be read as YAML: " + reason
}

object Yaml extends Directive {
  def name: String = "yaml"

  def parameters: Seq[Parameter] = Seq(Parameter("file", required = true))

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
    arguments("file") match {
      case Node.IScalar(_, _, file, _) =>
        // the path is encoded before it is resolved, and one no URL can be made of fails the call too
        try {
          val url = context.url.fold(Paths.get(file).toUri)(_.toURI.resolve(new URI(null, null, file, null))).toURL
          Right(Fragment(YAML(url)))
        } catch { case e: Exception => Left(YamlUnreadable(file, e.toString)) }
      // a required argument is always given, and as a scalar
      case _ => Right(Fragment.None)
    }
}
```

Nothing a source holds should make a handler throw: what it fails with is reported at the call, its `message` and `hint` under the directive's name, and the call
gives nothing; loading goes on. A node a fragment brings keeps its own location behind the call's.

An error is compared by value: a source included from two places is parsed twice, its calls with it, and two errors
equal at one call are reported once. One holding an exception, which equals only itself, would be reported twice, so
the example keeps the exception's text instead.

## Markers

A marker stands after the opening quotes of a `"""…"""` or `'''…'''` block. A loader's markers run after the
block's layout, escapes and substitutions, each on what the one before it gave: a handler gets the arguments as a
directive does and the string, and gives back a new string or an error. A marker's name is a name, as a key's, or a
run of symbols.

```scala mdoc:silent
import java.nio.charset.StandardCharsets
import java.util.Base64

final class NotBase64(cause: IllegalArgumentException) extends MarkerError {
  def message: String = "not Base64: " + cause.getMessage

  override def hint: Option[String] = Some("write the value in Base64, or leave out the marker")
}

object Base64Marker extends Marker {
  def name: String = "base64"

  def parameters: Seq[Parameter] = Nil

  def apply(arguments: Node.IMap[Id.Root], value: String): Either[MarkerError, String] =
    try Right(new String(Base64.getMimeDecoder.decode(value), StandardCharsets.UTF_8))
    catch { case e: IllegalArgumentException => Left(new NotBase64(e)) }
}
```

## Together

```scala mdoc:silent
val loader = Psi.builder.withDirectives(Yaml).withMarkers(Base64Marker).build.fold(e => sys.error(e.toString), identity)
```

```scala mdoc:invisible
// Writes the two files the example below loads, as `app`
val app = {
  val dir = java.nio.file.Files.createTempDirectory("psi")
  def write(name: String, text: String) = java.nio.file.Files.write(dir.resolve(name), text.getBytes("UTF-8"))
  write("db config.yaml", "host: localhost\nport: 5432\n")
  write("app.psi", "database: #yaml(file: \"db config.yaml\")\ngreeting: \"\"\"base64\n  aGVsbG8sIHdvcmxk\n  \"\"\"\n")
  dir.resolve("app.psi").toUri.toURL
}
```

With `db config.yaml` holding `host: localhost` and `port: 5432`, and `app.psi` next to it:

```
database: #yaml(file: "db config.yaml")
greeting: """base64
  aGVsbG8sIHdvcmxk
  """
```

```scala mdoc
println(loader(app).fold(_.message, _.toString))
```

A marker that fails is reported at its block, with its handler's message and hint:

```scala mdoc
// a Scala string cannot hold three double quotes: QQQ stands for them
println(loader.parse("app.psi", "key: QQQbase64\n  not base64!\n  QQQ".replace("QQQ", "\"\"\"")).fold(_.message, _.toString))
```

A builder reports every error of its configuration at once:

```scala mdoc
Psi.builder.withDirectives(Yaml, Yaml).withMarkers(Base64Marker, Base64Marker).build
```
