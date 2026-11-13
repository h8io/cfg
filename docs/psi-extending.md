# Psi: extending

A [Psi](psi.md) loader is built with the directives and markers it knows. `#include` and the layout markers `<`, `>`,
`width` and `+` come with every loader; a builder adds others:

```scala
Psi.builder
  .withDirectives(…)
  .withMarkers(…)
  .disableDirective("include")
  .withSystemProperties(Map.empty)
  .withDepthLimit(256)
  .build
```

`build` gives the loader, with `apply(urls*)` and `parse(name, text)` as `Psi` has, or every error of its
configuration at once: a name registered twice, `include` and the built-in markers included, a disabled name
registered, a parameter declared twice, a marker named as no marker can be written.

## Directives

A directive is `#name(arguments)`, or `#name` with none, in one of two places: as a value, where it gives a node, or
among fields, where it gives a map merged into the block it stands in. Arguments are literals — strings, integers,
bare names — positional ones first, then named ones, `name: value`.

A handler declares its parameters and gets the arguments as a map keyed by them, every value a string. It also gets a
context: where the call stands, the name and URL of the source holding it, and a way to parse another Psi source. It
gives back an error of its own, nothing, or a `Fragment`: a Psi source the context parsed, or any `cfg` node,
`Fragment(node)`.

This one reads a YAML file with `cfg-yaml`, relative to the source the call stands in:

```scala mdoc:silent
import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.*
import h8io.cfg.impl.yaml.YAML

import java.nio.file.Paths

final class YamlUnreadable(file: String, cause: Exception) extends DirectiveError {
  def message: String = file + " cannot be read as YAML: " + cause.getMessage
}

object Yaml extends Directive {
  def name: String = "yaml"

  def parameters: Seq[Parameter] = Seq(Parameter("file", required = true))

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Option[Fragment]] =
    arguments("file") match {
      case Node.IScalar(_, _, file, _) =>
        val url = context.url.fold(Paths.get(file).toUri.toURL)(_.toURI.resolve(file).toURL)
        try Right(Some(Fragment(YAML(url))))
        catch { case e: Exception => Left(new YamlUnreadable(file, e)) }
      // a required argument is always given, and as a scalar
      case _ => Right(None)
    }
}
```

What the handler fails with is reported at the call, its `message` and `hint` under the directive's name, and the call
gives nothing; loading goes on. A node a fragment brings keeps its own location behind the call's.

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
  write("db.yaml", "host: localhost\nport: 5432\n")
  write("app.psi", "database: #yaml(file: \"db.yaml\")\ngreeting: \"\"\"base64\n  aGVsbG8sIHdvcmxk\n  \"\"\"\n")
  dir.resolve("app.psi").toUri.toURL
}
```

With `db.yaml` holding `host: localhost` and `port: 5432`, and `app.psi` next to it:

```
database: #yaml(file: "db.yaml")
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
