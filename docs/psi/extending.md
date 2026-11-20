# Psi: extending

A loader [built](api.md#building-a-loader) with `withDirectives` and `withMarkers` reads directives and markers of a
program's own.

## Directives

How a directive is written, and what it may give where it stands, is [Directives](directives.md).

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
