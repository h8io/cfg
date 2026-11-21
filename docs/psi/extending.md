# Psi: extending

A loader [built](api.md#building-a-loader) with `withDirective`, `withMarker` and `withFormatter` reads directives,
markers and formatters of a program's own. Each is registered under the name the files call it by, which is the
loader's to give, not the handler's: a handler's messages do not name it, the report putting the name it was called by
in front.

## Directives

How a directive is written, and what it may give where it stands, is [Directives](directives.md).

A handler declares its parameters and gets the arguments as a map keyed by them, every value a string. It also gets a
context: where the call stands, the name and URL of the source holding it, a way to parse another Psi source, a search
for the target its arguments name as `#include`'s do, each candidate opened by a function of the handler's (`find`,
with `Target.parameters`), and a way to read a URL's bytes under the load's limits on reading (`read`). The nodes of a
fragment it builds count against the load's node limit, every use of it again. It gives back an error of its own or a `Fragment`: a Psi source the
context parsed, any `cfg` node, `Fragment(node)`, or nothing, `Fragment.None`, which adds no field among fields and
leaves the field or element out as a value.

This one reads a text file as one value, its target written as `#include`'s is; `cfg-psi-yaml` and `cfg-psi-hocon`
are built the same way ([Directives](directives.md#importing-yaml-and-hocon)):

```scala mdoc:silent:reset-object
import h8io.cfg.{Id, Node}
import h8io.cfg.impl.psi.*

import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.ByteBuffer

final case class NotText(source: String) extends DirectiveError {
  def message: String = source + " is not UTF-8 text"
}

object Text extends Directive {
  def parameters: Seq[Parameter] = Target.parameters

  def apply(arguments: Node.IMap[Id.Root], context: DirectiveContext): Either[DirectiveError, Fragment] =
    // each candidate opened in turn, a URL from elsewhere not there when reading it fails as not found
    context.find(arguments, Seq(".txt"))(url => context.read(url).map(_.map(url -> _))).flatMap {
      case Some(Some((url, bytes))) =>
        val decoder = StandardCharsets.UTF_8.newDecoder.onMalformedInput(CodingErrorAction.REPORT)
        try Right(Fragment(Node.Scalar(Id.Root, None, decoder.decode(ByteBuffer.wrap(bytes)).toString, context.location)))
        catch { case _: CharacterCodingException => Left(NotText(url.toString)) }
      // an optional target not there, or the load's limit on reading gone, which the load reports
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
  def parameters: Seq[Parameter] = Nil

  def apply(arguments: Node.IMap[Id.Root], value: String): Either[MarkerError, String] =
    try Right(new String(Base64.getMimeDecoder.decode(value), StandardCharsets.UTF_8))
    catch { case e: IllegalArgumentException => Left(new NotBase64(e)) }
}
```

## Formatters

A formatter turns what a substitution finds into a string, `${path; name(arguments)}`
([Substitutions](substitutions.md#formatters)). It is called in two steps: with its arguments when the source is
read, so that what it refuses is reported with the errors of the syntax, and then with the node, resolved, at the
position it is defined at. Each step gives an error of its own or what comes next: a function, then the string.

```scala mdoc:silent
final case class InvalidWidth(width: String) extends FormatterError {
  def message: String = "the width " + width + " is not a positive integer"
}

case object NotAValue extends FormatterError {
  def message: String = "only a value is padded"
}

object Pad extends Formatter {
  def parameters: Seq[Parameter] = Seq(Parameter("width", required = true), Parameter("with", required = false))

  def apply(arguments: Node.IMap[Id.Root]): Either[FormatterError, Node.Some => Either[FormatterError, String]] =
    (arguments("width"), arguments("with")) match {
      case (Node.IScalar(_, _, width, _), padding) =>
        val pad = padding match {
          case Node.IScalar(_, _, value, _) => value
          case _ => " "
        }
        width.toIntOption.filter(_ > 0).toRight(InvalidWidth(width)).map { n =>
          {
            case Node.IScalar(_, _, value, _) => Right(pad * (n - value.length) + value)
            case _ => Left(NotAValue)
          }
        }
      // a required argument is always given, and as a scalar
      case _ => Left(InvalidWidth(""))
    }
}
```

## Together

```scala mdoc:silent
val loader =
  Psi.builder
    .withDirective("text", Text)
    .withMarker("base64", Base64Marker)
    .withFormatter("pad", Pad)
    .build
    .fold(e => sys.error(e.toString), identity)
```

```scala mdoc:invisible
// Writes the two files the example below loads, as `app`
val app = {
  val dir = java.nio.file.Files.createTempDirectory("psi")
  def write(name: String, text: String) = java.nio.file.Files.write(dir.resolve(name), text.getBytes("UTF-8"))
  write("motd.txt", "Hello")
  write(
    "app.psi",
    "motd: #text(file: motd)\nport: 5432\npadded: ${port; pad(8, with: 0)}\n" +
      "greeting: \"\"\"base64\n  aGVsbG8sIHdvcmxk\n  \"\"\"\n")
  dir.resolve("app.psi").toUri.toURL
}
```

With `motd.txt` holding `Hello`, and `app.psi` next to it:

```
motd: #text(file: motd)
port: 5432
padded: ${port; pad(8, with: 0)}
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

So is a formatter, at its name, whichever step it fails at. Arguments it refuses are reported with the errors of the
syntax, before any substitution is resolved, so the second error waits for the first to be fixed:

```scala mdoc
println(loader.parse("app.psi", "a: ${b; pad(-1)}\nb: x\nc: ${d; pad(3)}\nd: [x]").fold(_.message, _.toString))
println(loader.parse("app.psi", "b: x\nc: ${d; pad(3)}\nd: [x]").fold(_.message, _.toString))
```

A builder reports every error of its configuration at once:

```scala mdoc
Psi.builder
  .withDirective("text", Text)
  .withDirective("text", Text)
  .withMarker("base64", Base64Marker)
  .withFormatter("join", Pad)
  .build
```
