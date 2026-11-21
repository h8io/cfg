package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Lexer.*

import h8io.cfg.{Id, Node}

/** Reads the markers of a triple-quoted block (`format.md` §5) — what stands between the opening delimiter and the end
  * of its line — into the layout stages they pick.
  *
  * `line` is that text with the forbidden characters already reported and left out, and `offsets` the offset in the
  * source of each of its characters, and of the end of the line after them. A syntax error is reported where it stands,
  * once, and nothing else of the line; any error leaves the block to be read as with no marker.
  */
private[psi] final class MarkerParser(
    text: CharSequence,
    positions: IndexedSeq[Int],
    reporter: (SourceLocation => PsiError, Int) => Unit,
    locator: Int => SourceLocation,
    markers: Map[String, Marker])
    extends CallReader(text, positions, reporter, locator) {
  import MarkerParser.*

  protected val invalidCall: SourceLocation => PsiError = InvalidMarker.apply

  /** The stages to apply, in order, Java's rule first when no marker sets the indent, and the loader's markers to run
    * over the value after them (`format.md` §5); none of the latter when any marker is wrong.
    */
  def stages(): (Vector[Stage], Vector[MarkerCall]) = {
    val read = chain().map(check)
    read match {
      case Some((stages, calls)) if !failed =>
        (if (stages.exists(_.isInstanceOf[Indent])) stages else CommonIndent +: stages, calls)
      case _ => (Vector(CommonIndent), Vector.empty)
    }
  }

  /** Markers separated by whitespace; whitespace before the first or after the last is an error of its own. */
  private def chain(): Option[Vector[Call]] = {
    if (has(0) && isSpace(line.charAt(0))) {
      report(WhitespaceAfterDelimiter.apply, offsets(0))
      skipSpaces()
    }
    val calls = Vector.newBuilder[Call]
    var broken = false
    while (!broken && has(i))
      call() match {
        case Some(read) =>
          calls += read
          if (has(i))
            if (isSpace(line.charAt(i))) {
              val spaces = i
              skipSpaces()
              if (!has(i)) report(WhitespaceAfterDelimiter.apply, offsets(spaces))
            } else {
              syntaxError()
              broken = true
            }
        case None => broken = true
      }
    if (broken) None else Some(calls.result())
  }

  /** A marker: its name, and its arguments in parentheses if any. */
  private def call(): Option[Call] = {
    val start = i
    name() match {
      case Some(name) if at(i, '(') => arguments().map(Call(name, start, _))
      case Some(name) => Some(Call(name, start, Vector.empty))
      case None => invalid
    }
  }

  /** A run of symbol characters, or a name by the key rule. */
  private def name(): Option[String] = {
    val start = i
    if (has(i) && isSymbol(line.charAt(i))) while (has(i) && isSymbol(line.charAt(i))) i += 1
    else identifier()
    if (i > start) Some(line.subSequence(start, i).toString) else None
  }

  /** The stages the markers pick; every unknown marker, bad argument and second indent is reported. */
  private def check(calls: Vector[Call]): (Vector[Stage], Vector[MarkerCall]) = {
    var indented = false
    val stages = Vector.newBuilder[Stage]
    val loaders = Vector.newBuilder[MarkerCall]
    var loaderSeen = false
    calls.foreach { call =>
      builtIns.get(call.name) match {
        case Some(BuiltIn(parameters, stage)) =>
          if (loaderSeen) error(BuiltInMarkerAfterLoaders(_, call.name), call.at)
          stage(call, bind(call, parameters.map(Parameter(_, required = false)))).foreach { stage =>
            stage match {
              case _: Indent =>
                if (indented) error(ConflictingIndent(_, call.name), call.at)
                indented = true
              case Fold => ()
            }
            stages += stage
            ()
          }
        case None =>
          markers.get(call.name) match {
            case Some(marker) =>
              loaderSeen = true
              loaders +=
                MarkerCall(
                  call.name,
                  marker,
                  handed(call, bind(call, marker.parameters.toVector)),
                  locate(offsets(call.at)))
              ()
            case None => error(UnknownMarker(_, call.name), call.at)
          }
      }
    }
    (stages.result(), loaders.result())
  }

  /** The markers every loader knows until handlers can be registered (plan step 10), each with its parameters. */
  private val builtIns: Map[String, BuiltIn] =
    Map(
      "<" -> BuiltIn(Vector.empty, (_, _) => Some(StripIndent)),
      ">" -> BuiltIn(Vector.empty, (_, _) => Some(KeepIndent)),
      "+" -> BuiltIn(Vector.empty, (_, _) => Some(Fold)),
      "width" -> BuiltIn(Vector("n"), width)
    )

  private def width(call: Call, arguments: Map[String, Argument]): Option[Stage] = {
    val n = arguments.get("n")
    if (n.isEmpty) error(MissingArgument(_, "n"), call.at)
    n.map { argument =>
      val width = argument.value match {
        case IntegerLiteral(text) => Some(BigInt(text)).filter(w => w >= 1 && w <= Int.MaxValue)
        case _ => None
      }
      width.fold {
        error(InvalidArgument(_, "n"), argument.valueAt)
        WidthIndent(1)
      }(w => WidthIndent(w.toInt))
    }
  }

}

private object MarkerParser {

  /** A character of a symbolic marker name: ASCII punctuation but brackets, quotes, `,`, `\`, `_` and `/`, which is
    * kept for a comment after the markers.
    */
  def isSymbol(c: Char): Boolean = "!#$%&*+-.:;<=>?@^|~".indexOf(c.toInt) >= 0

  def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  final case class Call(name: String, at: Int, arguments: Vector[Argument])

  /** An argument, named or positional. */
  final case class Argument(name: Option[String], at: Int, value: Literal, valueAt: Int)

  /** A loader's marker called on a block by `name`, with its arguments, at `location`, run over the block's value (§5).
    */
  final case class MarkerCall(name: String, marker: Marker, arguments: Node.IMap[Id.Root], location: SourceLocation)

  /** A built-in marker: its parameters, in order, and the stage it picks from its arguments by parameter. */
  final case class BuiltIn(parameters: Vector[String], stage: (Call, Map[String, Argument]) => Option[Stage])

  sealed trait Literal
  final case class StringLiteral(value: String) extends Literal

  /** An integer as written, `-0` included (§5, §6): a handler gets it as text, as it gets a directive's. */
  final case class IntegerLiteral(text: String) extends Literal

  /** A bare name, `true` and `false` included: a literal has no type until a handler reads it. */
  final case class NameLiteral(value: String) extends Literal
}
