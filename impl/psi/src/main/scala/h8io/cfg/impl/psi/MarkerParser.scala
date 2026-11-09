package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Lexer.*

import scala.collection.mutable

/** Reads the markers of a triple-quoted block (`format.md` §5) — what stands between the opening delimiter and the end
  * of its line — into the layout stages they pick.
  *
  * `line` is that text with the forbidden characters already reported and left out, and `offsets` the offset in the
  * source of each of its characters, and of the end of the line after them. A syntax error is reported where it stands,
  * once, and nothing else of the line; any error leaves the block to be read as with no marker.
  */
private[psi] final class MarkerParser(
    line: String,
    offsets: IndexedSeq[Int],
    report: (SourceLocation => PsiError, Int) => Unit) {
  import MarkerParser.*

  private var i = 0
  private var failed = false

  /** The stages to apply, in order, Java's rule first when no marker sets the indent. */
  def stages(): Vector[Stage] = {
    val read = chain().map(check)
    read match {
      case Some(stages) if !failed =>
        if (stages.exists(_.isInstanceOf[Indent])) stages else CommonIndent +: stages
      case _ => Vector(CommonIndent)
    }
  }

  private def error(error: SourceLocation => PsiError, at: Int): Unit = {
    failed = true
    report(error, offsets(at))
  }

  private def syntaxError(): Unit = error(InvalidMarker.apply, i)

  private def invalid[A]: Option[A] = {
    syntaxError()
    None
  }

  @inline private def has(at: Int): Boolean = at < line.length
  @inline private def at(at: Int, c: Char): Boolean = has(at) && line.charAt(at) == c

  private def skipSpaces(): Unit = while (has(i) && isSpace(line.charAt(i))) i += 1

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
    if (i > start) Some(line.substring(start, i)) else None
  }

  private def identifier(): Unit =
    if (has(i) && isNameStart(line.codePointAt(i)))
      while (has(i) && isNamePart(line.codePointAt(i))) i += Character.charCount(line.codePointAt(i))

  /** `(name: literal, …)` from its `(`. */
  private def arguments(): Option[Vector[Argument]] = {
    i += 1
    skipSpaces()
    if (at(i, ')')) {
      i += 1
      Some(Vector.empty)
    } else {
      val read = Vector.newBuilder[Argument]
      var result: Option[Vector[Argument]] = None
      var more = true
      while (more)
        argument() match {
          case Some(argument) =>
            read += argument
            skipSpaces()
            if (at(i, ',')) {
              i += 1
              skipSpaces()
            } else {
              more = false
              if (at(i, ')')) {
                i += 1
                result = Some(read.result())
              } else syntaxError()
            }
          case None => more = false
        }
      result
    }
  }

  /** `name: literal`, or a literal alone, positional; `name = literal` is reported and read as the former. */
  private def argument(): Option[Argument] = {
    val start = i
    identifier()
    val name = line.substring(start, i)
    skipSpaces()
    if (i > start && (at(i, ':') || at(i, '='))) {
      if (at(i, '=')) error(EqualsInArgument.apply, i)
      else if (has(i + 1) && !isSpace(line.charAt(i + 1))) error(ColonWithoutSpace.apply, i)
      i += 1
      skipSpaces()
      val valueStart = i
      literal().map(Argument(Some(name), start, _, valueStart))
    } else {
      i = start
      literal().map(Argument(None, start, _, start))
    }
  }

  /** A string in `"…"` or `'…'`, an integer or a name, as a directive's arguments are (§6). */
  private def literal(): Option[Literal] =
    if (at(i, '"')) quoted()
    else if (at(i, '\'')) raw()
    else if (at(i, '-') || has(i) && isDigit(line.charAt(i))) integer()
    else {
      val start = i
      identifier()
      if (i > start) Some(NameLiteral(line.substring(start, i))) else invalid
    }

  /** A string with the escapes of `"…"`; `\${` is an error, an argument being literal. */
  private def quoted(): Option[Literal] = {
    val start = i
    i += 1
    val value = new StringBuilder
    while (has(i) && !at(i, '"'))
      if (at(i, '\\')) {
        val (read, end) = readEscape(line, i, has(i + 1))
        read match {
          case Some(escaped) => value ++= escaped
          case None => error(InvalidEscape(_, line.substring(i, end)), i)
        }
        i = end
      } else {
        if (line.startsWith("${", i)) error(SubstitutionInArgument.apply, i)
        value += line.charAt(i)
        i += 1
      }
    closing(start, value)
  }

  /** A string with the escapes of `'…'`: `\'` and `\\`. */
  private def raw(): Option[Literal] = {
    val start = i
    i += 1
    val value = new StringBuilder
    while (has(i) && !at(i, '\'')) {
      if (at(i, '\\') && (at(i + 1, '\'') || at(i + 1, '\\'))) i += 1
      value += line.charAt(i)
      i += 1
    }
    closing(start, value)
  }

  /** The string read, at its closing quote; with none, a syntax error at its opening one. */
  private def closing(start: Int, value: StringBuilder): Option[Literal] =
    if (has(i)) {
      i += 1
      Some(StringLiteral(value.result()))
    } else {
      i = start
      invalid
    }

  /** An optional `-` and decimal digits, with no leading zeros. */
  private def integer(): Option[Literal] = {
    val start = i
    if (at(i, '-')) i += 1
    val digits = i
    while (has(i) && isDigit(line.charAt(i))) i += 1
    if (i == digits) invalid
    else if (i - digits > 1 && line.charAt(digits) == '0') {
      i = digits + 1
      invalid
    } else Some(IntegerLiteral(BigInt(line.substring(start, i))))
  }

  /** The stages the markers pick; every unknown marker, bad argument and second indent is reported. */
  private def check(calls: Vector[Call]): Vector[Stage] = {
    var indented = false
    calls.flatMap { call =>
      val stage = builtIn(call)
      stage.foreach {
        case _: Indent =>
          if (indented) error(ConflictingIndent.apply, call.at)
          indented = true
        case Fold => ()
      }
      stage
    }
  }

  private def builtIn(call: Call): Option[Stage] =
    builtIns.get(call.name) match {
      case Some(BuiltIn(parameters, stage)) => stage(call, bind(call, parameters))
      case None =>
        error(UnknownMarker(_, call.name), call.at)
        None
    }

  /** The markers every loader knows until handlers can be registered (plan step 10), each with its parameters. */
  private def builtIns: Map[String, BuiltIn] =
    Map(
      "<" -> BuiltIn(Vector.empty, (_, _) => Some(StripIndent)),
      ">" -> BuiltIn(Vector.empty, (_, _) => Some(KeepIndent)),
      "+" -> BuiltIn(Vector.empty, (_, _) => Some(Fold)),
      "width" -> BuiltIn(Vector("n"), width)
    )

  private def width(call: Call, arguments: Map[String, Argument]): Option[Stage] = {
    val n = arguments.get("n")
    if (n.isEmpty) error(MissingArgument(_, "n"), call.at)
    n.map {
      case Argument(_, _, IntegerLiteral(width), _) if width >= 1 && width <= Int.MaxValue => WidthIndent(width.toInt)
      case argument =>
        error(InvalidArgument(_, "n"), argument.valueAt)
        WidthIndent(1)
    }
  }

  /** The arguments of `call` by the parameter they go to — positional ones in order, then named ones, as in Scala —
    * every one that goes to none, or to one already given, reported.
    */
  private def bind(call: Call, parameters: Vector[String]): Map[String, Argument] = {
    val bound = mutable.LinkedHashMap.empty[String, Argument]
    var named = false
    call.arguments.zipWithIndex.foreach {
      case (argument @ Argument(None, at, _, _), index) =>
        if (named) error(PositionalAfterNamed.apply, at)
        else if (index >= parameters.length) error(ExtraArgument.apply, at)
        else bound(parameters(index)) = argument
      case (argument @ Argument(Some(name), at, _, _), _) =>
        named = true
        if (!parameters.contains(name)) error(UnknownArgument(_, name), at)
        else if (bound.contains(name)) error(DuplicateArgument(_, name), at)
        else bound(name) = argument
    }
    bound.toMap
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

  /** A built-in marker: its parameters, in order, and the stage it picks from its arguments by parameter. */
  final case class BuiltIn(parameters: Vector[String], stage: (Call, Map[String, Argument]) => Option[Stage])

  sealed trait Literal
  final case class StringLiteral(value: String) extends Literal
  final case class IntegerLiteral(value: BigInt) extends Literal

  /** A bare name, `true` and `false` included: a literal has no type until a handler reads it. */
  final case class NameLiteral(value: String) extends Literal
}
