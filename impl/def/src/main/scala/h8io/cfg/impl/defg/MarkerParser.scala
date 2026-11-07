package h8io.cfg.impl.defg

import h8io.cfg.impl.defg.Lexer.*

import scala.collection.mutable

/** Reads the markers of a triple-quoted block (`format.md` §5) — what stands between the opening delimiter and the end
  * of its line — into the layout stages they pick.
  *
  * `line` is that text with the forbidden characters already reported and left out, and `offsets` the offset in the
  * source of each of its characters, and of the end of the line after them. A syntax error is reported where it stands,
  * once, and nothing else of the line; any error leaves the block to be read as with no marker.
  */
private[defg] final class MarkerParser(
    line: String,
    offsets: IndexedSeq[Int],
    report: (SourceLocation => DefError, Int) => Unit) {
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

  private def error(error: SourceLocation => DefError, at: Int): Unit = {
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

  /** `(name = literal, …)` from its `(`. */
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

  private def argument(): Option[Argument] = {
    val start = i
    identifier()
    if (i == start) invalid
    else {
      val name = line.substring(start, i)
      skipSpaces()
      if (!at(i, '=')) invalid
      else {
        i += 1
        skipSpaces()
        val valueStart = i
        literal().map(Argument(name, start, _, valueStart))
      }
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
    call.name match {
      case "<" => none(call, StripIndent)
      case ">" => none(call, KeepIndent)
      case "+" => none(call, Fold)
      case "width" =>
        validate(call, Set("n"))
        val n = call.arguments.find(_.name == "n")
        if (n.isEmpty) error(MissingArgument(_, "n"), call.at)
        n.map {
          case Argument(_, _, IntegerLiteral(width), _) if width >= 1 && width <= Int.MaxValue =>
            WidthIndent(width.toInt)
          case argument =>
            error(InvalidArgument(_, argument.name), argument.valueAt)
            WidthIndent(1)
        }
      case name =>
        error(UnknownMarker(_, name), call.at)
        None
    }

  /** `stage`, for a marker that takes no arguments. */
  private def none(call: Call, stage: Stage): Option[Stage] = {
    validate(call, Set.empty)
    Some(stage)
  }

  /** Reports the arguments of `call` not in `known`, and every one given again. */
  private def validate(call: Call, known: Set[String]): Unit = {
    val seen = mutable.Set.empty[String]
    call.arguments.foreach { argument =>
      if (!known(argument.name)) error(UnknownArgument(_, argument.name), argument.at)
      else if (!seen.add(argument.name)) error(DuplicateArgument(_, argument.name), argument.at)
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
  final case class Argument(name: String, at: Int, value: Literal, valueAt: Int)

  sealed trait Literal
  final case class StringLiteral(value: String) extends Literal
  final case class IntegerLiteral(value: BigInt) extends Literal

  /** A bare name, `true` and `false` included: a literal has no type until a handler reads it. */
  final case class NameLiteral(value: String) extends Literal
}
