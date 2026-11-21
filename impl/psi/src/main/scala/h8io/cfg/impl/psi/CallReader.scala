package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Lexer.*
import h8io.cfg.impl.psi.MarkerParser.*

import h8io.cfg.{Id, Node}

import scala.collection.{immutable, mutable}

/** Reads calls written as a marker's are (`format.md` §5, §6), a name and its arguments in parentheses, from `line`:
  * the text to read, and `offsets`, the offset in the source of each of its characters and of the end after them. An
  * error is reported where it stands; [[failed]] says whether any was.
  */
private[psi] abstract class CallReader(
    protected val line: String,
    protected val offsets: IndexedSeq[Int],
    protected val report: (SourceLocation => PsiError, Int) => Unit,
    protected val locate: Int => SourceLocation) {

  /** Where reading stands in `line`. */
  protected var i = 0

  /** Whether an error has been reported. */
  protected var failed = false

  /** The error of a call that cannot be read, where it stops fitting. */
  protected def invalidCall: SourceLocation => PsiError

  protected def error(error: SourceLocation => PsiError, at: Int): Unit = {
    failed = true
    report(error, offsets(at))
  }

  protected def syntaxError(): Unit = error(invalidCall, i)

  protected def invalid[A]: Option[A] = {
    syntaxError()
    None
  }

  @inline protected def has(at: Int): Boolean = at < line.length
  @inline protected def at(at: Int, c: Char): Boolean = has(at) && line.charAt(at) == c

  protected def skipSpaces(): Unit = while (has(i) && isSpace(line.charAt(i))) i += 1

  protected def identifier(): Unit =
    if (has(i) && isNameStart(line.codePointAt(i)))
      while (has(i) && isNamePart(line.codePointAt(i))) i += Character.charCount(line.codePointAt(i))

  /** `(name: literal, …)` from its `(`, separated as a directive's arguments are (§6): a trailing `,` is allowed, and a
    * `,` with no argument before it is an empty one.
    */
  protected def arguments(): Option[Vector[Argument]] = {
    i += 1
    skipSpaces()
    val read = Vector.newBuilder[Argument]
    var result: Option[Vector[Argument]] = None
    var more = true
    while (more)
      if (at(i, ')')) {
        i += 1
        result = Some(read.result())
        more = false
      } else if (at(i, ',')) {
        error(EmptyElement.apply, i)
        more = false
      } else
        argument() match {
          case Some(argument) =>
            read += argument
            skipSpaces()
            if (at(i, ',')) {
              i += 1
              skipSpaces()
            } else if (!at(i, ')')) {
              syntaxError()
              more = false
            }
          case None => more = false
        }
    result
  }

  /** `name: literal`, or a literal alone, positional; `name = literal` is reported and read as the former. A name is
    * bare or in backticks, as every name is (§6); a value in backticks is an error.
    */
  protected def argument(): Option[Argument] = {
    val start = i
    val written =
      if (at(i, '`')) backticked()
      else {
        identifier()
        if (i > start) Some(line.substring(start, i)) else None
      }
    skipSpaces()
    written.flatMap(name => if (at(i, ':') || at(i, '=')) Some(name) else None) match {
      case Some(name) => named(name, start)
      case None if at(start, '`') =>
        if (written.nonEmpty) error(BacktickedValue.apply, start)
        None
      case None =>
        i = start
        literal().map(Argument(None, start, _, start))
    }
  }

  /** The rest of `name: literal` from its `:`, or the `=` written in its place. */
  protected def named(name: String, start: Int): Option[Argument] = {
    if (at(i, '=')) error(EqualsInArgument.apply, i)
    else if (has(i + 1) && !isSpace(line.charAt(i + 1))) error(ColonWithoutSpace.apply, i)
    i += 1
    skipSpaces()
    val valueStart = i
    literal().map(Argument(Some(name), start, _, valueStart))
  }

  /** A name in backticks with the escapes of `"…"`, from its opening backtick; with no closing one, a syntax error
    * there.
    */
  protected def backticked(): Option[String] = {
    val start = i
    i += 1
    val value = new StringBuilder
    while (has(i) && !at(i, '`'))
      if (at(i, '\\')) {
        val (read, end) = readEscape(line, i, has(i + 1))
        read match {
          case Some(escaped) => value ++= escaped
          case None => error(InvalidEscape(_, line.substring(i, end)), i)
        }
        i = end
      } else {
        value += line.charAt(i)
        i += 1
      }
    if (has(i)) {
      i += 1
      Some(value.result())
    } else {
      i = start
      invalid
    }
  }

  /** A string in `"…"` or `'…'`, an integer or a name, as a directive's arguments are (§6). */
  protected def literal(): Option[Literal] =
    if (at(i, '"')) quoted()
    else if (at(i, '\'')) raw()
    else if (at(i, '-') || has(i) && isDigit(line.charAt(i))) integer()
    else {
      val start = i
      identifier()
      if (i > start) Some(NameLiteral(line.substring(start, i))) else invalid
    }

  /** A string with the escapes of `"…"`; `\${` is an error, an argument being literal. */
  protected def quoted(): Option[Literal] = {
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
  protected def raw(): Option[Literal] = {
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
  protected def closing(start: Int, value: StringBuilder): Option[Literal] =
    if (has(i)) {
      i += 1
      Some(StringLiteral(value.result()))
    } else {
      i = start
      invalid
    }

  /** An optional `-` and decimal digits, with no leading zeros. */
  protected def integer(): Option[Literal] = {
    val start = i
    if (at(i, '-')) i += 1
    val digits = i
    while (has(i) && isDigit(line.charAt(i))) i += 1
    if (i == digits) invalid
    else if (i - digits > 1 && line.charAt(digits) == '0') {
      i = digits + 1
      invalid
    } else Some(IntegerLiteral(line.substring(start, i)))
  }

  /** The arguments of a handler's call, as it gets them: by parameter, in the order declared. */
  protected def handed(call: Call, bound: Map[String, Argument]): Node.IMap[Id.Root] = {
    val entries = bound.toVector.map { case (name, argument) =>
      name -> (Tree.Scalar(text(argument.value), None, locate(offsets(argument.valueAt))): Tree.Value)
    }
    MapImpl(Id.Root, Tree.Map(entries.map(_._1), immutable.Map(entries*), None, locate(offsets(call.at))))
  }

  /** A literal as a handler gets it: a string, an integer as written, a bare name (§6). */
  protected def text(literal: Literal): String =
    literal match {
      case StringLiteral(value) => value
      case IntegerLiteral(text) => text
      case NameLiteral(value) => value
    }

  /** The arguments of `call` by the parameter they go to — positional ones in order, then named ones, as in Scala —
    * every one that goes to none, or to one already given, reported.
    */
  protected def bind(call: Call, declared: Vector[Parameter]): Map[String, Argument] = {
    val parameters = declared.map(_.name)
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
    declared.foreach { parameter =>
      if (parameter.required && !bound.contains(parameter.name)) error(MissingArgument(_, parameter.name), call.at)
    }
    immutable.ListMap(parameters.flatMap(name => bound.get(name).map(name -> _))*)
  }
}
