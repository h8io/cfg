package h8io.cfg.impl.psi

import h8io.cfg.Id

/** A token of a source.
  *
  * @param startsLine
  *   whether no other token stands before it on its line; with its column, what the indentation syntax will need
  * @param spaced
  *   whether whitespace or a comment stands right before it, which the dots of a key may not have around them
  */
private[psi] final case class Token(kind: Token.Kind, location: SourceLocation, startsLine: Boolean, spaced: Boolean)

private[psi] object Token {
  sealed trait Kind

  case object LeftBrace extends Kind
  case object RightBrace extends Kind
  case object LeftBracket extends Kind
  case object RightBracket extends Kind
  case object LeftParen extends Kind
  case object RightParen extends Kind
  case object Comma extends Kind
  case object Dot extends Kind
  case object Colon extends Kind

  /** `:=` */
  case object Replace extends Kind
  case object Equals extends Kind
  case object Newline extends Kind

  /** The end of the source, or of what could be read of it after a fatal error. */
  case object End extends Kind

  /** A doc comment, with the text between its delimiters as written. */
  final case class DocComment(text: String) extends Kind

  /** A name in key position: a bare key, or any name in backticks, unescaped. */
  final case class Name(value: String, quoted: Boolean) extends Kind

  /** `@name`, in either mode: a tag before a key is an error the parser explains */
  final case class Tag(name: String) extends Kind

  /** An integer in a directive's arguments: an optional `-` and decimal digits with no leading zeros, as written. */
  final case class Integer(value: String) extends Kind

  /** `#name`, before its arguments */
  final case class Directive(name: String) extends Kind

  /** A scalar in value position, as text and the substitutions inside it, in order: unquoted, or `quoted` in any of
    * `"…"`, `"""…"""`, `'…'` and `'''…'''`, its escapes interpreted and the layout of a block applied. Consecutive text
    * is one part; `""` has no parts. `markers` are the loader's markers on a block, to run over its value (§5).
    */
  final case class Scalar(parts: Vector[Part], quoted: Boolean, markers: Vector[MarkerParser.MarkerCall] = Vector.empty)
      extends Kind

  sealed trait Part

  final case class Text(value: String) extends Part

  /** `${path}`, or `${?path}` when `optional`, found at `location`. */
  final case class Reference(path: Id.Branch, optional: Boolean, location: SourceLocation) extends Part

  /** What the parser asks the lexer for: the same characters mean different things on either side of `:`, and in a
    * directive's arguments.
    */
  sealed trait Mode
  case object KeyMode extends Mode
  case object ValueMode extends Mode

  /** Inside a directive's parentheses: names, integers, quoted strings, `:`, `,` and `)` (`format.md` §6); `=` too,
    * which the parser explains.
    */
  case object ArgumentMode extends Mode

  /** What a token is, for a message saying what was found where something else was expected. */
  def describe(kind: Kind): String =
    kind match {
      case LeftBrace => "'{'"
      case RightBrace => "'}'"
      case LeftBracket => "'['"
      case RightBracket => "']'"
      case LeftParen => "'('"
      case RightParen => "')'"
      case Comma => "','"
      case Dot => "'.'"
      case Colon => "':'"
      case Replace => "':='"
      case Equals => "'='"
      case Newline => "a line break"
      case End => "the end of the source"
      case DocComment(_) => "a doc comment"
      case Name(value, quoted) => Message.quote(if (quoted) Id.Key(value, Id.Root).path else value)
      case Tag(name) => Message.quote("@" + name)
      case Integer(value) => Message.quote(value)
      case Directive(name) => Message.quote("#" + Id.Key(name, Id.Root).path)
      case Scalar(parts, quoted, _) =>
        val text = written(parts)
        if (quoted) "a quoted string " + Message.cut("\"" + text + "\"") else Message.quote(text)
    }

  /** A scalar's parts as text, a substitution as `${path}`. */
  def written(parts: Vector[Part]): String =
    parts.iterator.map {
      case Text(value) => value
      case Reference(path, optional, _) => "${" + (if (optional) "?" else "") + Message.path(path) + "}"
    }.mkString
}
