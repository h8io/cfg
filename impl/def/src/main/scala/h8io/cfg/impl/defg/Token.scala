package h8io.cfg.impl.defg

import h8io.cfg.Id

/** A token of a source.
  *
  * @param startsLine
  *   whether no other token stands before it on its line; with its column, what the indentation syntax will need
  */
private[defg] final case class Token(kind: Token.Kind, location: SourceLocation, startsLine: Boolean)

private[defg] object Token {
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

  /** `@name` */
  final case class Tag(name: String) extends Kind

  /** `#name`, before its arguments */
  final case class Directive(name: String) extends Kind

  /** A scalar in value position, unquoted or in `"…"`, as text and the substitutions inside it, in order. Consecutive
    * text is one part; `""` has no parts.
    */
  final case class Scalar(parts: Vector[Part], quoted: Boolean) extends Kind

  sealed trait Part

  final case class Text(value: String) extends Part

  /** `${path}`, or `${?path}` when `optional`, found at `location`. */
  final case class Reference(path: Id.Branch, optional: Boolean, location: SourceLocation) extends Part

  /** What the parser asks the lexer for: the same characters mean different things on either side of `:`. */
  sealed trait Mode
  case object KeyMode extends Mode
  case object ValueMode extends Mode
}
