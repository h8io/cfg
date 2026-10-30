package h8io.cfg.impl.defg

import h8io.cfg.CfgError

import java.io.IOException

/** An error found while loading a DEF configuration. Every error points at the place it is about. */
sealed trait DefError {
  def location: DefLocation
}

/** The source could not be read at all; the error points at its start. */
final case class SourceUnreadable(source: String, cause: IOException) extends DefError {
  def location: SourceLocation = SourceLocation(source, 1, 1)
}

/** The source is not UTF-8; the error points at the first byte that is not. Fatal for the source. */
final case class InvalidEncoding(location: SourceLocation) extends DefError

/** A carriage return that is not part of a CRLF. */
final case class LoneCarriageReturn(location: SourceLocation) extends DefError

/** A block or doc comment that is never closed; the error points at its opening. Fatal for the source. */
final case class UnterminatedComment(location: SourceLocation) extends DefError

/** The end of a block comment outside any comment. */
final case class StrayCommentEnd(location: SourceLocation) extends DefError

/** A `"` with no closing quote on its line; the error points at the opening. Fatal for the source. */
final case class UnterminatedString(location: SourceLocation) extends DefError

/** A backtick with no closing one on its line; the error points at the opening. Fatal for the source. */
final case class UnterminatedName(location: SourceLocation) extends DefError

/** A backslash escape outside the escape set; `escape` is what was written, the backslash included. */
final case class InvalidEscape(location: SourceLocation, escape: String) extends DefError

/** A key written bare that does not match `Id.SafeKeyPattern` and has to be quoted in backticks. */
final case class InvalidKey(location: SourceLocation, key: String) extends DefError

/** A `:` or `:=` after a key that is not followed by whitespace or the end of the line. */
final case class ColonWithoutSpace(location: SourceLocation) extends DefError

/** A `"` or backtick inside an unquoted scalar; the value has to be quoted as a whole. */
final case class QuoteInUnquotedScalar(location: SourceLocation) extends DefError

/** An unquoted scalar starting with `'`, which YAML would read as a quote. */
final case class ApostropheAtStart(location: SourceLocation) extends DefError

/** An `@` with no tag name after it. */
final case class MissingTagName(location: SourceLocation) extends DefError

/** `@-name`, reserved for removing an inherited tag. */
final case class ReservedTagRemoval(location: SourceLocation) extends DefError

/** `@name(`: a tag takes no arguments. */
final case class TagArguments(location: SourceLocation) extends DefError

/** A `#` with no directive name after it, such as a pasted YAML or HOCON comment. */
final case class MissingDirectiveName(location: SourceLocation) extends DefError

/** Whitespace inside `\${…}`. */
final case class WhitespaceInSubstitution(location: SourceLocation) extends DefError

/** A `\${…}` whose path cannot be read; the error points at the `\${`. */
final case class InvalidSubstitution(location: SourceLocation) extends DefError

/** A triple-quoted string, which the lexer does not read yet. Fatal for the source. */
final case class TripleQuoteNotSupported(location: SourceLocation) extends DefError

/** Every error a load reported, in the order found. The loader reports as many as it can without inventing any. */
final case class DefErrors(errors: ::[DefError]) extends CfgError
