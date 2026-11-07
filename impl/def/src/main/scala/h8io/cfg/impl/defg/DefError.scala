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

/** NEL, U+2028 or U+2029 anywhere, quotes included; `char` is the one written. In quotes it is written `\\uXXXX`. */
final case class ForeignLineBreak(location: SourceLocation, char: Char) extends DefError

/** A control character other than tab, LF and the CR of a CRLF anywhere, quotes and comments included, C1 and DEL as
  * well; `char` is the one written. NEL and a lone CR have errors of their own. In `"…"`, backticks and `"""…"""` it is
  * written as an escape; a raw string cannot hold one.
  */
final case class ControlCharacter(location: SourceLocation, char: Char) extends DefError

/** A whitespace character other than space and tab outside quotes and comments, such as NBSP, that is not a control
  * character; `char` is the one written.
  */
final case class ForeignWhitespace(location: SourceLocation, char: Char) extends DefError

/** A tab in the indentation of a line; the error points at the first one. */
final case class TabInIndentation(location: SourceLocation) extends DefError

/** A block or doc comment that is never closed; the error points at its opening. Fatal for the source. */
final case class UnterminatedComment(location: SourceLocation) extends DefError

/** The end of a block comment outside any comment. */
final case class StrayCommentEnd(location: SourceLocation) extends DefError

/** A string that is never closed: `"…"` or `'…'` not closed on its line, a triple-quoted block not closed at all; the
  * error points at the opening. Fatal for the source.
  */
final case class UnterminatedString(location: SourceLocation) extends DefError

/** Whitespace right after the opening delimiter of a triple-quoted block, or at the end of its line after the markers;
  * the error points at the first whitespace character. The markers are applied all the same.
  */
final case class WhitespaceAfterDelimiter(location: SourceLocation) extends DefError

/** The markers after the opening delimiter of a triple-quoted block cannot be read: names separated by whitespace, each
  * with arguments in parentheses or none (`format.md` §5). The error points at the first character that does not fit,
  * the rest of the line is not looked at, and the block is read as with no marker. A string meant to be one line and
  * not closed on it gives this error, or an [[UnknownMarker]]: a block starts on the line after the delimiter.
  */
final case class InvalidMarker(location: SourceLocation) extends DefError

/** A marker the loader has no handler for; `name` is its name. The block is read as with no marker. */
final case class UnknownMarker(location: SourceLocation, name: String) extends DefError

/** A second marker that sets the indent, such as `>` after `<`; the block is read as with no marker. */
final case class ConflictingIndent(location: SourceLocation) extends DefError

/** A named argument the marker does not take; `argument` is its name. */
final case class UnknownArgument(location: SourceLocation, argument: String) extends DefError

/** A positional argument past the last parameter. */
final case class ExtraArgument(location: SourceLocation) extends DefError

/** A positional argument after a named one: positional arguments come first, as in Scala. */
final case class PositionalAfterNamed(location: SourceLocation) extends DefError

/** An argument given twice, by name or by position and then by name; the error points at the second. */
final case class DuplicateArgument(location: SourceLocation, argument: String) extends DefError

/** An argument the marker requires and was not given; the error points at the marker. */
final case class MissingArgument(location: SourceLocation, argument: String) extends DefError

/** An argument whose value the marker cannot take, such as a width that is not a positive integer; the error points at
  * the value.
  */
final case class InvalidArgument(location: SourceLocation, argument: String) extends DefError

/** `\${` in a string argument, which is literal; escaped, it is text. */
final case class SubstitutionInArgument(location: SourceLocation) extends DefError

/** With no marker, two lines of a triple-quoted block whose indents are not one a prefix of the other, such as a tab
  * where the other has a space; the error points at the first character that differs.
  */
final case class InconsistentIndent(location: SourceLocation) extends DefError

/** With an explicit width, a line of a triple-quoted block that does not start with the indent of the opening line and
  * the width; the error points at the first character that falls short.
  */
final case class InsufficientIndent(location: SourceLocation) extends DefError

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

/** Every error a load reported, in the order found. The loader reports as many as it can without inventing any. */
final case class DefErrors(errors: ::[DefError]) extends CfgError
