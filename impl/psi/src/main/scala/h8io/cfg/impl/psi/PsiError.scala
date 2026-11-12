package h8io.cfg.impl.psi

import h8io.cfg.CfgError

import java.io.IOException

/** An error found while loading a Psi configuration. Every error points at the place it is about. */
sealed trait PsiError {
  def location: PsiLocation
}

/** The source could not be read at all; the error points at its start. */
final case class SourceUnreadable(source: String, cause: IOException) extends PsiError {
  def location: SourceLocation = SourceLocation(source, 1, 1)
}

/** The source is not UTF-8; the error points at the first byte that is not. Fatal for the source. */
final case class InvalidEncoding(location: SourceLocation) extends PsiError

/** A carriage return that is not part of a CRLF. */
final case class LoneCarriageReturn(location: SourceLocation) extends PsiError

/** NEL, U+2028 or U+2029 anywhere, quotes included; `char` is the one written. In quotes it is written `\\uXXXX`. */
final case class ForeignLineBreak(location: SourceLocation, char: Char) extends PsiError

/** A control character other than tab, LF and the CR of a CRLF anywhere, quotes and comments included, C1 and DEL as
  * well; `char` is the one written. NEL and a lone CR have errors of their own. In `"…"`, backticks and `"""…"""` it is
  * written as an escape; a raw string cannot hold one.
  */
final case class ControlCharacter(location: SourceLocation, char: Char) extends PsiError

/** A bidirectional formatting or invisible character anywhere, quotes and comments included: U+202A–U+202E,
  * U+2066–U+2069, U+200B, U+2060–U+2064, or U+FEFF past the start; `char` is the one written. In `"…"`, backticks and
  * `"""…"""` it is written as an escape; a raw string cannot hold one.
  */
final case class InvisibleCharacter(location: SourceLocation, char: Char) extends PsiError

/** A whitespace character other than space and tab outside quotes and comments, such as NBSP, that is not a control
  * character; `char` is the one written.
  */
final case class ForeignWhitespace(location: SourceLocation, char: Char) extends PsiError

/** A tab in the indentation of a line; the error points at the first one. */
final case class TabInIndentation(location: SourceLocation) extends PsiError

/** A block or doc comment that is never closed; the error points at its opening. Fatal for the source. */
final case class UnterminatedComment(location: SourceLocation) extends PsiError

/** The end of a block comment outside any comment. */
final case class StrayCommentEnd(location: SourceLocation) extends PsiError

/** A string that is never closed: `"…"` or `'…'` not closed on its line, a triple-quoted block not closed at all; the
  * error points at the opening. Fatal for the source.
  */
final case class UnterminatedString(location: SourceLocation) extends PsiError

/** Whitespace right after the opening delimiter of a triple-quoted block, or at the end of its line after the markers;
  * the error points at the first whitespace character. The markers are applied all the same.
  */
final case class WhitespaceAfterDelimiter(location: SourceLocation) extends PsiError

/** The markers after the opening delimiter of a triple-quoted block cannot be read: names separated by whitespace, each
  * with arguments in parentheses or none (`format.md` §5). The error points at the first character that does not fit,
  * the rest of the line is not looked at, and the block is read as with no marker. A string meant to be one line and
  * not closed on it gives this error, or an [[UnknownMarker]]: a block starts on the line after the delimiter.
  */
final case class InvalidMarker(location: SourceLocation) extends PsiError

/** A marker the loader has no handler for; `name` is its name. The block is read as with no marker. */
final case class UnknownMarker(location: SourceLocation, name: String) extends PsiError

/** A second marker that sets the indent, such as `>` after `<`; the block is read as with no marker. */
final case class ConflictingIndent(location: SourceLocation) extends PsiError

/** A named argument the marker or directive does not take; `argument` is its name. */
final case class UnknownArgument(location: SourceLocation, argument: String) extends PsiError

/** A positional argument past the last parameter. */
final case class ExtraArgument(location: SourceLocation) extends PsiError

/** A positional argument after a named one: positional arguments come first, as in Scala. */
final case class PositionalAfterNamed(location: SourceLocation) extends PsiError

/** An argument given twice, by name or by position and then by name; the error points at the second. */
final case class DuplicateArgument(location: SourceLocation, argument: String) extends PsiError

/** An argument the marker or directive requires and was not given; the error points at the call. */
final case class MissingArgument(location: SourceLocation, argument: String) extends PsiError

/** An argument whose value the marker cannot take, such as a width that is not a positive integer; the error points at
  * the value.
  */
final case class InvalidArgument(location: SourceLocation, argument: String) extends PsiError

/** `\${` in a string argument, which is literal; escaped, it is text. */
final case class SubstitutionInArgument(location: SourceLocation) extends PsiError

/** With no marker, two lines of a triple-quoted block whose indents are not one a prefix of the other, such as a tab
  * where the other has a space; the error points at the first character that differs.
  */
final case class InconsistentIndent(location: SourceLocation) extends PsiError

/** With an explicit width, a line of a triple-quoted block that does not start with the indent of the opening line and
  * the width; the error points at the first character that falls short.
  */
final case class InsufficientIndent(location: SourceLocation) extends PsiError

/** A backtick with no closing one on its line; the error points at the opening. Fatal for the source. */
final case class UnterminatedName(location: SourceLocation) extends PsiError

/** A backslash escape outside the escape set; `escape` is what was written, the backslash included. */
final case class InvalidEscape(location: SourceLocation, escape: String) extends PsiError

/** A key written bare that does not match `Id.SafeKeyPattern` and has to be quoted in backticks. */
final case class InvalidKey(location: SourceLocation, key: String) extends PsiError

/** A `:` or `:=` after a key, or a `:` after the name of an argument, that is not followed by whitespace or the end of
  * the line.
  */
final case class ColonWithoutSpace(location: SourceLocation) extends PsiError

/** A `"` or backtick inside an unquoted scalar; the value has to be quoted as a whole. */
final case class QuoteInUnquotedScalar(location: SourceLocation) extends PsiError

/** An `@` with no tag name after it. */
final case class MissingTagName(location: SourceLocation) extends PsiError

/** `@-name`, reserved for removing an inherited tag. */
final case class ReservedTagRemoval(location: SourceLocation) extends PsiError

/** `@name(`: a tag takes no arguments. */
final case class TagArguments(location: SourceLocation) extends PsiError

/** A `#` with no directive name after it, such as a pasted YAML or HOCON comment. */
final case class MissingDirectiveName(location: SourceLocation) extends PsiError

/** Whitespace inside `\${…}`. */
final case class WhitespaceInSubstitution(location: SourceLocation) extends PsiError

/** A `\${…}` whose path cannot be read; the error points at the `\${`. */
final case class InvalidSubstitution(location: SourceLocation) extends PsiError

/** Where a key is expected, at the start of a field or after a dot, something that cannot be one, such as a `:` or a
  * bracket.
  */
final case class ExpectedKey(location: SourceLocation) extends PsiError

/** A key in `"…"` or `'…'`: a quoted key is written in backticks. */
final case class QuotedKey(location: SourceLocation) extends PsiError

/** A `"""` or `\'\'\'` string in a directive's arguments, which take `"…"` and `'…'` only; read all the same. */
final case class TripleQuotedArgument(location: SourceLocation) extends PsiError

/** An unquoted word in a directive's arguments that is neither a name by the key rule nor an integer, such as `1.5`,
  * `007` or `a.b`; `literal` is the word. A value of any other shape is quoted.
  */
final case class InvalidLiteral(location: SourceLocation, literal: String) extends PsiError

/** The `(` of a directive's arguments apart from its name, after whitespace, a comment or a newline: it touches the
  * name, `#name(…)`. The error points at the `(`.
  */
final case class DetachedArguments(location: SourceLocation) extends PsiError

/** `name = value` in the arguments of a directive or a marker: a named argument is written as a field is,
  * `name: value`. The error points at the `=`.
  */
final case class EqualsInArgument(location: SourceLocation) extends PsiError

/** In a directive's arguments, something that cannot start one, such as `=` or a tag. */
final case class ExpectedArgument(location: SourceLocation) extends PsiError

/** A value right after another, as in HOCON's `"jdbc:"\${host}` or `"a" \${b}`: values never join, and a concatenation
  * is one scalar with `\${…}` inside it, `"jdbc:\${host}"`. The error points at the second value.
  */
final case class JoinedValues(location: SourceLocation) extends PsiError

/** A value in backticks, which are for names: a quoted value is written in `"…"`. */
final case class BacktickedValue(location: SourceLocation) extends PsiError

/** A tag before a key: it goes after the `:`, before the value, as in `key: @tag value`. */
final case class TagBeforeKey(location: SourceLocation) extends PsiError

/** Whitespace or a comment before or after a dot of a key, which has none, as `Id.path` writes it; the error points at
  * the dot.
  */
final case class SpaceAroundDot(location: SourceLocation) extends PsiError

/** After a key, neither a dot, `:`, `:=` nor `{`, such as HOCON's `=` or nothing at all. */
final case class ExpectedColon(location: SourceLocation) extends PsiError

/** The `{` of `key { … }` on a line after the key's: it goes on the key's line. */
final case class BraceOnNextLine(location: SourceLocation) extends PsiError

/** A tag between a key and its value, as in `key @tag { … }`: it goes after a `:`, `key: @tag { … }`. */
final case class TagAfterKey(location: SourceLocation) extends PsiError

/** No value after a `:`, `:=` or tag, such as a `,` or a closing bracket where one should be. */
final case class MissingValue(location: SourceLocation) extends PsiError

/** A value on a line after its `:`, `:=` or tag: it goes on the same line (`format.md` §4). */
final case class ValueOnNextLine(location: SourceLocation) extends PsiError

/** After a value, something other than a separator or a closing bracket, such as a second field on the line; whitespace
  * never separates. The error points at what follows the value.
  */
final case class MissingSeparator(location: SourceLocation) extends PsiError

/** A second word after an unquoted scalar, as in `a: foo bar`: a value with whitespace in it is quoted. The error
  * points at the second word.
  */
final case class SpaceInValue(location: SourceLocation) extends PsiError

/** An empty element, field or argument: two `,` with nothing between them, or a `,` before the first one; the error
  * points at the `,` that has nothing before it.
  */
final case class EmptyElement(location: SourceLocation) extends PsiError

/** A doc comment that does not stand before a field, a sequence element or a directive. */
final case class MisplacedDocComment(location: SourceLocation) extends PsiError

/** A `}`, `]` or `)` that closes nothing, or not the bracket opened last. */
final case class UnmatchedBracket(location: SourceLocation) extends PsiError

/** A `{`, `[` or `(` that the source ends without closing; the error points at the opening. */
final case class UnclosedBracket(location: SourceLocation) extends PsiError

/** A `:` merge of a map or a substitution, written at `location`, with an earlier value of its key where either is a
  * whole-value substitution, which may turn out a map or not (`format.md` §7); `:=` replaces instead.
  *
  * @param earlier
  *   where the earlier value was defined
  */
final case class MergeWithSubstitution(location: PsiLocation, earlier: PsiLocation) extends PsiError

/** `\${path}`, alone or in a concatenation, at `location`, the `\${`, with nothing at `path` in the tree (`format.md`
  * §9).
  */
final case class UnresolvedSubstitution(location: SourceLocation, path: String) extends PsiError

/** `\${path}` at `location` that leads back to itself, directly or through other substitutions (§9). Reported once, at
  * the reference that closes the cycle.
  */
final case class SubstitutionCycle(location: SourceLocation, path: String) extends PsiError

/** `\${path}` at `location`, a part of a concatenation, referring to a node that is not a scalar: a map, a sequence or
  * `null`, defined at `origin` (§9).
  */
final case class NonScalarInConcatenation(location: SourceLocation, path: String, origin: PsiLocation) extends PsiError

/** A loader's marker that failed on a block's value, with its handler's error (`format.md` §5); the block stands for
  * its value with none of the loader's markers.
  */
final case class MarkerFailed(location: SourceLocation, name: String, error: MarkerError) extends PsiError

/** A built-in marker after one of the loader's: the built-in ones shape the lines, before the loader's run over the
  * value (§5); the block is read as with no marker.
  */
final case class BuiltInMarkerAfterLoaders(location: SourceLocation, name: String) extends PsiError

/** A call of a directive the loader has no handler for (`format.md` §6). */
final case class UnknownDirective(location: SourceLocation, name: String) extends PsiError

/** A call of a directive the loader was built with disabled (§6). */
final case class DisabledDirective(location: SourceLocation, name: String) extends PsiError

/** A call whose handler failed, with the handler's error. */
final case class DirectiveFailed(location: SourceLocation, name: String, error: DirectiveError) extends PsiError

/** A call in statement position whose handler gave something other than a map. */
final case class DirectiveNotMap(location: SourceLocation, name: String) extends PsiError

/** A call that parses a source already being expanded (§6).
  *
  * @param chain
  *   the sources from the one loaded to the one parsed again, each as locations name it
  */
final case class DirectiveCycle(location: SourceLocation, chain: ::[String]) extends PsiError

/** A call that parses a source `limit` sources deep: a backstop for a cycle no key catches (§6). */
final case class DirectiveTooDeep(location: SourceLocation, limit: Int) extends PsiError

/** Something this version of the loader cannot load yet: `#include`. Plan step 11 replaces it with the real thing.
  *
  * @param what
  *   what it is, as `#name`
  */
final case class NotSupportedYet(location: SourceLocation, what: String) extends PsiError

/** Every error a load reported, in the order found. The loader reports as many as it can without inventing any. */
final case class PsiErrors(errors: ::[PsiError]) extends CfgError
