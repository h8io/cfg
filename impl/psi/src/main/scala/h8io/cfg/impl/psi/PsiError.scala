package h8io.cfg.impl.psi

import java.io.IOException
import java.time.Duration

/** An error found while loading a Psi configuration. Every error points at the place it is about, says what is wrong
  * there and, where it can, what to do about it (`format.md` §11). The texts are for people and may change in any
  * release; a program reads an error by its type and fields.
  */
sealed trait PsiError {
  def location: PsiLocation

  /** What is wrong, naming what was found; computed from the fields, never stored. */
  def message: String

  /** What to do about it, apart from the message so that a tool can show it on its own. */
  def hint: Option[String] = None
}

/** The source could not be read at all; the error points at its start. */
final case class SourceUnreadable(source: String, cause: IOException) extends PsiError {
  def location: SourceLocation = SourceLocation(source, 1, 1)

  def message: String = "the source cannot be read: " + cause.toString
}

/** The source is not UTF-8; the error points at the first byte that is not. Fatal for the source. */
final case class InvalidEncoding(location: SourceLocation) extends PsiError {
  def message: String = "a byte that is not UTF-8" + Message.Fatal

  override def hint: Option[String] = Some("save the source as UTF-8")
}

/** Reading the sources of a load taking more than `limit` bytes in all, every read counted (`format.md` §6, §11): at
  * the source whose reading went past, after which every read of the load is dropped and not reported.
  */
final case class ReadLimitExceeded(source: String, limit: Int) extends PsiError {
  def location: SourceLocation = SourceLocation(source, 1, 1)

  def message: String =
    "reading the sources of this load takes more than " + limit.toString + " bytes, a source read twice counted twice"

  override def hint: Option[String] =
    Some("include fewer or smaller sources, or build the loader with a higher withReadLimit")
}

/** Reading the sources of a load taking longer than `limit` (`format.md` §6, §11): at the source being read then, after
  * which every read of the load is dropped and not reported.
  */
final case class ReadTimeExceeded(source: String, limit: Duration) extends PsiError {
  def location: SourceLocation = SourceLocation(source, 1, 1)

  def message: String = "reading the sources of this load takes longer than " + Message.duration(limit)

  override def hint: Option[String] =
    Some("make the sources quicker to reach, or build the loader with a longer withReadLimit")
}

/** A carriage return that is not part of a CRLF. */
final case class LoneCarriageReturn(location: SourceLocation) extends PsiError {
  def message: String = "a carriage return, " + Message.char('\r') + ", with no line feed after it"

  override def hint: Option[String] = Some("end lines with LF or CRLF")
}

/** NEL, U+2028 or U+2029 anywhere, quotes included; `char` is the one written. In quotes it is written `\\uXXXX`. */
final case class ForeignLineBreak(location: SourceLocation, char: Char) extends PsiError {
  def message: String = "a line break other than LF and CRLF, " + Message.char(char)

  override def hint: Option[String] =
    Some("end lines with LF or CRLF; in a string, write it as " + Message.escape(char))
}

/** A control character other than tab, LF and the CR of a CRLF anywhere, quotes and comments included, C1 and DEL as
  * well; `char` is the one written. NEL and a lone CR have errors of their own. In `"…"`, backticks and `"""…"""` it is
  * written as an escape; a raw string cannot hold one.
  */
final case class ControlCharacter(location: SourceLocation, char: Char) extends PsiError {
  def message: String = "a control character, " + Message.char(char)

  override def hint: Option[String] = Some("remove it, or in \"…\" write it as " + Message.escape(char))
}

/** A bidirectional formatting or invisible character anywhere, quotes and comments included: U+202A–U+202E,
  * U+2066–U+2069, U+200B, U+2060–U+2064, or U+FEFF past the start; `char` is the one written. In `"…"`, backticks and
  * `"""…"""` it is written as an escape; a raw string cannot hold one.
  */
final case class InvisibleCharacter(location: SourceLocation, char: Char) extends PsiError {
  def message: String = "an invisible character, " + Message.char(char)

  override def hint: Option[String] = Some("remove it, or in \"…\" write it as " + Message.escape(char))
}

/** A whitespace character other than space and tab outside quotes and comments, such as NBSP, that is not a control
  * character; `char` is the one written.
  */
final case class ForeignWhitespace(location: SourceLocation, char: Char) extends PsiError {
  def message: String = "whitespace other than space and tab, " + Message.char(char)

  override def hint: Option[String] = Some("write a space instead")
}

/** A tab in the indentation of a line; the error points at the first one. */
final case class TabInIndentation(location: SourceLocation) extends PsiError {
  def message: String = "a tab in the indentation"

  override def hint: Option[String] = Some("indent with spaces")
}

/** A block or doc comment that is never closed; the error points at its opening. Fatal for the source. */
final case class UnterminatedComment(location: SourceLocation) extends PsiError {
  def message: String = "a comment that is never closed" + Message.Fatal

  override def hint: Option[String] = Some("close it with '*/'; comments nest, so each '/*' inside needs its own")
}

/** The end of a block comment outside any comment. */
final case class StrayCommentEnd(location: SourceLocation) extends PsiError {
  def message: String = "'*/' outside any comment"
}

/** A string that is never closed: `"…"` or `'…'` not closed on its line, a triple-quoted block not closed at all; the
  * error points at the opening, `delimiter`. Fatal for the source.
  */
final case class UnterminatedString(location: SourceLocation, delimiter: String) extends PsiError {
  def message: String =
    "a string opened with " + delimiter + " is never closed" + (if (delimiter.length == 1) " on its line" else "") +
      Message.Fatal

  override def hint: Option[String] =
    Some(
      if (delimiter.length == 1)
        "close it on its line; a value on several lines is written in " + delimiter * 3 + "…" + delimiter * 3
      else "close it with " + delimiter)
}

/** Whitespace right after the opening delimiter of a triple-quoted block, or at the end of its line after the markers;
  * the error points at the first whitespace character. The markers are applied all the same.
  */
final case class WhitespaceAfterDelimiter(location: SourceLocation) extends PsiError {
  def message: String = "whitespace on the opening line of a block, which holds only its markers"

  override def hint: Option[String] = Some("remove it")
}

/** The markers after the opening delimiter of a triple-quoted block cannot be read: names separated by whitespace, each
  * with arguments in parentheses or none (`format.md` §5). The error points at the first character that does not fit,
  * the rest of the line is not looked at, and the block is read as with no marker. A string meant to be one line and
  * not closed on it gives this error, or an [[UnknownMarker]]: a block starts on the line after the delimiter.
  */
final case class InvalidMarker(location: SourceLocation) extends PsiError {
  def message: String = "the markers of a block cannot be read from here; the block is read with no marker"

  override def hint: Option[String] =
    Some(
      "markers are names separated by spaces, each with its arguments in parentheses or none; " +
        "the block itself starts on the next line")
}

/** A marker the loader has no handler for; `name` is its name. The block is read as with no marker. */
final case class UnknownMarker(location: SourceLocation, name: String) extends PsiError {
  def message: String = "unknown marker " + Message.quote(name) + "; the block is read with no marker"
}

/** A second marker that sets the indent, `marker`, such as `>` after `<`; the block is read as with no marker. */
final case class ConflictingIndent(location: SourceLocation, marker: String) extends PsiError {
  def message: String =
    "marker " + Message.quote(marker) + " sets the indent a second time; the block is read with no marker"

  override def hint: Option[String] = Some("keep one marker that sets the indent")
}

/** A named argument the marker or directive does not take; `argument` is its name. */
final case class UnknownArgument(location: SourceLocation, argument: String) extends PsiError {
  def message: String = "no argument named " + Message.quote(argument) + " is taken here"
}

/** A positional argument past the last parameter. */
final case class ExtraArgument(location: SourceLocation) extends PsiError {
  def message: String = "an argument past the last parameter"
}

/** A positional argument after a named one: positional arguments come first, as in Scala. */
final case class PositionalAfterNamed(location: SourceLocation) extends PsiError {
  def message: String = "a positional argument after a named one"

  override def hint: Option[String] = Some("put positional arguments first, as in Scala")
}

/** An argument given twice, by name or by position and then by name; the error points at the second. */
final case class DuplicateArgument(location: SourceLocation, argument: String) extends PsiError {
  def message: String = "argument " + Message.quote(argument) + " is given twice"
}

/** An argument the marker or directive requires and was not given; the error points at the call. */
final case class MissingArgument(location: SourceLocation, argument: String) extends PsiError {
  def message: String = "argument " + Message.quote(argument) + " is required and not given"
}

/** An argument whose value the marker cannot take, such as a width that is not a positive integer; the error points at
  * the value.
  */
final case class InvalidArgument(location: SourceLocation, argument: String) extends PsiError {
  def message: String = "the value of argument " + Message.quote(argument) + " cannot be taken"
}

/** `\${` in a string argument, which is literal; escaped, it is text. */
final case class SubstitutionInArgument(location: SourceLocation) extends PsiError {
  def message: String = "a substitution in an argument, which is taken as written"

  override def hint: Option[String] = Some("to mean the text, quote it, and write '\\${' for '${'")
}

/** With no marker, two lines of a triple-quoted block whose indents are not one a prefix of the other, such as a tab
  * where the other has a space; the error points at the first character that differs.
  */
final case class InconsistentIndent(location: SourceLocation) extends PsiError {
  def message: String = "the indent of this line differs from an earlier one's, such as a tab against a space"

  override def hint: Option[String] = Some("indent the lines of a block alike")
}

/** With an explicit width, a line of a triple-quoted block that does not start with the indent of the opening line and
  * the width; the error points at the first character that falls short.
  */
final case class InsufficientIndent(location: SourceLocation) extends PsiError {
  def message: String = "a line of a block indented less than its width requires"
}

/** A backtick with no closing one on its line; the error points at the opening. Fatal for the source. */
final case class UnterminatedName(location: SourceLocation) extends PsiError {
  def message: String = "a name in backticks that is never closed on its line" + Message.Fatal

  override def hint: Option[String] = Some("close it with a backtick on its line")
}

/** A backslash escape outside the escape set; `escape` is what was written, the backslash included. */
final case class InvalidEscape(location: SourceLocation, escape: String) extends PsiError {
  def message: String = "unknown escape " + Message.quote(escape)

  override def hint: Option[String] = Some("write a backslash as '\\\\'")
}

/** A key written bare that does not match `Id.SafeKeyPattern` and has to be quoted in backticks. */
final case class InvalidKey(location: SourceLocation, key: String) extends PsiError {
  def message: String = Message.quote(key) + " cannot be a bare key"

  override def hint: Option[String] = Some("quote it in backticks: " + Message.key(key))
}

/** A `:` or `:=` after a key, or a `:` after the name of an argument, that is not followed by whitespace or the end of
  * the line.
  */
final case class ColonWithoutSpace(location: SourceLocation) extends PsiError {
  def message: String = "no space after the colon"

  override def hint: Option[String] = Some("put a space after it, as in 'key: value'")
}

/** A `$` outside quotes that opens no substitution: no path after it, bare or in backticks (`format.md` §3, §9). */
final case class StrayDollar(location: SourceLocation) extends PsiError {
  def message: String = "a '$' outside quotes opens a substitution, and no path follows it"

  override def hint: Option[String] = Some("write the path after it, or quote the value: \"…$…\"")
}

/** A `"` or backtick inside an unquoted scalar; the value has to be quoted as a whole. */
final case class QuoteInUnquotedScalar(location: SourceLocation) extends PsiError {
  def message: String = "a quote inside an unquoted value"

  override def hint: Option[String] = Some("quote the whole value in \"…\"")
}

/** An `@` with no tag name after it. */
final case class MissingTagName(location: SourceLocation) extends PsiError {
  def message: String = "'@' with no tag name after it"
}

/** `@-name`, reserved for removing an inherited tag. */
final case class ReservedTagRemoval(location: SourceLocation) extends PsiError {
  def message: String = "'@-' is reserved for removing an inherited tag"
}

/** `@name(`: a tag takes no arguments. */
final case class TagArguments(location: SourceLocation) extends PsiError {
  def message: String = "arguments to a tag, which takes none"
}

/** A `#` with no directive name after it, such as a pasted YAML or HOCON comment. */
final case class MissingDirectiveName(location: SourceLocation) extends PsiError {
  def message: String = "'#' with no directive name after it"

  override def hint: Option[String] = Some("a comment is written '//' or '/* … */'")
}

/** Whitespace inside `\${…}`. */
final case class WhitespaceInSubstitution(location: SourceLocation) extends PsiError {
  def message: String = "whitespace inside '${" + "…}'"

  override def hint: Option[String] = Some("remove it")
}

/** A `\${…}` whose path cannot be read; the error points at the `\${`. */
final case class InvalidSubstitution(location: SourceLocation) extends PsiError {
  def message: String = "the path of a substitution cannot be read"

  override def hint: Option[String] =
    Some("a path is keys joined by dots and indexes in brackets, a key that is not bare in backticks")
}

/** Where a key is expected, at the start of a field or after a dot, something that cannot be one, such as a `:` or a
  * bracket; `found` says what.
  */
final case class ExpectedKey(location: SourceLocation, found: String) extends PsiError {
  def message: String = "expected a key, found " + found
}

/** A key in `"…"` or `'…'`, `key` its text: a quoted key is written in backticks. */
final case class QuotedKey(location: SourceLocation, key: String) extends PsiError {
  def message: String = "a key in quotes"

  override def hint: Option[String] =
    Some("write a key bare, or in backticks when it has to be quoted: " + Message.key(key))
}

/** A `"""` or `\'\'\'` string in a directive's arguments, which take `"…"` and `'…'` only; read all the same. */
final case class TripleQuotedArgument(location: SourceLocation) extends PsiError {
  def message: String = "a triple-quoted string in a directive's arguments; it is read all the same"

  override def hint: Option[String] = Some("write it in \"…\" or '…'")
}

/** An unquoted word in a directive's arguments that is neither a name by the key rule nor an integer, such as `1.5`,
  * `007` or `a.b`; `literal` is the word. A value of any other shape is quoted.
  */
final case class InvalidLiteral(location: SourceLocation, literal: String) extends PsiError {
  def message: String = Message.quote(literal) + " is neither a name nor an integer"

  override def hint: Option[String] = Some("quote it: " + Message.cut("\"" + literal + "\""))
}

/** The `(` of a directive's arguments apart from its name, after whitespace, a comment or a newline: it touches the
  * name, `#name(…)`. The error points at the `(`.
  */
final case class DetachedArguments(location: SourceLocation) extends PsiError {
  def message: String = "the '(' of a directive's arguments apart from its name"

  override def hint: Option[String] = Some("write it touching the name, as in '#name(…)'")
}

/** `name = value` in the arguments of a directive or a marker: a named argument is written as a field is,
  * `name: value`. The error points at the `=`.
  */
final case class EqualsInArgument(location: SourceLocation) extends PsiError {
  def message: String = "'=' in an argument"

  override def hint: Option[String] = Some("write a named argument as a field is, 'name: value'")
}

/** In a directive's arguments, something that cannot start one, such as `=` or a tag; `found` says what. */
final case class ExpectedArgument(location: SourceLocation, found: String) extends PsiError {
  def message: String = "expected an argument, found " + found
}

/** A value right after another, as in HOCON's `"jdbc:"\${host}` or `"a" \${b}`: values never join, and a concatenation
  * is one scalar with `\${…}` inside it, `"jdbc:\${host}"`. The error points at the second value.
  */
final case class JoinedValues(location: SourceLocation) extends PsiError {
  def message: String = "a value right after another; values never join"

  override def hint: Option[String] = Some("write a concatenation as one string, as in \"jdbc:${" + "host}\"")
}

/** A value in backticks, which are for names: a quoted value is written in `"…"`. */
final case class BacktickedValue(location: SourceLocation) extends PsiError {
  def message: String = "a value in backticks, which are for names"

  override def hint: Option[String] = Some("quote a value in \"…\"")
}

/** A tag before a key: it goes after the `:`, before the value, as in `key: @tag value`. */
final case class TagBeforeKey(location: SourceLocation) extends PsiError {
  def message: String = "a tag before a key"

  override def hint: Option[String] = Some("put it after the ':', as in 'key: @tag value'")
}

/** Whitespace or a comment before or after a dot of a key, which has none, as `Id.path` writes it; the error points at
  * the dot.
  */
final case class SpaceAroundDot(location: SourceLocation) extends PsiError {
  def message: String = "whitespace around a dot of a key"

  override def hint: Option[String] = Some("write the keys and the dots touching, as in 'a.b'")
}

/** After a key, neither a dot, `:`, `:=` nor `{`, such as HOCON's `=` or nothing at all; `found` says what. */
final case class ExpectedColon(location: SourceLocation, found: String) extends PsiError {
  def message: String = "expected ':', ':=' or '{' after a key, found " + found

  override def hint: Option[String] = Some("write a field as 'key: value' or 'key { … }'")
}

/** The `{` of `key { … }` on a line after the key's: it goes on the key's line. */
final case class BraceOnNextLine(location: SourceLocation) extends PsiError {
  def message: String = "the '{' of a block on a line after its key"

  override def hint: Option[String] = Some("put it on the key's line, as in 'key {'")
}

/** A tag between a key and its value, as in `key @tag { … }`: it goes after a `:`, `key: @tag { … }`. */
final case class TagAfterKey(location: SourceLocation) extends PsiError {
  def message: String = "a tag between a key and its value"

  override def hint: Option[String] = Some("put it after a ':', as in 'key: @tag { … }'")
}

/** No value after a `:`, `:=` or tag, such as a `,` or a closing bracket where one should be; `found` says what. */
final case class MissingValue(location: SourceLocation, found: String) extends PsiError {
  def message: String = "expected a value, found " + found
}

/** A value on a line after its tag inside `[…]`, where indentation means nothing: it goes on the tag's line
  * (`format.md` §4).
  */
final case class ValueOnNextLine(location: SourceLocation) extends PsiError {
  def message: String = "a value on a line after its tag"

  override def hint: Option[String] = Some("put it on the same line")
}

/** A `:`, `:=`, tag or `- ` at the end of a line, `location`, with no line indented deeper after it to hold its value
  * (`format.md` §4).
  */
final case class NothingIndented(location: SourceLocation) extends PsiError {
  def message: String = "nothing indented deeper below to be the value"

  override def hint: Option[String] =
    Some("indent the value on the lines below, or write it on this line; an empty block is '{}' or '[]'")
}

/** A line indented deeper than the lines of its block, where nothing opens a block: Psi has no continuation lines, and
  * the top level starts at column 1 (`format.md` §4).
  */
final case class UnexpectedIndent(location: SourceLocation) extends PsiError {
  def message: String = "a line indented deeper than its block, where nothing opens one"

  override def hint: Option[String] = Some("align it with the lines of its block")
}

/** A line indented less than the lines of its block, landing on the width of no block still open (`format.md` §4). */
final case class MisalignedIndent(location: SourceLocation) extends PsiError {
  def message: String = "a line indented less than its block lands on the width of no block still open"

  override def hint: Option[String] = Some("align it with the lines of the block it belongs to")
}

/** A block opened by indentation that holds `- ` elements and anything else (`format.md` §4). */
final case class MixedBlock(location: SourceLocation) extends PsiError {
  def message: String = "a block holds '- ' elements or anything else, not both"

  override def hint: Option[String] = Some("give the elements a key of their own, or write them all with '- '")
}

/** A second value in a block opened by indentation, which holds one value, fields or `- ` elements (`format.md` §4).
  */
final case class SecondValue(location: SourceLocation) extends PsiError {
  def message: String = "a second value in a block that holds one"

  override def hint: Option[String] = Some("write each as an element, '- value', or give each a key")
}

/** A value among the fields of a block opened by indentation, with no key (`format.md` §4). */
final case class ValueAmongFields(location: SourceLocation) extends PsiError {
  def message: String = "a value among the fields of a block"

  override def hint: Option[String] = Some("write a key and ':' before it")
}

/** A tag before the first field of a map written by indentation, which has no place for one (`format.md` §4). */
final case class TagBeforeField(location: SourceLocation) extends PsiError {
  def message: String = "a tag before the first field of a map, which has no place for it"

  override def hint: Option[String] =
    Some("write the map in braces after the tag, as in '- @t { host: a }', or the fields on the lines below the tag")
}

/** After a value, something other than a separator or a closing bracket, such as a second field on the line; whitespace
  * never separates. The error points at what follows the value, `found`.
  */
final case class MissingSeparator(location: SourceLocation, found: String) extends PsiError {
  def message: String = "expected ',' or a line break, found " + found

  override def hint: Option[String] = Some("separate items with ',' or a line break; whitespace never separates")
}

/** A second word after an unquoted scalar, as in `a: foo bar`: a value with whitespace in it is quoted. The error
  * points at the second word, `word`.
  */
final case class SpaceInValue(location: SourceLocation, word: String) extends PsiError {
  def message: String = "a second word, " + Message.quote(word) + ", after a value"

  override def hint: Option[String] = Some("quote a value with whitespace in it, as in \"foo bar\"")
}

/** An empty element, field or argument: two `,` with nothing between them, or a `,` before the first one; the error
  * points at the `,` that has nothing before it.
  */
final case class EmptyElement(location: SourceLocation) extends PsiError {
  def message: String = "a ',' with nothing before it"

  override def hint: Option[String] = Some("remove it")
}

/** A doc comment that does not stand before a field, a sequence element or a directive. */
final case class MisplacedDocComment(location: SourceLocation) extends PsiError {
  def message: String = "a doc comment that documents nothing"

  override def hint: Option[String] =
    Some("put it before a field, an element or a directive, or make it a plain comment, '/* … */'")
}

/** A `}`, `]` or `)`, `bracket`, that closes nothing, or not the bracket opened last; `fatal` when it stands inside
  * brackets, which ends the source, and not when nothing at all is open, where it is skipped.
  */
final case class UnmatchedBracket(location: SourceLocation, bracket: Char, fatal: Boolean) extends PsiError {
  def message: String =
    Message.quote(bracket.toString) + " closes no bracket open" + (if (fatal) Message.Fatal else "")
}

/** A `{`, `[` or `(`, `bracket`, that the source ends without closing; the error points at the opening. */
final case class UnclosedBracket(location: SourceLocation, bracket: Char) extends PsiError {
  def message: String = Message.quote(bracket.toString) + " is never closed" + Message.Fatal
}

/** A level past the depth limit, `limit` (`format.md` §11), where it was crossed: in a source's text, which ends the
  * source (`fatal`); at a directive's call, whose fragment does not fit where it lands; at a substitution, whose value
  * does not fit where it is grafted.
  */
final case class DepthExceeded(location: SourceLocation, limit: Int, fatal: Boolean) extends PsiError {
  def message: String = "nested deeper than " + limit.toString + " levels" + (if (fatal) Message.Fatal else "")

  override def hint: Option[String] = Some("nest it less, or build the loader with a higher withDepthLimit")
}

/** `\${path}` at `location`, the innermost substitution being resolved when resolution went more than `limit` steps
  * deep, each a level descended or a substitution followed (`format.md` §11).
  */
final case class ResolutionTooDeep(location: SourceLocation, path: String, limit: Int) extends PsiError {
  def message: String =
    "resolving '$" + "{" + Message.cut(path) + "}' goes more than " + limit.toString +
      " steps deep, levels and substitutions together"

  override def hint: Option[String] =
    Some("shorten the chain of substitutions, or build the loader with a higher withDepthLimit")
}

/** A graft, of `\${path}` at `location`, that would take the nodes grafts copy into the tree past `limit` (`format.md`
  * §11): the first thing resolution would build past either of its size limits.
  */
final case class ResolutionTooLarge(location: SourceLocation, path: String, limit: Int) extends PsiError {
  def message: String =
    "grafting '$" + "{" + Message.cut(path) + "}' copies more than " + limit.toString +
      " nodes into the tree, substitutions together"

  override def hint: Option[String] =
    Some("graft large values fewer times, or build the loader with a higher withSizeLimit")
}

/** A concatenation, written at `location`, that would take the characters of the scalars concatenations build past
  * `limit` (`format.md` §11): the first thing resolution would build past either of its size limits.
  */
final case class ConcatenationTooLong(location: SourceLocation, limit: Int) extends PsiError {
  def message: String =
    "the concatenation takes the text concatenations build past " + limit.toString + " characters"

  override def hint: Option[String] =
    Some("concatenate long values fewer times, or build the loader with a higher withSizeLimit")
}

/** A `:` merge of a map or a substitution, written at `location`, with an earlier value of its key where either is a
  * whole-value substitution, which may turn out a map or not (`format.md` §7); `:=` replaces instead.
  *
  * @param earlier
  *   where the earlier value was defined
  */
final case class MergeWithSubstitution(location: PsiLocation, earlier: PsiLocation) extends PsiError {
  def message: String =
    "':' merges with the value at " + earlier.description +
      ", and one of them is a substitution, which may not be a map"

  override def hint: Option[String] = Some("write ':=' to replace the earlier value")
}

/** `\${path}`, alone or in a concatenation, at `location`, the `\${`, with nothing at `path` in the tree (`format.md`
  * §9).
  */
final case class UnresolvedSubstitution(location: SourceLocation, path: String) extends PsiError {
  def message: String = "nothing at " + Message.quote(path) + " to substitute"

  override def hint: Option[String] = Some("write '${?" + Message.cut(path) + "}' if it may be missing")
}

/** `\${path}` at `location` that leads back to itself, directly or through other substitutions (§9). Reported once, at
  * the reference that closes the cycle.
  */
final case class SubstitutionCycle(location: SourceLocation, path: String) extends PsiError {
  def message: String = "'${" + Message.cut(path) + "}' leads back to itself"
}

/** `\${path}` at `location`, a part of a concatenation, referring to a node that is not a scalar: a map, a sequence or
  * `null`, defined at `origin` (§9).
  */
final case class NonScalarInConcatenation(location: SourceLocation, path: String, origin: PsiLocation)
    extends PsiError {
  def message: String =
    "'${" + Message.cut(path) + "}' in a string is not a scalar; it is defined at " + origin.description
}

/** Among the fields of a block, an unquoted scalar holding a substitution and more than it: an injection stands alone
  * there (`format.md` §9).
  */
final case class MisplacedInjection(location: SourceLocation) extends PsiError {
  def message: String = "an injection stands alone among the fields of a block"

  override def hint: Option[String] = Some("write a key and ':' before a value")
}

/** A substitution standing alone among the fields of a block, `\$path` with no `*`: a graft with no key to stand at
  * (`format.md` §9).
  */
final case class SubstitutionAmongFields(location: SourceLocation, path: String, optional: Boolean) extends PsiError {
  private def written(star: String): String = "'${" + (if (optional) "?" else "") + Message.cut(path) + star + "}'"

  def message: String = written("") + " among the fields of a block has no key to stand at"

  override def hint: Option[String] =
    Some("write " + written("*") + " to inject the fields of the map there, or a key and ':' before it")
}

/** An injection, a substitution or a call with a `*` after it, anywhere but alone among the fields of a block or the
  * elements of a sequence: as a field's value, in a string or a concatenation, or a call among fields (`format.md` §6,
  * §9).
  */
final case class InjectionOutOfPlace(location: SourceLocation) extends PsiError {
  def message: String = "an injection stands alone among the fields of a block or the elements of a sequence"

  override def hint: Option[String] = Some("remove the '*' to put the node itself here, or quote a '*' that is text")
}

/** A tag before an injection among the elements of a sequence, whose elements keep their own tags, so that the tag has
  * no node to go to (`format.md` §9).
  */
final case class TaggedInjection(location: SourceLocation) extends PsiError {
  def message: String = "a tag before an injection, whose elements keep their own tags"

  override def hint: Option[String] = Some("remove the tag, or the '*' to put the node itself as one element")
}

/** An injection among the elements of a sequence, at `location`, of `\${path}`, which is not a sequence; `found` is
  * where its node is defined (`format.md` §9).
  */
final case class InjectionNotSequence(location: SourceLocation, path: String, found: PsiLocation) extends PsiError {
  def message: String =
    "'${" + Message.cut(path) + "}' injected among elements is not a sequence; it is defined at " + found.description

  override def hint: Option[String] = Some("remove the '*' to put it as one element")
}

/** A call injected among the elements of a sequence, `#name(…)*`, whose handler gave something other than a sequence
  * (`format.md` §6, §9).
  */
final case class DirectiveNotSequence(location: SourceLocation, name: String) extends PsiError {
  def message: String = Message.directive(name) + " injected among elements gave something other than a sequence"

  override def hint: Option[String] = Some("remove the '*' to put what it gives as one element")
}

/** An injection among the fields of the root, which every path goes through, so that it would refer to the tree it
  * builds (`format.md` §9); a source included in a block puts its injections in the block.
  */
final case class InjectionAtRoot(location: SourceLocation) extends PsiError {
  def message: String = "an injection at the root, which every path goes through, refers to the tree it builds"

  override def hint: Option[String] = Some("inject into a block, or include the source at the root with #include")
}

/** An injection among the fields of a block, at `location`, of `\${path}`, which is not a map; `found` is where its
  * node is defined (`format.md` §9).
  */
final case class InjectionNotMap(location: SourceLocation, path: String, found: PsiLocation) extends PsiError {
  def message: String =
    "'${" + Message.cut(path) + "}' injected among fields is not a map; it is defined at " + found.description
}

/** A loader's marker that failed on a block's value, with its handler's error (`format.md` §5); the block stands for
  * its value with none of the loader's markers.
  */
final case class MarkerFailed(location: SourceLocation, name: String, error: MarkerError) extends PsiError {
  def message: String = "marker " + Message.quote(name) + ": " + error.message

  override def hint: Option[String] = error.hint
}

/** A built-in marker after one of the loader's: the built-in ones shape the lines, before the loader's run over the
  * value (§5); the block is read as with no marker.
  */
final case class BuiltInMarkerAfterLoaders(location: SourceLocation, name: String) extends PsiError {
  def message: String =
    "built-in marker " + Message.quote(name) + " after one of the loader's; the block is read with no marker"

  override def hint: Option[String] = Some("put the built-in markers first")
}

/** A call of a directive the loader has no handler for (`format.md` §6). */
final case class UnknownDirective(location: SourceLocation, name: String) extends PsiError {
  def message: String = "unknown directive " + Message.directive(name)
}

/** A call of a directive the loader was built with disabled (§6). */
final case class DisabledDirective(location: SourceLocation, name: String) extends PsiError {
  def message: String = "directive " + Message.directive(name) + " is disabled in this loader"
}

/** A call whose handler failed, with the handler's error. */
final case class DirectiveFailed(location: SourceLocation, name: String, error: DirectiveError) extends PsiError {
  def message: String = Message.directive(name) + ": " + error.message

  override def hint: Option[String] = error.hint
}

/** A call in statement position whose handler gave something other than a map. */
final case class DirectiveNotMap(location: SourceLocation, name: String) extends PsiError {
  def message: String = Message.directive(name) + " as a statement gave something other than a map"

  override def hint: Option[String] = Some("make it the value of a field")
}

/** A call that parses a source already being expanded (§6).
  *
  * @param chain
  *   the sources from the one loaded to the one parsed again, each as locations name it
  */
final case class DirectiveCycle(location: SourceLocation, chain: ::[String]) extends PsiError {
  def message: String = "a cycle of sources: " + chain.mkString(" -> ")
}

/** A call that parses a source `limit` sources deep: a backstop for a cycle no key catches (§6). */
final case class DirectiveTooDeep(location: SourceLocation, limit: Int) extends PsiError {
  def message: String = "sources nested " + limit.toString + " deep, a cycle that changes its source on every step"
}
