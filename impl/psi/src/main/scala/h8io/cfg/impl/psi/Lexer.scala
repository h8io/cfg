package h8io.cfg.impl.psi

import h8io.cfg.Id
import h8io.cfg.impl.psi.Lexer.*
import h8io.cfg.impl.psi.Token.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Reads the tokens of one source, in the mode the parser asks for (`format.md` §3).
  *
  * Errors are collected rather than thrown. A fatal one — an unterminated quote or comment — leaves nothing to
  * resynchronise on, so the lexer reports it and answers [[Token.End]] from then on (§11).
  */
private[psi] final class Lexer(source: Source) {
  private val text = source.text
  private var offset = 0
  private var atLineStart = true
  private var ended = false
  private val reported = mutable.ListBuffer.empty[PsiError]

  /** The errors found so far, in order. */
  def errors: List[PsiError] = reported.toList

  /** How many errors have been found so far. */
  def errorCount: Int = reported.length

  /** Whether a fatal error has ended the source, so that its [[Token.End]] is no real end. */
  def halted: Boolean = ended

  def next(mode: Mode): Token = {
    val before = offset
    skipBlanks()
    val start = offset
    val startsLine = atLineStart
    val kind = if (ended || offset >= text.length) End else read(mode)
    atLineStart = kind == Newline
    Token(kind, source.location(start), startsLine, spaced = start > before)
  }

  private def report(error: SourceLocation => PsiError, at: Int): Unit = {
    reported += error(source.location(at))
    ()
  }

  /** Reports a fatal error: nothing after it can be trusted, so the source ends here. */
  private def stop(error: SourceLocation => PsiError, at: Int): Unit = {
    report(error, at)
    ended = true
  }

  private def fail(error: SourceLocation => PsiError, at: Int): Kind = {
    stop(error, at)
    End
  }

  @inline private def has(i: Int): Boolean = i < text.length
  @inline private def at(i: Int, c: Char): Boolean = has(i) && text.charAt(i) == c
  @inline private def startsWith(s: String, i: Int = offset): Boolean = text.startsWith(s, i)

  /** LF or CRLF. */
  private def isNewline(i: Int): Boolean = at(i, '\n') || at(i, '\r') && at(i + 1, '\n')

  /** A CR that is not part of a CRLF: an error, read as whitespace once reported. */
  private def isLoneCarriageReturn(i: Int): Boolean = at(i, '\r') && !at(i + 1, '\n')

  /** NEL, U+2028 and U+2029, which some tools show as a line break and others do not (`format.md` §3). An error
    * anywhere, as a lone CR is.
    */
  private def isForeignLineBreak(c: Char): Boolean = c == '\u0085' || c == '\u2028' || c == '\u2029'

  /** A character that is an error anywhere, quotes and comments included (`format.md` §3): a lone CR, NEL, U+2028 or
    * U+2029, which tools disagree on whether it ends a line; a control character other than tab; or an invisible
    * character, which makes the source read otherwise than it parses.
    */
  private def isForbidden(i: Int): Boolean =
    isLoneCarriageReturn(i) || has(i) && {
      val c = text.charAt(i)
      isForeignLineBreak(c) || isControl(c) || isInvisible(c)
    }

  /** Whitespace other than space and the line breaks — NBSP and the other Unicode spaces (`format.md` §3). An error
    * outside quotes and comments, read as whitespace once reported; content inside them. All of them are single UTF-16
    * units. Those that are control characters, such as FF and VT, are forbidden.
    */
  private def isForeignSpace(c: Char): Boolean =
    c != ' ' && !isForeignLineBreak(c) &&
      Character.isSpaceChar(c)

  /** Ends a token: whitespace, a newline, or a character that is read as whitespace once reported. */
  private def isBreak(i: Int): Boolean = {
    val c = text.charAt(i)
    isSpace(c) || isNewline(i) || isForbidden(i) || isForeignSpace(c)
  }

  /** A forbidden character is reported where it stands and then treated as whitespace — dropped, inside quotes and
    * comments.
    */
  private def skipForbidden(): Boolean =
    if (isLoneCarriageReturn(offset)) {
      report(LoneCarriageReturn.apply, offset)
      offset += 1
      true
    } else if (has(offset) && isForeignLineBreak(text.charAt(offset))) {
      report(ForeignLineBreak(_, text.charAt(offset)), offset)
      offset += 1
      true
    } else if (has(offset) && isControl(text.charAt(offset))) {
      report(ControlCharacter(_, text.charAt(offset)), offset)
      offset += 1
      true
    } else if (has(offset) && isInvisible(text.charAt(offset))) {
      report(InvisibleCharacter(_, text.charAt(offset)), offset)
      offset += 1
      true
    } else false

  /** Skips spaces, tabs, comments and stray comment ends — everything that is whitespace to the grammar — and the
    * characters read as whitespace once reported. Newlines are tokens.
    *
    * At the start of a line what it skips is the line's indentation, comments included, and the first tab in it is
    * reported, in its place among the errors, if a token follows: a blank or comment-only line has no indentation
    * (`format.md` §3). A line is what newline tokens delimit, so one that a block comment carries on past is still the
    * same line.
    */
  private def skipBlanks(): Unit = {
    var tab = -1
    var tabIndex = 0
    var blank = true
    while (blank && !ended && has(offset))
      if (isSpace(text.charAt(offset))) {
        if (atLineStart && tab < 0 && text.charAt(offset) == '\t') {
          tab = offset
          tabIndex = reported.length
        }
        offset += 1
      } else if (skipForbidden()) ()
      else if (isForeignSpace(text.charAt(offset))) {
        report(ForeignWhitespace(_, text.charAt(offset)), offset)
        offset += 1
      } else if (startsWith("//")) skipLineComment()
      else if (startsWith("/*") && !isDocComment) skipBlockComment().fold(())(_ => ())
      else if (startsWith("*/")) {
        report(StrayCommentEnd.apply, offset)
        offset += 2
      } else blank = false
    if (tab >= 0 && !ended && has(offset) && !isNewline(offset))
      reported.insert(tabIndex, TabInIndentation(source.location(tab)))
  }

  /** A doc comment opens here: `/` and two `*`, but not followed by `/`, which makes an empty ordinary comment. */
  private def isDocComment: Boolean = startsWith("/**") && !startsWith("/**/")

  private def skipLineComment(): Unit =
    while (has(offset) && !isNewline(offset))
      if (!skipForbidden()) offset += 1

  /** Skips a nesting block comment from its opening, and gives its text; an unterminated one is fatal. */
  private def skipBlockComment(): Option[String] = {
    val start = offset
    offset += 2
    var depth = 1
    while (depth > 0 && has(offset))
      if (startsWith("/*")) {
        depth += 1
        offset += 2
      } else if (startsWith("*/")) {
        depth -= 1
        offset += 2
      } else if (!skipForbidden()) offset += 1
    if (depth > 0) {
      stop(UnterminatedComment.apply, start)
      None
    } else Some(text.substring(start + 2, offset - 2))
  }

  private def read(mode: Mode): Kind = {
    val start = offset
    text.charAt(offset) match {
      case '\n' => advance(1, Newline)
      case '\r' => advance(2, Newline) // a CRLF: a lone CR was skipped as whitespace
      case '{' => advance(1, LeftBrace)
      case '}' => advance(1, RightBrace)
      case '[' => advance(1, LeftBracket)
      case ']' => advance(1, RightBracket)
      case ',' => advance(1, Comma)
      case '/' if isDocComment => skipBlockComment().fold[Kind](End)(comment => DocComment(comment.substring(1)))
      case '"' if startsWith("\"\"\"") => triple(start, '"')
      case '"' => quoted()
      case '\'' if startsWith("'''") => triple(start, '\'')
      case '\'' => raw()
      case '`' => backticked(start).fold[Kind](End)(Name(_, quoted = true))
      case '#' =>
        offset += 1
        name(MissingDirectiveName.apply, start).fold[Kind](End)(Directive.apply)
      case '@' => tag(start)
      case c =>
        mode match {
          case KeyMode => keyMode(c, start)
          case ValueMode => unquoted()
        }
    }
  }

  private def advance(by: Int, kind: Kind): Kind = {
    offset += by
    kind
  }

  private def keyMode(c: Char, start: Int): Kind =
    c match {
      case '(' => advance(1, LeftParen)
      case ')' => advance(1, RightParen)
      case '.' => advance(1, Dot)
      case '=' => advance(1, Equals)
      case ':' =>
        val kind = if (at(offset + 1, '=')) advance(2, Replace) else advance(1, Colon)
        if (has(offset) && !isBreak(offset)) report(ColonWithoutSpace.apply, start)
        kind
      case _ =>
        while (has(offset) && !endsKey(offset)) offset += 1
        val key = text.substring(start, offset)
        if (!Id.SafeKeyPattern.matcher(key).matches()) report(InvalidKey(_, key), start)
        Name(key, quoted = false)
    }

  private def endsKey(i: Int): Boolean =
    isBreak(i) ||
      (text.charAt(i) match {
        case '.' | ':' | '=' | '{' | '}' | '[' | ']' | '(' | ')' | ',' | '"' | '`' => true
        case _ => startsWith("//", i) || startsWith("/*", i)
      })

  /** `@name` from its `@`. */
  private def tag(start: Int): Kind = {
    offset += 1
    if (at(offset, '-')) {
      report(ReservedTagRemoval.apply, start)
      offset += 1
    }
    val tag = name(MissingTagName.apply, start)
    if (at(offset, '(')) report(TagArguments.apply, start)
    tag.fold[Kind](End)(Tag.apply)
  }

  /** A tag or directive name after its prefix: bare, or in backticks. */
  private def name(missing: SourceLocation => PsiError, prefix: Int): Option[String] =
    if (at(offset, '`')) backticked(offset)
    else {
      val start = offset
      if (has(offset) && isNameStart(text.codePointAt(offset))) skipNameChars()
      if (offset == start) report(missing, prefix)
      Some(text.substring(start, offset))
    }

  private def skipNameChars(): Unit =
    while (has(offset) && isNamePart(text.codePointAt(offset))) offset += Character.charCount(text.codePointAt(offset))

  /** A name in backticks from its opening one; `None` once an unterminated one has ended the source. */
  private def backticked(start: Int): Option[String] = {
    offset = start + 1
    val value = new StringBuilder
    while (has(offset) && !at(offset, '`') && !isNewline(offset))
      if (at(offset, '\\')) escape(value)
      else if (!skipForbidden()) {
        value += text.charAt(offset)
        offset += 1
      }
    if (at(offset, '`')) {
      offset += 1
      Some(value.result())
    } else {
      stop(UnterminatedName.apply, start)
      None
    }
  }

  /** Reads an escape from its backslash into `value`; one outside the set is reported and dropped. */
  private def escape(value: StringBuilder): Unit = {
    val start = offset
    val (read, end) = readEscape(text, start, has(start + 1) && !isNewline(start + 1))
    read match {
      case Some(escaped) => value ++= escaped
      case None => report(InvalidEscape(_, text.substring(start, end)), start)
    }
    offset = end
  }

  /** A scalar in `"…"`, with its escapes and substitutions. */
  private def quoted(): Kind = {
    val start = offset
    offset += 1
    val parts = new Parts
    while (!ended && has(offset) && !at(offset, '"') && !isNewline(offset))
      if (startsWith("${")) reference(quoted = true).foreach(parts.reference)
      else if (at(offset, '\\')) escape(parts.buffer)
      else if (!skipForbidden()) {
        parts.buffer += text.charAt(offset)
        offset += 1
      }
    if (ended) End
    else if (at(offset, '"')) {
      offset += 1
      Scalar(parts.result(), quoted = true)
    } else fail(UnterminatedString.apply, start)
  }

  /** An unquoted scalar: everything up to whitespace, `,`, `}` or `]`, comment openers and backslashes included. */
  private def unquoted(): Kind = {
    val parts = new Parts
    while (!ended && has(offset) && !endsUnquoted(offset))
      if (startsWith("${")) reference(quoted = false).foreach(parts.reference)
      else {
        val c = text.charAt(offset)
        if (c == '"' || c == '`') report(QuoteInUnquotedScalar.apply, offset)
        parts.buffer += c
        offset += 1
      }
    if (ended) End else Scalar(parts.result(), quoted = false)
  }

  private def endsUnquoted(i: Int): Boolean = isBreak(i) || at(i, ',') || at(i, '}') || at(i, ']')

  /** A raw string in `'…'`, Perl's single quotes: `\'` and `\\` are its only escapes, and `${` is text. */
  private def raw(): Kind = {
    val start = offset
    offset += 1
    val value = new StringBuilder
    while (has(offset) && !at(offset, '\'') && !isNewline(offset)) {
      val escaped = rawEscape()
      escaped.foreach(value += _)
      if (escaped.isEmpty && !skipForbidden()) {
        value += text.charAt(offset)
        offset += 1
      }
    }
    if (at(offset, '\'')) {
      offset += 1
      Scalar(if (value.isEmpty) Vector.empty else Vector(Text(value.result())), quoted = true)
    } else fail(UnterminatedString.apply, start)
  }

  /** `\'` or `\\` here, read; any other backslash is an ordinary character in a raw string. */
  private def rawEscape(): Option[Char] =
    if (at(offset, '\\') && (at(offset + 1, '\'') || at(offset + 1, '\\'))) {
      offset += 2
      Some(text.charAt(offset - 1))
    } else None

  /** A triple-quoted string from its opening delimiter, `"""` or `'''` (`format.md` §5): always a block, and what
    * stands between the delimiter and the end of its line are its markers, read by [[MarkerParser]].
    */
  private def triple(start: Int, quote: Char): Kind = {
    offset = start + 3
    val stages = markers()
    if (has(offset)) {
      offset += (if (at(offset, '\r')) 2 else 1)
      content(start, quote).fold[Kind](End)(lines => scalar(arrange(lines, stages, start)))
    } else fail(UnterminatedString.apply, start)
  }

  /** The layout stages the markers on the rest of this line pick; a forbidden character there is reported and left out.
    */
  private def markers(): Vector[Stage] = {
    val line = new StringBuilder
    val offsets = Vector.newBuilder[Int]
    while (has(offset) && !isNewline(offset))
      if (!skipForbidden()) {
        line += text.charAt(offset)
        offsets += offset
        offset += 1
      }
    offsets += offset
    new MarkerParser(line.result(), offsets.result(), (error, at) => report(error, at)).stages()
  }

  /** The content of a triple-quoted string up to its closing delimiter, split into lines, escapes and substitutions
    * read but no layout applied; `None` once the source has ended.
    */
  private def content(start: Int, quote: Char): Option[Vector[Line]] = {
    val lines = Vector.newBuilder[Line]
    var line = Vector.newBuilder[Piece]
    var lineStart = offset
    var closed = false
    while (!closed && !ended && has(offset)) {
      var run = 0
      while (at(offset + run, quote)) run += 1
      if (run >= 3) {
        // a run of more than three quotes closes at its last three
        for (_ <- 3 until run) line += Written(quote)
        offset += run
        closed = true
      } else if (run > 0) {
        for (_ <- 0 until run) line += Written(quote)
        offset += run
      } else if (isNewline(offset)) {
        lines += Line(lineStart, line.result())
        line = Vector.newBuilder[Piece]
        offset += (if (at(offset, '\r')) 2 else 1)
        lineStart = offset
      } else if (quote == '\'') {
        val escaped = rawEscape()
        escaped.foreach(c => line += Escaped(c.toString))
        if (escaped.isEmpty) written(line)
      } else if (at(offset, '\\')) {
        val value = new StringBuilder
        escape(value)
        if (value.nonEmpty) line += Escaped(value.result())
      } else if (startsWith("${")) reference(quoted = true).foreach(line += Substituted(_))
      else written(line)
    }
    if (closed) Some(lines.result() :+ Line(lineStart, line.result()))
    else {
      if (!ended) stop(UnterminatedString.apply, start)
      None
    }
  }

  /** A character as written, here; a forbidden one is reported and dropped instead. */
  private def written(line: mutable.Builder[Piece, Vector[Piece]]): Unit =
    if (!skipForbidden()) {
      line += Written(text.charAt(offset))
      offset += 1
    }

  /** Applies the layout stages of a block (`format.md` §5) to its lines, in order, the last line being the one of the
    * closing delimiter, which is dropped with the newline before it when it holds nothing but whitespace. Trailing
    * whitespace goes first.
    */
  private def arrange(lines: Vector[Line], stages: Vector[Stage], start: Int): Vector[Piece] = {
    val last = lines.last
    val closing = if (last.pieces.forall(isBlank)) Vector(last) else Vector.empty
    val body = (if (closing.isEmpty) lines else lines.init).map(_.trimmed)
    join(stages.foldLeft(body)((body, stage) => layout(stage, body, closing, start)))
  }

  private def layout(stage: Stage, body: Vector[Line], closing: Vector[Line], start: Int): Vector[Line] =
    stage match {
      case CommonIndent =>
        val indent = commonIndent(body.filter(_.pieces.nonEmpty) ++ closing)
        body.map(_.drop(indent))
      case StripIndent => body.map(line => line.drop(line.indent.length))
      case KeepIndent => body
      case WidthIndent(width) =>
        val opening = indentOf(start)
        body.map(line =>
          if (line.pieces.isEmpty) line else line.drop(widthIndent(line, opening, required(opening, width))))
      case Fold => fold(body)
    }

  /** The indent an explicit width asks for: the opening line's and `width` more. A width can be as large as an `Int`,
    * so the sum is capped there rather than overflowing to a negative indent; no line is that long, so every line is
    * short of it, as when the opening line has no indent.
    */
  private def required(opening: String, width: Int): Int = math.min(opening.length.toLong + width, Int.MaxValue).toInt

  /** The indent of the line holding `offset`. */
  private def indentOf(offset: Int): String = {
    var i = text.lastIndexOf('\n', offset - 1) + 1
    val from = i
    while (isSpace(text.charAt(i))) i += 1
    text.substring(from, i)
  }

  /** The longest indent `lines` share; any two of which neither is a prefix of the other are reported, at the first
    * character that differs. `lines` is never empty: it holds the closing delimiter's line, or a last line that is not
    * blank.
    */
  private def commonIndent(lines: Vector[Line]): Int = {
    var longest = ""
    lines.foreach { line =>
      val indent = line.indent
      if (!longest.startsWith(indent))
        if (indent.startsWith(longest)) longest = indent
        else report(InconsistentIndent.apply, line.start + sharedPrefix(longest, indent))
    }
    lines.map(_.indent).reduce((a, b) => a.substring(0, sharedPrefix(a, b))).length
  }

  private def sharedPrefix(a: String, b: String): Int = {
    var i = 0
    while (i < a.length && i < b.length && a.charAt(i) == b.charAt(i)) i += 1
    i
  }

  /** How much of `line`'s indent to strip under an explicit width: `required` characters, the first of them `opening`;
    * a line that falls short is reported where it does, and loses what indent it has up to that.
    */
  private def widthIndent(line: Line, opening: String, required: Int): Int = {
    val indent = line.indent
    if (indent.startsWith(opening) && indent.length >= required) required
    else {
      val short = if (indent.startsWith(opening)) indent.length else sharedPrefix(opening, indent)
      report(InsufficientIndent.apply, line.start + short)
      math.min(indent.length, required)
    }
  }

  /** Folds lines as YAML does: two lines with no indent and nothing between them become one, joined by a space, and of
    * a run of empty lines between two such lines one is dropped; a line with an indent is not folded, nor are the empty
    * lines around it.
    */
  private def fold(lines: Vector[Line]): Vector[Line] = {
    def isText(line: Line): Boolean = !isBlank(line.pieces.head) // called on non-empty lines only
    val folded = mutable.ArrayBuffer.empty[Line]
    val empty = mutable.ArrayBuffer.empty[Line]
    var afterText = false
    lines.foreach { line =>
      if (line.pieces.isEmpty) {
        empty += line
        ()
      } else {
        val text = isText(line)
        if (afterText && text && empty.isEmpty)
          folded(folded.length - 1) = folded.last.copy(pieces = (folded.last.pieces :+ Escaped(" ")) ++ line.pieces)
        else {
          folded ++= (if (afterText && text) empty.drop(1) else empty)
          folded += line
        }
        empty.clear()
        afterText = text
      }
    }
    (folded ++ empty).toVector
  }

  /** The pieces of `lines` joined with newlines. */
  private def join(lines: Vector[Line]): Vector[Piece] =
    lines.map(_.pieces).reduceOption((a, b) => (a :+ Escaped("\n")) ++ b).getOrElse(Vector.empty)

  /** A quoted scalar of `pieces`. */
  private def scalar(pieces: Vector[Piece]): Kind = {
    val parts = new Parts
    pieces.foreach {
      case Written(c) =>
        parts.buffer += c
        ()
      case Escaped(value) =>
        parts.buffer ++= value
        ()
      case Substituted(reference) => parts.reference(reference)
    }
    Scalar(parts.result(), quoted = true)
  }

  /** `${path}` or `${?path}` from its `$`, its path written as `Id.path` renders one (`format.md` §9). A path that
    * cannot be read is reported — at the space or tab, if that is what broke it, at the `${` otherwise — and skipped up
    * to the next `}` on its line, or with none up to where the scalar around it ends.
    */
  private def reference(quoted: Boolean): Option[Reference] = {
    val start = offset
    offset += 2
    val optional = at(offset, '?')
    if (optional) offset += 1
    path(Id.Root).flatMap(path => if (at(offset, '}')) Some(path) else None) match {
      case Some(path) =>
        offset += 1
        Some(Reference(path, optional, source.location(start)))
      case None if !ended =>
        if (has(offset) && isSpace(text.charAt(offset)))
          report(WhitespaceInSubstitution.apply, offset)
        else report(InvalidSubstitution.apply, start)
        val close = closingBrace(quoted)
        if (close >= 0) offset = close + 1
        else while (has(offset) && !endsAfterReference(offset, quoted)) offset += 1
        None
      case None => None
    }
  }

  /** The `}` that closes a broken substitution: the next one on its line, and inside quotes before the closing one. A
    * forbidden character stops the search too, so that it is reported rather than skipped.
    */
  private def closingBrace(quoted: Boolean): Int = {
    var i = offset
    while (has(i) && !at(i, '}') && !isNewline(i) && !isForbidden(i) && !(quoted && at(i, '"'))) i += 1
    if (at(i, '}')) i else -1
  }

  /** With no `}` to close it, a broken substitution runs to where the scalar around it would end — or, inside quotes,
    * to a newline or a forbidden character, so that one is reported rather than skipped.
    */
  private def endsAfterReference(i: Int, quoted: Boolean): Boolean =
    if (quoted) at(i, '"') || isNewline(i) || isForbidden(i) else endsUnquoted(i)

  /** The segments of a path below `parent`: a key first, then `.key` or `[index]` any number of times. */
  private def path(parent: Id): Option[Id.Branch] = key(parent).flatMap(segments)

  @tailrec private def segments(current: Id.Branch): Option[Id.Branch] =
    if (at(offset, '.')) {
      offset += 1
      key(current) match {
        case Some(next) => segments(next)
        case None => None
      }
    } else if (at(offset, '[')) {
      offset += 1
      index(current) match {
        case Some(next) => segments(next)
        case None => None
      }
    } else Some(current)

  private def key(parent: Id): Option[Id.Key] =
    if (at(offset, '`')) backticked(offset).map(Id.Key(_, parent))
    else if (has(offset) && isNameStart(text.codePointAt(offset))) {
      val start = offset
      skipNameChars()
      Some(Id.Key(text.substring(start, offset), parent))
    } else None

  /** An index after its `[`: a decimal with no sign and no leading zeros, as `Id.path` prints one. */
  private def index(parent: Id): Option[Id.Index] = {
    val start = offset
    while (has(offset) && text.charAt(offset) >= '0' && text.charAt(offset) <= '9') offset += 1
    val digits = text.substring(start, offset)
    if (digits.isEmpty || digits.length > 1 && digits.charAt(0) == '0' || !at(offset, ']')) None
    else if (digits.length > 10 || digits.toLong > Int.MaxValue) None
    else {
      offset += 1
      Some(Id.Index(digits.toInt, parent))
    }
  }

  /** The parts of a scalar being read: text accumulates until a substitution closes it. */
  private final class Parts {
    val buffer = new StringBuilder
    private val parts = Vector.newBuilder[Part]

    def reference(part: Reference): Unit = {
      flush()
      parts += part
      ()
    }

    def result(): Vector[Part] = {
      flush()
      parts.result()
    }

    private def flush(): Unit =
      if (buffer.nonEmpty) {
        parts += Text(buffer.result())
        buffer.clear()
      }
  }
}

private object Lexer {

  /** Whitespace inside a line: space and tab, nothing else (`format.md` §3). */
  def isSpace(c: Char): Boolean = c == ' ' || c == '\t'

  /** A control character other than tab, LF and CR (`format.md` §3), C1 and DEL included; NEL is one, with an error of
    * its own. LF and CR are line breaks, a lone CR being forbidden too.
    */
  def isControl(c: Char): Boolean = Character.getType(c) == Character.CONTROL && c != '\t' && c != '\n' && c != '\r'

  /** A character that makes source read otherwise than it parses (`format.md` §3): the bidirectional embeddings,
    * overrides and isolates, U+202A–U+202E and U+2066–U+2069; the zero-width space U+200B; the word joiner and the
    * invisible operators, U+2060–U+2064; and U+FEFF past the start of a source.
    */
  def isInvisible(c: Char): Boolean = {
    val code = c.toInt
    code >= 0x202a && code <= 0x202e || code >= 0x2066 && code <= 0x2069 || code == 0x200b ||
    code >= 0x2060 && code <= 0x2064 || code == 0xfeff
  }

  def isNameStart(cp: Int): Boolean = Character.isLetter(cp) || cp == '_'

  def isNamePart(cp: Int): Boolean =
    isNameStart(cp) || cp == '-' || {
      val kind = Character.getType(cp)
      kind == Character.DECIMAL_DIGIT_NUMBER || kind == Character.LETTER_NUMBER || kind == Character.OTHER_NUMBER
    }

  /** The escape whose backslash stands at `i` of `s` (`format.md` §3): what it gives, `None` for one outside the set,
    * and where it ends. `next` says whether a character the escape can take follows the backslash.
    */
  def readEscape(s: String, i: Int, next: Boolean): (Option[String], Int) =
    if (!next) (None, i + 1)
    else
      s.charAt(i + 1) match {
        case c @ ('"' | '`' | '\\' | '$') => (Some(c.toString), i + 2)
        case 'b' => (Some("\b"), i + 2)
        case 'f' => (Some("\f"), i + 2)
        case 'n' => (Some("\n"), i + 2)
        case 'r' => (Some("\r"), i + 2)
        case 't' => (Some("\t"), i + 2)
        case 'u' if i + 5 < s.length && s.substring(i + 2, i + 6).forall(isHexDigit) =>
          (Some(Integer.parseInt(s.substring(i + 2, i + 6), 16).toChar.toString), i + 6)
        case _ => (None, i + 2)
      }

  private def isHexDigit(c: Char): Boolean = Character.digit(c, 16) >= 0

  /** What a line of a triple-quoted string holds before its layout is applied (`format.md` §5): a character as written,
    * what an escape produced — never whitespace to the layout, which comes first — or a substitution.
    */
  sealed trait Piece
  final case class Written(c: Char) extends Piece
  final case class Escaped(value: String) extends Piece
  final case class Substituted(reference: Reference) extends Piece

  def isBlank(piece: Piece): Boolean =
    piece match {
      case Written(c) => isSpace(c)
      case _ => false
    }

  /** A line of a triple-quoted string from `start`, its newline left out. */
  final case class Line(start: Int, pieces: Vector[Piece]) {
    def indent: String = pieces.iterator.takeWhile(isBlank).collect { case Written(c) => c }.mkString

    def drop(n: Int): Line = copy(pieces = pieces.drop(n))

    /** Without trailing whitespace; a line of nothing else becomes empty. */
    def trimmed: Line = copy(pieces = pieces.reverseIterator.dropWhile(isBlank).toVector.reverse)
  }

  /** A step of a block's layout, picked by a marker (`format.md` §5). */
  sealed trait Stage

  /** A stage that sets the indent; one per block. */
  sealed trait Indent extends Stage
  case object CommonIndent extends Indent
  case object StripIndent extends Indent
  case object KeepIndent extends Indent
  final case class WidthIndent(width: Int) extends Indent

  case object Fold extends Stage
}
