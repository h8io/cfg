package h8io.cfg.impl.defg

import h8io.cfg.Id
import h8io.cfg.impl.defg.Lexer.*
import h8io.cfg.impl.defg.Token.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Reads the tokens of one source, in the mode the parser asks for (`format.md` §3).
  *
  * Errors are collected rather than thrown. A fatal one — an unterminated quote or comment — leaves nothing to
  * resynchronise on, so the lexer reports it and answers [[Token.End]] from then on (§11).
  */
private[defg] final class Lexer(source: Source) {
  private val text = source.text
  private var offset = 0
  private var atLineStart = true
  private var ended = false
  private val reported = mutable.ListBuffer.empty[DefError]

  /** The errors found so far, in order. */
  def errors: List[DefError] = reported.toList

  def next(mode: Mode): Token = {
    val tab = if (atLineStart) indentationTab() else -1
    val before = reported.length
    skipBlanks()
    if (tab >= 0 && holdsToken(tab)) reported.insert(before, TabInIndentation(source.location(tab)))
    val start = offset
    val startsLine = atLineStart
    val kind = if (ended || offset >= text.length) End else read(mode)
    atLineStart = kind == Newline
    Token(kind, source.location(start), startsLine)
  }

  private def report(error: SourceLocation => DefError, at: Int): Unit = {
    reported += error(source.location(at))
    ()
  }

  /** Reports a fatal error: nothing after it can be trusted, so the source ends here. */
  private def stop(error: SourceLocation => DefError, at: Int): Unit = {
    report(error, at)
    ended = true
  }

  private def fail(error: SourceLocation => DefError, at: Int): Kind = {
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

  /** A character that is an error anywhere because tools disagree on whether it ends a line. */
  private def isBadBreak(i: Int): Boolean = isLoneCarriageReturn(i) || has(i) && isForeignLineBreak(text.charAt(i))

  /** Whitespace other than space, tab and the newlines — NBSP, the other Unicode spaces, FF, VT, and a byte order mark
    * past the start (`format.md` §3). An error outside quotes and comments, read as whitespace once reported; content
    * inside them. All of them are single UTF-16 units.
    */
  private def isForeignSpace(c: Char): Boolean =
    c != ' ' && c != '\t' && c != '\n' && c != '\r' && !isForeignLineBreak(c) &&
      (Character.isWhitespace(c) || Character.isSpaceChar(c) || c == '\uFEFF')

  /** Ends a token: whitespace, a newline, or a character that is read as whitespace once reported. */
  private def isBreak(i: Int): Boolean = {
    val c = text.charAt(i)
    isSpace(c) || isNewline(i) || isBadBreak(i) || isForeignSpace(c)
  }

  /** A lone CR, NEL, U+2028 or U+2029 is reported where it stands and then treated as whitespace — dropped, inside
    * quotes and comments.
    */
  private def skipBadBreak(): Boolean =
    if (isLoneCarriageReturn(offset)) {
      report(LoneCarriageReturn.apply, offset)
      offset += 1
      true
    } else if (has(offset) && isForeignLineBreak(text.charAt(offset))) {
      report(ForeignLineBreak(_, text.charAt(offset)), offset)
      offset += 1
      true
    } else false

  /** The first tab in the indentation of the line starting here, or -1: indentation is the whitespace before the first
    * token of a line (`format.md` §3).
    */
  private def indentationTab(): Int = {
    var i = offset
    while (at(i, ' ')) i += 1
    if (at(i, '\t')) i else -1
  }

  /** Whether the line of `tab` holds a token, now that whitespace and comments are skipped: a blank or comment-only
    * line has no indentation, and neither has a line that a block comment carries on past.
    */
  private def holdsToken(tab: Int): Boolean =
    !ended && has(offset) && !isNewline(offset) && text.lastIndexOf('\n', offset - 1) < tab

  /** Skips spaces, tabs, comments and stray comment ends — everything that is whitespace to the grammar — and the
    * characters read as whitespace once reported. Newlines are tokens.
    */
  private def skipBlanks(): Unit = {
    var blank = true
    while (blank && !ended && has(offset))
      if (isSpace(text.charAt(offset))) offset += 1
      else if (skipBadBreak()) ()
      else if (isForeignSpace(text.charAt(offset))) {
        report(ForeignWhitespace(_, text.charAt(offset)), offset)
        offset += 1
      } else if (startsWith("//")) skipLineComment()
      else if (startsWith("/*") && !isDocComment) skipBlockComment().fold(())(_ => ())
      else if (startsWith("*/")) {
        report(StrayCommentEnd.apply, offset)
        offset += 2
      } else blank = false
  }

  /** A doc comment opens here: `/` and two `*`, but not followed by `/`, which makes an empty ordinary comment. */
  private def isDocComment: Boolean = startsWith("/**") && !startsWith("/**/")

  private def skipLineComment(): Unit =
    while (has(offset) && !isNewline(offset))
      if (!skipBadBreak()) offset += 1

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
      } else if (!skipBadBreak()) offset += 1
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
      case c =>
        mode match {
          case KeyMode => keyMode(c, start)
          case ValueMode => valueMode(c, start)
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

  private def valueMode(c: Char, start: Int): Kind =
    if (c == '@') {
      offset += 1
      if (at(offset, '-')) {
        report(ReservedTagRemoval.apply, start)
        offset += 1
      }
      val tag = name(MissingTagName.apply, start)
      if (at(offset, '(')) report(TagArguments.apply, start)
      tag.fold[Kind](End)(Tag.apply)
    } else unquoted()

  /** A tag or directive name after its prefix: bare, or in backticks. */
  private def name(missing: SourceLocation => DefError, prefix: Int): Option[String] =
    if (at(offset, '`')) backticked(offset)
    else {
      val start = offset
      if (has(offset) && isNameStart(text.codePointAt(offset))) skipNameChars()
      if (offset == start) report(missing, prefix)
      Some(text.substring(start, offset))
    }

  private def isNameStart(cp: Int): Boolean = Character.isLetter(cp) || cp == '_'

  private def isNamePart(cp: Int): Boolean =
    isNameStart(cp) || cp == '-' || {
      val kind = Character.getType(cp)
      kind == Character.DECIMAL_DIGIT_NUMBER || kind == Character.LETTER_NUMBER || kind == Character.OTHER_NUMBER
    }

  private def skipNameChars(): Unit =
    while (has(offset) && isNamePart(text.codePointAt(offset))) offset += Character.charCount(text.codePointAt(offset))

  /** A name in backticks from its opening one; `None` once an unterminated one has ended the source. */
  private def backticked(start: Int): Option[String] = {
    offset = start + 1
    val value = new StringBuilder
    while (has(offset) && !at(offset, '`') && !isNewline(offset))
      if (at(offset, '\\')) escape(value)
      else if (!skipBadBreak()) {
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
    offset += 1
    if (has(offset) && !isNewline(offset)) {
      text.charAt(offset) match {
        case c @ ('"' | '`' | '\\' | '$') => value += c
        case 'b' => value += '\b'
        case 'f' => value += '\f'
        case 'n' => value += '\n'
        case 'r' => value += '\r'
        case 't' => value += '\t'
        case 'u' if has(offset + 4) && text.substring(offset + 1, offset + 5).forall(isHexDigit) =>
          value += Integer.parseInt(text.substring(offset + 1, offset + 5), 16).toChar
          offset += 4
        case _ => report(InvalidEscape(_, text.substring(start, offset + 1)), start)
      }
      offset += 1
    } else report(InvalidEscape(_, "\\"), start)
  }

  private def isHexDigit(c: Char): Boolean = Character.digit(c, 16) >= 0

  /** A scalar in `"…"`, with its escapes and substitutions. */
  private def quoted(): Kind = {
    val start = offset
    offset += 1
    val parts = new Parts
    while (!ended && has(offset) && !at(offset, '"') && !isNewline(offset))
      if (startsWith("${")) reference(quoted = true).foreach(parts.reference)
      else if (at(offset, '\\')) escape(parts.buffer)
      else if (!skipBadBreak()) {
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
      if (escaped.isEmpty && !skipBadBreak()) {
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
    * stands between the delimiter and the end of its line is its marker — an unknown one reported, and the block read
    * as with none.
    */
  private def triple(start: Int, quote: Char): Kind = {
    offset = start + 3
    val markerStart = offset
    val written = new StringBuilder
    while (has(offset) && !isNewline(offset))
      if (!skipBadBreak()) {
        written += text.charAt(offset)
        offset += 1
      }
    var length = written.length
    while (length > 0 && isSpace(written.charAt(length - 1))) length -= 1
    val known = layout(written.substring(0, length), markerStart)
    // whitespace after an unknown marker is part of that one mistake
    if (known.isEmpty) report(UnknownMarker(_, written.result()), markerStart)
    else if (length < written.length) report(WhitespaceAfterDelimiter.apply, offset - (written.length - length))
    if (has(offset)) {
      offset += (if (at(offset, '\r')) 2 else 1)
      content(start, quote).fold[Kind](End)(lines =>
        scalar(arrange(lines, known.getOrElse(Layout(CommonIndent, folded = false)), start)))
    } else fail(UnterminatedString.apply, start)
  }

  /** The layout a marker picks, `None` for an unknown one. */
  private def layout(marker: String, at: Int): Option[Layout] = {
    def isWidth(c: Char): Boolean = c >= '1' && c <= '9'
    marker.toList match {
      case Nil => Some(Layout(CommonIndent, folded = false))
      case '<' :: Nil => Some(Layout(StripIndent, folded = false))
      case '=' :: Nil => Some(Layout(KeepIndent, folded = false))
      case '>' :: Nil => Some(Layout(CommonIndent, folded = true))
      case width :: Nil if isWidth(width) => Some(Layout(WidthIndent(width - '0'), folded = false))
      case '>' :: width :: Nil if isWidth(width) => Some(Layout(WidthIndent(width - '0'), folded = true))
      case ('<' | '=') :: width :: Nil if isWidth(width) =>
        report(WidthWithMarker.apply, at + 1)
        layout(marker.substring(0, 1), at)
      case _ => None
    }
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

  /** A character as written, here; a lone CR, NEL, U+2028 or U+2029 is reported and dropped instead. */
  private def written(line: mutable.Builder[Piece, Vector[Piece]]): Unit =
    if (!skipBadBreak()) {
      line += Written(text.charAt(offset))
      offset += 1
    }

  /** Applies the layout of a block (`format.md` §5) to its lines, the last being the one of the closing delimiter,
    * which is dropped with the newline before it when it holds nothing but whitespace.
    */
  private def arrange(lines: Vector[Line], layout: Layout, start: Int): Vector[Piece] = {
    val last = lines.last
    val closingAlone = last.pieces.forall(isBlank)
    val body = (if (closingAlone) lines.init else lines).map(_.trimmed)
    val stripped = layout.indent match {
      case CommonIndent =>
        val indent = commonIndent(body.filter(_.pieces.nonEmpty) ++ (if (closingAlone) Vector(last) else Vector.empty))
        body.map(_.drop(indent))
      case StripIndent => body.map(line => line.drop(line.indent.length))
      case KeepIndent => body
      case WidthIndent(width) =>
        val opening = indentOf(start)
        body.map(line =>
          if (line.pieces.isEmpty) line else line.drop(widthIndent(line, opening, opening.length + width)))
    }
    join(stripped.map(_.pieces), layout.folded)
  }

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

  /** Joins lines with newlines, or folds them as in YAML: a newline between two lines left with no indent becomes a
    * space, and of a run of empty lines between them the first newline is dropped; a line indented deeper is not
    * folded, nor are the newlines around it.
    */
  private def join(lines: Vector[Vector[Piece]], folded: Boolean): Vector[Piece] = {
    def isText(line: Vector[Piece]): Boolean = !isBlank(line.head) // called on non-empty lines only
    def newlines(n: Int): Vector[Piece] = Vector.fill(n)(Escaped("\n"))
    val filled = lines.indices.filter(lines(_).nonEmpty)
    if (filled.isEmpty) newlines(math.max(lines.length - 1, 0))
    else {
      val between = filled.zip(filled.tail).flatMap { case (a, b) =>
        val empty = b - a - 1
        val separator =
          if (folded && isText(lines(a)) && isText(lines(b)))
            if (empty == 0) Vector(Escaped(" ")) else newlines(empty)
          else newlines(empty + 1)
        separator ++ lines(b)
      }
      newlines(filled.head) ++ lines(filled.head) ++ between ++ newlines(lines.length - 1 - filled.last)
    }
  }

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
    * lone CR, NEL, U+2028 or U+2029 stops the search too, so that it is reported rather than skipped.
    */
  private def closingBrace(quoted: Boolean): Int = {
    var i = offset
    while (has(i) && !at(i, '}') && !isNewline(i) && !isBadBreak(i) && !(quoted && at(i, '"'))) i += 1
    if (at(i, '}')) i else -1
  }

  /** With no `}` to close it, a broken substitution runs to where the scalar around it would end — or, inside quotes,
    * to a line break of any kind, so that a bad one is reported rather than skipped.
    */
  private def endsAfterReference(i: Int, quoted: Boolean): Boolean =
    if (quoted) at(i, '"') || isNewline(i) || isBadBreak(i) else endsUnquoted(i)

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

  sealed trait Indent
  case object CommonIndent extends Indent
  case object StripIndent extends Indent
  case object KeepIndent extends Indent
  final case class WidthIndent(width: Int) extends Indent

  final case class Layout(indent: Indent, folded: Boolean)
}
