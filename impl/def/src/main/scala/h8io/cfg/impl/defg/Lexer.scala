package h8io.cfg.impl.defg

import h8io.cfg.Id
import h8io.cfg.impl.defg.Token.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Reads the tokens of one source, in the mode the parser asks for (`format.md` §3).
  *
  * Errors are collected rather than thrown. A fatal one — an unterminated quote or comment, a triple-quoted string —
  * leaves nothing to resynchronise on, so the lexer reports it and answers [[Token.End]] from then on (§11).
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
    skipBlanks()
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

  private def isNewline(i: Int): Boolean = at(i, '\n') || at(i, '\r') && at(i + 1, '\n')
  private def isBlank(c: Char): Boolean = c == ' ' || c == '\t' || c == '\r' || c == '\n'

  /** A lone CR is reported where it stands and then treated as whitespace. */
  private def skipLoneCarriageReturn(): Boolean =
    if (at(offset, '\r') && !at(offset + 1, '\n')) {
      report(LoneCarriageReturn.apply, offset)
      offset += 1
      true
    } else false

  /** Skips spaces, tabs, comments and stray comment ends — everything that is whitespace to the grammar. Newlines are
    * tokens.
    */
  private def skipBlanks(): Unit = {
    var blank = true
    while (blank && !ended && has(offset))
      if (at(offset, ' ') || at(offset, '\t')) offset += 1
      else if (skipLoneCarriageReturn()) ()
      else if (startsWith("//")) skipLineComment()
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
      if (!skipLoneCarriageReturn()) offset += 1

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
      } else if (!skipLoneCarriageReturn()) offset += 1
    if (depth > 0) {
      stop(UnterminatedComment.apply, start)
      None
    } else Some(text.substring(start + 2, offset - 2))
  }

  private def read(mode: Mode): Kind = {
    val start = offset
    text.charAt(offset) match {
      case '\n' => advance(1, Newline)
      case '\r' => advance(2, Newline) // a lone CR was skipped as a blank
      case '{' => advance(1, LeftBrace)
      case '}' => advance(1, RightBrace)
      case '[' => advance(1, LeftBracket)
      case ']' => advance(1, RightBracket)
      case ',' => advance(1, Comma)
      case '/' if isDocComment => skipBlockComment().fold[Kind](End)(comment => DocComment(comment.substring(1)))
      case '"' if startsWith("\"\"\"") => fail(TripleQuoteNotSupported.apply, start)
      case '"' => quoted()
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
        if (has(offset) && !isBlank(text.charAt(offset))) report(ColonWithoutSpace.apply, start)
        kind
      case _ =>
        while (has(offset) && !endsKey(offset)) offset += 1
        val key = text.substring(start, offset)
        if (!Id.SafeKeyPattern.matcher(key).matches()) report(InvalidKey(_, key), start)
        Name(key, quoted = false)
    }

  private def endsKey(i: Int): Boolean =
    text.charAt(i) match {
      case ' ' | '\t' | '\r' | '\n' | '.' | ':' | '=' | '{' | '}' | '[' | ']' | '(' | ')' | ',' | '"' | '`' => true
      case _ => startsWith("//", i) || startsWith("/*", i)
    }

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
    } else {
      if (c == '\'') report(ApostropheAtStart.apply, start)
      unquoted()
    }

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
      else if (!skipLoneCarriageReturn()) {
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
      if (startsWith("${")) reference(parts, quoted = true)
      else if (at(offset, '\\')) escape(parts.buffer)
      else if (!skipLoneCarriageReturn()) {
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
    while (!ended && has(offset) && !endsUnquoted(text.charAt(offset)))
      if (startsWith("${")) reference(parts, quoted = false)
      else {
        val c = text.charAt(offset)
        if (c == '"' || c == '`') report(QuoteInUnquotedScalar.apply, offset)
        parts.buffer += c
        offset += 1
      }
    if (ended) End else Scalar(parts.result(), quoted = false)
  }

  private def endsUnquoted(c: Char): Boolean = isBlank(c) || c == ',' || c == '}' || c == ']'

  /** `${path}` or `${?path}` from its `$`, its path written as `Id.path` renders one (`format.md` §9). A path that
    * cannot be read is reported — at the whitespace, if that is what broke it, at the `${` otherwise — and skipped up
    * to the next `}` on its line, or with none up to where the scalar around it ends.
    */
  private def reference(parts: Parts, quoted: Boolean): Unit = {
    val start = offset
    offset += 2
    val optional = at(offset, '?')
    if (optional) offset += 1
    path(Id.Root).flatMap(path => if (at(offset, '}')) Some(path) else None) match {
      case Some(path) =>
        offset += 1
        parts.reference(Reference(path, optional, source.location(start)))
      case None if !ended =>
        if (has(offset) && isBlank(text.charAt(offset)) && !isNewline(offset))
          report(WhitespaceInSubstitution.apply, offset)
        else report(InvalidSubstitution.apply, start)
        val close = closingBrace(quoted)
        if (close >= 0) offset = close + 1
        else while (has(offset) && !endsAfterReference(text.charAt(offset), quoted)) offset += 1
      case None => ()
    }
  }

  /** The `}` that closes a broken substitution: the next one on its line, and inside quotes before the closing one. */
  private def closingBrace(quoted: Boolean): Int = {
    var i = offset
    while (has(i) && !at(i, '}') && !at(i, '\n') && !at(i, '\r') && !(quoted && at(i, '"'))) i += 1
    if (at(i, '}')) i else -1
  }

  /** With no `}` to close it, a broken substitution runs to where the scalar around it would end. */
  private def endsAfterReference(c: Char, quoted: Boolean): Boolean =
    if (quoted) c == '"' || c == '\n' || c == '\r' else endsUnquoted(c)

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
