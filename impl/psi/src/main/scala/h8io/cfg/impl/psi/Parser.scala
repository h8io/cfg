package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Parser.*
import h8io.cfg.impl.psi.Token.*

import scala.annotation.tailrec
import scala.util.control.ControlThrowable

/** Reads the syntax tree of one source by recursive descent over its tokens (`format.md` §3, §4, §6, §9).
  *
  * The lexer's errors are collected as it reports them, and it goes on past them. So does the parser, at the
  * synchronisation points of §11: a broken field, element or argument is reported, skipped to the next separator at its
  * bracket depth or to the bracket closing its container, and left out of the tree. A directive call with any error in
  * it is left out whole. An unbalanced bracket ends the source, as a fatal error of the lexer does; one that closes
  * nothing at the top level is skipped.
  */
private[psi] final class Parser(lexer: Lexer, base: Int, limit: Int) {

  /** How many errors had been reported before the last token taken through [[next]] was read. */
  private var reportedBefore = 0

  /** How many errors had been reported once the last token taken through [[next]] was read. */
  private var reportedAfter = 0

  /** Whether the last value read was an unquoted scalar, which a second word after it makes `a: foo bar`. */
  private var unquoted = false

  /** Whether the last value read held a substitution, which a value after it makes HOCON's `${a}"b"`. */
  private var substituted = false

  /** The mode the last token taken through [[next]] was read in, where skipping a broken item goes on in. */
  private var reading: Mode = KeyMode

  /** The brackets open around the token being read, innermost first. */
  private var open: List[Closing] = Nil

  /** The keys after a dot of the dotted keys around the token being read, each a level (`format.md` §11). */
  private var dots = 0

  /** The maps and sequences opened by indentation around the token being read, each a level. */
  private var levels = 0

  /** The levels open around the token being read: those around the source, its brackets, its dotted keys and its blocks
    * opened by indentation.
    */
  private def depth: Int = base + open.size + dots + levels

  /** The containers around the token being read, innermost first: a block's region, whose lines the indentation syntax
    * counts, or none for `[…]` and `(…)`, where indentation means nothing (`format.md` §4).
    */
  private var layouts: List[Option[Region]] = Nil

  /** The column of the first token on the line of the last token taken through [[next]]. */
  private var lineColumn = 1

  /** The column that a block opened below a `:`, a tag or a `- ` ending its line must go deeper than. */
  private var deeper = 0

  /** Where a line indented less than a block stands, the block having ended there: a block further out has to have it
    * on its width, or it lands on none.
    */
  private var dedent: Option[SourceLocation] = None

  /** Where the last token read stands, to come back to before it. */
  private var before: Lexer.Mark = lexer.mark()

  /** Where the item being read starts, read from there again in another mode when its first token says to. */
  private var itemMark: Lexer.Mark = lexer.mark()

  /** Where the doc comment before the item being read stands, if it has one. */
  private var docAt: Option[SourceLocation] = None

  /** Before `token` opens a level: past the limit, the source ends there (§11). */
  private def enter(token: Token, reported: Int = reportedAfter): Unit =
    if (depth >= limit) halt(token, DepthExceeded(token.location, limit, fatal = true), reported)

  def document(): Result[AST.Document] = {
    // the top level starts at column 1 (§4)
    val statements = new Statements
    items(KeyMode, None, documented = true, Some(new Region(1, indented = false)))(statements)
    Result(AST.Document(statements.read.result()), lexer.errors)
  }

  private def next(mode: Mode): Token = {
    reportedBefore = lexer.errorCount
    reading = mode
    before = lexer.mark()
    val token = lexer.next(mode)
    reportedAfter = lexer.errorCount
    if (token.startsLine && token.kind != Newline) lineColumn = token.location.column
    token
  }

  /** Comes back to `mark`, after reading ahead found nothing to stop at. */
  private def rewind(mark: Lexer.Mark): Unit = {
    lexer.reset(mark)
    reportedAfter = lexer.errorCount
  }

  /** Reads past newlines without taking the tokens through [[next]]: what it finds only explains an error already
    * found, and the lexer's errors on it are none of the parser's business.
    */
  @tailrec private def skipNewlines(mode: Mode): Token = {
    val token = lexer.next(mode)
    if (token.kind == Newline) skipNewlines(mode) else token
  }

  /** Breaks the item being read at `error`, found at `token`, keeping the lexer's errors up to `reported`; the item is
    * skipped from `token` on. At the end of a source that a fatal error has ended nothing is reported but the fatal
    * error, which is the cause (§11), and the source ends.
    */
  private def fail(token: Token, error: PsiError, reported: Int = reportedAfter): Nothing =
    if (token.kind == End && lexer.halted) throw Halt(None, lexer.errorCount)
    else throw Broken(error, reported, token)

  /** Ends the source at `error`, found at `token`, keeping the lexer's errors up to `reported`. */
  private def halt(token: Token, error: PsiError, reported: Int): Nothing =
    if (token.kind == End && lexer.halted) throw Halt(None, lexer.errorCount)
    else throw Halt(Some(error), reported)

  /** Reports `error`, keeping the lexer's errors up to `reported`, and goes on. */
  private def record(error: PsiError, reported: Int = reportedAfter): Unit = {
    lexer.settle(reported, Some(error))
    reportedAfter = lexer.errorCount
  }

  /** The fields of a block, the elements of a sequence or the arguments of a directive, separated by `,` and newlines
    * (§3): a run of newlines with at most one `,` is one separator, and a `,` with no item before it in its container
    * is an empty element.
    *
    * In a `region` the lines are counted (§4): a line starts at the region's width, set by the first line of a `{…}`
    * that starts in it; a line further left ends a block opened by indentation, the line then read by a block further
    * out, and one that lands on no width, or goes deeper where nothing opens a block, is reported and skipped. A line
    * holding only a doc comment is not counted, the comment going with the item after it; one that starts with a doc
    * comment and goes on with its item starts where the doc comment does.
    *
    * A broken item is skipped (§11). A bracket that closes another container than this one ends the source when any is
    * open, and is skipped when none is; in a block opened by indentation it ends the block, for a container further out
    * to close.
    *
    * @param closing
    *   the bracket that ends the container, with the location of the one that opened it; none for a document, which the
    *   end of the source ends, and for a block opened by indentation, which a line further left ends
    * @param documented
    *   whether an item may have a doc comment before it; arguments may not
    */
  private def items(mode: Mode, closing: Option[Closing], documented: Boolean, region: Option[Region] = None)(
      item: Item): Unit = {
    val indented = region.exists(_.indented)
    var separated = true
    var afterItem = false
    var mark = lexer.mark()
    var token = next(mode)
    var done = false
    // the doc comment waiting for its item, with where it starts
    var doc: Option[(Token, Lexer.Mark)] = None
    def advance(): Unit = {
      mark = lexer.mark()
      token = next(mode)
    }
    def misplaced(): Unit = {
      // at the end a fatal error made, the doc comment may have stood before what it cut off
      doc.foreach(doc => if (token.kind != End || !lexer.halted) record(MisplacedDocComment(doc._1.location)))
      doc = None
    }
    // a block opened by indentation ends before `token`, and before the doc comment waiting for it
    def end(): Unit = {
      rewind(doc.fold(mark)(_._2))
      doc = None
      done = true
    }
    // a line starts at `start`: its first token, or the doc comment it starts with when its item goes on after it;
    // whether the item on it is to be read
    def lined(region: Region, start: SourceLocation): Boolean = {
      val column = start.column
      if (region.width < 0) region.width = column
      if (column == region.width) true
      else {
        if (column < region.width && region.indented) {
          dedent = Some(start)
          end()
        } else {
          // its doc comment is skipped with the line it documents
          doc = None
          record(
            if (column < region.width || dedent.contains(start)) MisalignedIndent(start) else UnexpectedIndent(start))
          token = skipLine(token, mode)
          mark = lexer.remark(before)
        }
        false
      }
    }
    open = closing.toList ++ open
    layouts = region :: layouts
    try
      while (!done) {
        val atItem = token.kind match {
          case Newline =>
            separated = true
            advance()
            false
          case Comma =>
            misplaced()
            if (!afterItem) record(EmptyElement(token.location))
            afterItem = false
            separated = true
            advance()
            false
          case End =>
            closing match {
              case Some(closing) => halt(token, unclosed(closing), reportedAfter)
              case None if indented => end()
              case None =>
                misplaced()
                done = true
            }
            false
          case RightBrace | RightBracket | RightParen if indented && open.exists(_.kind == token.kind) =>
            end()
            false
          case RightBrace | RightBracket | RightParen =>
            misplaced()
            if (closing.exists(_.kind == token.kind)) done = true
            else if (open.isEmpty) {
              record(UnmatchedBracket(token.location, bracket(token), fatal = false))
              advance()
            } else closedElsewhere(token, reportedAfter)
            false
          case DocComment(_) if documented && (separated || token.startsLine) =>
            misplaced()
            doc = Some((token, mark))
            advance()
            false
          case DocComment(_) =>
            misplaced()
            record(MisplacedDocComment(token.location))
            advance()
            false
          case _ =>
            region match {
              case Some(region) if token.startsLine => lined(region, token.location)
              case Some(region) =>
                doc match {
                  case Some((comment, _)) if comment.startsLine => lined(region, comment.location)
                  case _ => true
                }
              case None => true
            }
        }
        // the item is read here rather than in a helper: an item nested in another recurses through this method, and
        // every frame on that path costs stack at each level
        if (atItem) {
          // a line starts with the item, or with the doc comment before it
          val starts = token.startsLine || doc.exists(_._1.startsLine)
          val text = doc.collect { case (Token(DocComment(text), _, _, _), _) => text }
          docAt = doc.map(_._1.location)
          itemMark = mark
          doc = None
          try {
            // a line that starts after a block opened by indentation is separated from the item that opened it
            if (!separated && !starts) afterValue(token, mode, mark)
            item(token, text)
            advance()
          } catch {
            case Broken(error, reported, at) =>
              record(error, reported)
              token = skip(at, mode)
              // what was reported stays, should a block opened by indentation end before the token
              mark = lexer.remark(before)
            case Dropped => advance()
          }
          afterItem = true
          separated = false
        }
      }
    catch {
      case Halt(error, reported) if closing.isEmpty && !indented => lexer.settle(reported, error)
    } finally {
      open = open.drop(closing.size)
      layouts = layouts.tail
    }
  }

  /** Skips the rest of a line from `token`, which is part of it, to the newline, closing bracket or end after it. */
  @tailrec private def skipLine(token: Token, context: Mode): Token = {
    val found = skip(token, context)
    if (found.kind == Comma) skipLine(lexer.next(context), context) else found
  }

  /** A bracket, `token`, that closes none of the brackets open: an unclosed bracket when it closes one further out, a
    * bracket closing nothing otherwise, either ending the source.
    */
  private def closedElsewhere(token: Token, reported: Int): Nothing =
    if (open.exists(_.kind == token.kind)) halt(token, unclosed(open.head), reported)
    else halt(token, UnmatchedBracket(token.location, bracket(token), fatal = true), reported)

  /** The bracket `closing` closes, never closed. */
  private def unclosed(closing: Closing): UnclosedBracket =
    UnclosedBracket(
      closing.opening,
      closing.kind match {
        case RightBrace => '{'
        case RightBracket => '['
        case _ => '('
      })

  /** A closing bracket as written. */
  private def bracket(token: Token): Char =
    token.kind match {
      case RightBrace => '}'
      case RightBracket => ']'
      case _ => ')'
    }

  /** Skips a broken item from `token`, which is part of it, to the separator, closing bracket or end after it at the
    * same bracket depth, and answers that. The item is in a container whose items start in `context` and goes on in the
    * mode the parser read last.
    *
    * What the lexer finds on the way is forgotten, a fatal error but excepted: the tokens are read in a guessed mode,
    * and the parser cannot tell what they were meant to be (§11).
    */
  private def skip(token: Token, context: Mode): Token = {
    val reported = lexer.errorCount
    @tailrec def loop(token: Token, mode: Mode): Token =
      token.kind match {
        case Newline | Comma | End | RightBrace | RightBracket | RightParen => token
        case _ =>
          val after = skipOver(token, context, mode, reported)
          before = lexer.mark()
          loop(lexer.next(after), after)
      }
    val found = loop(token, reading)
    lexer.settle(reported, None)
    reportedAfter = lexer.errorCount
    found
  }

  /** Skips past `token`, read in `mode` in a container whose items start in `context`: a container it opens with
    * everything in it, the arguments of a directive it names, `(` on the directive's line. Answers the mode to read on
    * in, as the parser would: in a block, a value after `:`, `:=` or `=`, a key after a separator.
    */
  private def skipOver(token: Token, context: Mode, mode: Mode, reported: Int): Mode =
    token.kind match {
      case LeftBrace =>
        enter(token, reported)
        skipNested(Closing(RightBrace, token.location), KeyMode, reported)
        mode
      case LeftBracket =>
        enter(token, reported)
        skipNested(Closing(RightBracket, token.location), ValueMode, reported)
        mode
      case LeftParen =>
        enter(token, reported)
        skipNested(Closing(RightParen, token.location), ArgumentMode, reported)
        mode
      case Directive(_) =>
        // arguments apart from the name on its line are its still, or their separators would end the skipping
        val mark = lexer.mark()
        val paren = lexer.next(KeyMode)
        if (paren.kind == LeftParen) {
          enter(paren, reported)
          skipNested(Closing(RightParen, paren.location), ArgumentMode, reported)
        } else lexer.reset(mark)
        mode
      case Newline | Comma => context
      case Colon | Replace | Equals if context == KeyMode => ValueMode
      case _ => mode
    }

  /** Skips to the bracket that closes `closing`'s, its items starting in `context`; the end of the source or a wrong
    * bracket first ends it.
    */
  private def skipNested(closing: Closing, context: Mode, reported: Int): Unit = {
    open = closing :: open
    @tailrec def loop(mode: Mode): Unit = {
      val token = lexer.next(mode)
      token.kind match {
        case kind if kind == closing.kind => ()
        case End => halt(token, unclosed(closing), reported)
        case RightBrace | RightBracket | RightParen => closedElsewhere(token, reported)
        case _ => loop(skipOver(token, context, mode, reported))
      }
    }
    loop(context)
    open = open.tail
  }

  /** Something other than a separator after an item, `token`, read from `mark`.
    *
    * In a block that is a second field on the line, or a second word of a value, told apart by whether a field starts
    * there, read ahead in key mode. The word is then read again from `mark` in value mode, last, so that what the lexer
    * reports is a value's — in key mode it may find an invalid key in a word of a value — and stays as found.
    */
  private def afterValue(token: Token, mode: Mode, mark: Lexer.Mark): Nothing =
    mode match {
      case KeyMode =>
        lexer.reset(mark)
        val field = lexer.next(KeyMode).kind.isInstanceOf[Name] && lexer.fieldAhead()
        lexer.reset(mark)
        val word = next(ValueMode)
        // a word with a substitution in it starts no field, keys being never computed
        val error =
          if (field && !substitutes(word)) MissingSeparator(word.location, describe(word.kind))
          else wordError(word, inBlock = true)
        fail(word, error)
      case ValueMode => fail(token, wordError(token, inBlock = false))
      case ArgumentMode => fail(token, MissingSeparator(token.location, describe(token.kind)))
    }

  /** A second value after one: joined, as HOCON joins them, when either holds a substitution; a second word of an
    * unquoted scalar in a block; two values with no separator otherwise.
    */
  private def wordError(word: Token, inBlock: Boolean): PsiError =
    word.kind match {
      case Scalar(_, _, _) if substituted || substitutes(word) => JoinedValues(word.location)
      case Scalar(parts, _, _) if inBlock && unquoted => SpaceInValue(word.location, written(parts))
      case _ => MissingSeparator(word.location, describe(word.kind))
    }

  private def substitutes(token: Token): Boolean =
    token.kind match {
      case Scalar(parts, _, _) => parts.exists(Token.substitutes)
      case _ => false
    }

  private def statement(first: Token, doc: Option[String]): AST.Statement =
    first.kind match {
      case Name(name, _) => keys(AST.Key(name, first.location), Nil, doc)
      case Directive(name) => AST.DirectiveStatement(call(first, name), doc)
      // among the fields of a block a substitution alone is an injection (§9)
      case Scalar(Vector(Reference(path, optional, _)), false, _) =>
        AST.InjectionStatement(path, optional, first.location, doc)
      case Scalar(Vector(_: Formatted), false, _) => fail(first, FormattedInjection(first.location))
      case Scalar(_, false, _) => fail(first, MisplacedInjection(first.location))
      case Tag(_) => fail(first, TagBeforeKey(first.location))
      case Scalar(parts, _, _) => fail(first, QuotedKey(first.location, written(parts)))
      case _ => fail(first, ExpectedKey(first.location, describe(first.kind)))
    }

  /** The rest of a field after its first key; `rest` holds the keys after the first, last first.
    *
    * The keys after a dot are counted in [[dots]] here rather than by a helper taking the value by name: a value nested
    * in a value recurses through this method, and every frame on that path costs stack at each level.
    */
  @tailrec private def keys(head: AST.Key, rest: List[AST.Key], doc: Option[String]): AST.Field = {
    val mark = lexer.mark()
    val token = next(KeyMode)
    token.kind match {
      case Dot =>
        dots += rest.size
        try enter(token)
        finally dots -= rest.size
        keys(head, dotted(token) :: rest, doc)
      case Colon | Replace | LeftBrace =>
        if (token.kind != LeftBrace) below(lineColumn)
        dots += rest.size
        val read =
          try if (token.kind == LeftBrace) block(token, None) else value(next(ValueMode), None)
          finally dots -= rest.size
        val operator = if (token.kind == Replace) AST.Operator.Replace else AST.Operator.Merge
        AST.Field(::(head, rest.reverse), operator, read, doc)
      case Tag(_) => fail(token, TagAfterKey(token.location))
      case Newline =>
        val after = skipNewlines(KeyMode)
        if (after.kind == LeftBrace) fail(after, BraceOnNextLine(after.location))
        lexer.reset(mark)
        fail(next(KeyMode), ExpectedColon(token.location, describe(token.kind)))
      case DocComment(_) => fail(token, MisplacedDocComment(token.location))
      case _ => fail(token, ExpectedColon(token.location, describe(token.kind)))
    }
  }

  /** The key after `dot`, which touches it on both sides. */
  private def dotted(dot: Token): AST.Key = {
    if (dot.spaced) fail(dot, SpaceAroundDot(dot.location))
    val token = next(KeyMode)
    token.kind match {
      case Name(name, _) =>
        if (token.spaced) fail(token, SpaceAroundDot(dot.location))
        AST.Key(name, token.location)
      // a substitution, which key mode reads as one for an injection: a key is never computed
      case Scalar(parts, false, _) => fail(token, InvalidKey(token.location, written(parts)))
      case Scalar(parts, _, _) => fail(token, QuotedKey(token.location, written(parts)))
      case _ => fail(token, ExpectedKey(token.location, describe(token.kind)))
    }
  }

  /** A value from `token`, after the tag already read before it; a second one is reported (`format.md` §5). */
  @tailrec private def value(token: Token, tag: Option[String]): AST.Value =
    token.kind match {
      case Tag(name) =>
        if (tag.isDefined) record(SecondTag(token.location))
        value(next(ValueMode), tag.orElse(Some(name)))
      case Scalar(parts, quoted, markers) =>
        val value = scalar(token, parts, quoted, tag)
        if (markers.isEmpty) value else AST.Marked(value, markers)
      case LeftBrace => block(token, tag)
      case LeftBracket =>
        enter(token)
        val elements = new SequenceElements
        items(ValueMode, Some(Closing(RightBracket, token.location)), documented = true)(elements)
        ended()
        AST.Sequence(elements.read.result(), tag, token.location)
      case Directive(name) => AST.DirectiveValue(call(token, name), tag)
      case Name(_, _) => fail(token, BacktickedValue(token.location))
      // inside `[…]` indentation means nothing, and a value goes on its tag's line (§4)
      case Newline if layouts.headOption.flatten.isEmpty =>
        val after = skipNewlines(ValueMode)
        after.kind match {
          case Scalar(_, _, _) | LeftBrace | LeftBracket | Tag(_) | Directive(_) | Name(_, _) =>
            fail(after, ValueOnNextLine(after.location))
          // a separator, a closing bracket, the end or a doc comment, where skipping stops or which it drops
          case _ => fail(after, MissingValue(token.location, describe(token.kind)))
        }
      case Newline => indented(token, tag)
      case DocComment(_) => fail(token, MisplacedDocComment(token.location))
      case _ => fail(token, MissingValue(token.location, describe(token.kind)))
    }

  /** A scalar: a substitution when it is one unquoted `${…}` and nothing else, a concatenation when it holds any other,
    * `null` when it is that word unquoted (§3, §9).
    */
  private def scalar(token: Token, parts: Vector[Part], quoted: Boolean, tag: Option[String]): AST.Value = {
    unquoted = !quoted
    substituted = parts.exists(Token.substitutes)
    parts match {
      case Vector(Reference(path, optional, _)) if !quoted => AST.Substitution(path, optional, tag, token.location)
      case Vector(formatted: Formatted) if !quoted => AST.Formatted(formatted, tag, token.location)
      case _ if substituted => AST.Concatenation(parts, tag, token.location)
      case _ =>
        val value = text(parts)
        if (!quoted && value == "null") AST.Null(tag, token.location) else AST.Scalar(value, tag, token.location)
    }
  }

  /** Sets the column a block opened below the value about to be read goes deeper than: that of the block it stands in,
    * or of the line, `column`, if that goes deeper still (§4).
    */
  private def below(column: Int): Unit =
    deeper = layouts.headOption.flatten.fold(column)(region => math.max(region.width, column))

  /** The value below `newline`, which ends the line of a `:`, a tag or a `- `: a block indented deeper than [[deeper]],
    * with `tag` written before it; with no line deeper, an error (§4).
    */
  private def indented(newline: Token, tag: Option[String]): AST.Value = {
    val threshold = deeper
    val mark = lexer.mark()
    @tailrec def first(): Token = {
      val token = lexer.next(KeyMode)
      token.kind match {
        case Newline | DocComment(_) => first()
        case _ => token
      }
    }
    val after = first()
    // a fatal error ahead ends the source, and is all that is reported (§11)
    if (after.kind == End && lexer.halted) fail(after, NothingIndented(newline.location))
    rewind(mark)
    after.kind match {
      case End | RightBrace | RightBracket | RightParen => fail(newline, NothingIndented(newline.location))
      case _ if after.location.column <= threshold => fail(newline, NothingIndented(newline.location))
      case _ => content(after.location, tag, after.location.column)
    }
  }

  /** A block opened by indentation, its first token at `at`, the lines of its block starting at its column: a map, a
    * sequence of `- ` elements, one value, which takes `tag` too, or substitutions and calls alone, two or more, which
    * what they find or give makes a map or a sequence (§4, §9). Among fields or `- ` elements, a substitution or a call
    * alone on its line is an injection. The first item on its line goes below to a block deeper than `first`, as it
    * would after a `- `.
    */
  private def content(at: SourceLocation, tag: Option[String], first: Int): AST.Value = {
    val elements = Vector.newBuilder[AST.Element]
    val fields = Vector.newBuilder[AST.Statement]
    // a value with its doc comment, where that stands, and its first token
    val values = Vector.newBuilder[(AST.Value, Option[(String, SourceLocation)], Token)]
    var kind: BlockKind = Unknown
    var counted = false
    var read = 0
    // a map or a sequence is a level, which a block turns out to be once
    def opens(token: Token): Unit = {
      enter(token)
      levels += 1
      counted = true
    }
    // a value alone on its line among fields: an injection, or a call in statement position
    def field(value: AST.Value, doc: Option[String], at: Token): Unit =
      injected(value, at, ValueAmongFields(at.location)).foreach { item =>
        fields +=
          item.fold(AST.DirectiveStatement(_, doc), r => AST.InjectionStatement(r.path, r.optional, r.location, doc))
        ()
      }
    // a value alone on its line among `- ` elements: an injection of a sequence
    def element(value: AST.Value, doc: Option[String], at: Token): Unit =
      injected(value, at, MixedBlock(at.location)).foreach { _ =>
        elements += AST.Element(value, doc, AST.Put.Elements)
        ()
      }
    // the block turns out to be `to`, the values read before placed in it
    def becomes(token: Token, to: BlockKind)(place: (AST.Value, Option[String], Token) => Unit): Unit =
      if (kind != to) {
        opens(token)
        kind = to
        values.result().foreach { case (value, doc, at) => place(value, doc.map(_._1), at) }
        values.clear()
      }
    try
      items(KeyMode, None, documented = true, Some(new Region(at.column, indented = true)))(new Item {
        def apply(token: Token, doc: Option[String]): Unit = {
          val doced = docAt
          val threshold = if (read == 0) first else at.column
          read += 1
          token.kind match {
            case Dash if kind == Fields => fail(token, MixedBlock(token.location))
            case Dash =>
              becomes(token, Elements)(element)
              elements += dashed(token, doc)
              ()
            case Name(_, _) if lexer.fieldAhead() =>
              if (kind == Elements) fail(token, MixedBlock(token.location))
              becomes(token, Fields)(field)
              fields += statement(token, doc)
              ()
            case _ =>
              val start = itemMark
              // tags alone on their line, their value in a block below: a block in a block, which counts as a level
              var below = false
              if (tagged(token)) {
                val ahead = lexer.mark()
                val after = next(KeyMode)
                if (after.kind.isInstanceOf[Name] && lexer.fieldAhead()) fail(after, TagBeforeField(token.location))
                below = after.kind == Newline
                rewind(ahead)
              }
              rewind(start)
              deeper = threshold
              val first = next(ValueMode)
              if (below) {
                enter(first)
                levels += 1
              }
              val value =
                try Parser.this.value(first, None)
                finally if (below) levels -= 1
              kind match {
                case Fields => field(value, doc, first)
                case Elements => element(value, doc, first)
                case _ =>
                  kind = Values
                  values += ((value, doc.zip(doced), first))
                  ()
              }
          }
        }
      })
    finally if (counted) levels -= 1
    ended()
    kind match {
      case Elements => AST.Sequence(elements.result(), tag, at)
      case Fields => AST.Block(fields.result(), tag, at)
      case _ =>
        val all = values.result()
        val items = all.flatMap { case (value, _, _) => injection(value) }
        all match {
          // a formatted substitution among them injects nothing (§9)
          case _ +: _ +: _
              if all.exists(_._1.isInstanceOf[AST.Formatted]) &&
                all.forall { case (value, _, _) => value.isInstanceOf[AST.Formatted] || injection(value).nonEmpty } =>
            all.foreach {
              case (_: AST.Formatted, _, at) => record(FormattedInjection(at.location))
              case _ => ()
            }
            AST.Block(Vector.empty, tag, at)
          case _ +: _ +: _ if items.size == all.size =>
            AST.Injections(
              items.map(_.fold[AST.Injected](AST.Injected.Calling(_), AST.Injected.Referring(_))), tag, at)
          case (value, doc, start) +: rest =>
            // the tag after the key and one before the value are two on one value (§4, §5)
            if (tag.isDefined && value.tag.isDefined) record(SecondTag(start.location))
            doc.foreach { case (_, at) => record(MisplacedDocComment(at)) }
            rest.foreach { case (_, _, at) => record(SecondValue(at.location)) }
            lone(value, tag)
          // every item broken, and reported
          case _ => AST.Block(Vector.empty, tag, at)
        }
    }
  }

  /** What `value`, alone on its line among fields or elements, injects: a call or a reference; none when it is anything
    * else, reported as `otherwise`, or tagged, its tag having no node to go to.
    */
  private def injected(value: AST.Value, at: Token, otherwise: => PsiError): Option[Either[AST.Call, Reference]] = {
    val item = injection(value)
    if (item.isEmpty)
      value match {
        case _: AST.Substitution | _: AST.DirectiveValue => record(TaggedInjection(at.location))
        case _: AST.Formatted => record(FormattedInjection(at.location))
        case _ => record(otherwise)
      }
    item
  }

  /** What `value` injects where it stands alone: a call or a reference, untagged; none when it is anything else. */
  private def injection(value: AST.Value): Option[Either[AST.Call, Reference]] =
    value match {
      case AST.Substitution(path, optional, tag, location) if tag.isEmpty =>
        Some(Right(Reference(path, optional, location)))
      case AST.DirectiveValue(call, tag) if tag.isEmpty => Some(Left(call))
      case _ => None
    }

  /** Whether `token` is a tag; the tags from it on are read past, however many, with no recursion. */
  private def tagged(token: Token): Boolean =
    token.kind match {
      case Tag(_) =>
        pastTags()
        true
      case _ => false
    }

  /** Reads past the tags ahead, up to the first token that is none. */
  @tailrec private def pastTags(): Unit = {
    val mark = lexer.mark()
    next(KeyMode).kind match {
      case Tag(_) => pastTags()
      case _ => rewind(mark)
    }
  }

  /** One value alone in a block opened by indentation, which takes the `tag` written before the block when it has none
    * of its own, a second being reported before: a substitution or a call alone after a key or a `- ` is its value,
    * whole (§4, §9).
    */
  private def lone(value: AST.Value, tag: Option[String]): AST.Value =
    value match {
      case v: AST.DirectiveValue => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Scalar => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Null => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Block => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Sequence => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Substitution => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Injections => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Concatenation => v.copy(tag = v.tag.orElse(tag))
      case v: AST.Formatted => v.copy(tag = v.tag.orElse(tag))
      case AST.Marked(inner, markers) => AST.Marked(lone(inner, tag), markers)
    }

  /** An element after its `- `, `dash`: a value on the dash's line, the lines of a block it opens aligned under its
    * first token, or below it, deeper than the dash (§4).
    */
  private def dashed(dash: Token, doc: Option[String]): AST.Element = {
    val mark = lexer.mark()
    val first = next(KeyMode)
    deeper = dash.location.column
    first.kind match {
      case Newline => AST.Element(indented(first, None), doc)
      case End | Comma | RightBrace | RightBracket | RightParen =>
        fail(first, MissingValue(first.location, describe(first.kind)))
      case _ =>
        rewind(mark)
        AST.Element(content(first.location, None, dash.location.column), doc)
    }
  }

  private def block(open: Token, tag: Option[String]): AST.Block = {
    enter(open)
    val statements = new Statements
    items(KeyMode, Some(Closing(RightBrace, open.location)), documented = true, Some(new Region(-1, indented = false)))(
      statements)
    ended()
    AST.Block(statements.read.result(), tag, open.location)
  }

  /** After a container or a directive, a word that follows is no second word of a scalar. */
  private def ended(): Unit = {
    unquoted = false
    substituted = false
  }

  /** An element in `[…]`, where a substitution or a call is an injection of what it finds or gives (§9). */
  private def sequenceElement(first: Token, doc: Option[String]): AST.Element = {
    val read = value(first, None)
    read match {
      case _: AST.Substitution | _: AST.DirectiveValue => AST.Element(read, doc, AST.Put.Found)
      case _ => AST.Element(read, doc)
    }
  }

  /** The fields of a block, collected as they are read. */
  private final class Statements extends Item {
    val read: collection.mutable.Builder[AST.Statement, Vector[AST.Statement]] = Vector.newBuilder

    def apply(first: Token, doc: Option[String]): Unit = {
      read += statement(first, doc)
      ()
    }
  }

  /** The elements of a sequence in `[…]`, collected as they are read. */
  private final class SequenceElements extends Item {
    val read: collection.mutable.Builder[AST.Element, Vector[AST.Element]] = Vector.newBuilder

    def apply(first: Token, doc: Option[String]): Unit = {
      read += sequenceElement(first, doc)
      ()
    }
  }

  /** A directive from its `#name` token, the last one read: with its arguments when a `(` touches the name, with none
    * when nothing does — a `(` apart from the name, on its line or a later one, is an error (§6).
    *
    * A call with any error in it, its name's or its arguments', is read to its end and then left out, its item with it:
    * a handler must not run on arguments that are not what was meant (§11).
    */
  private def call(directive: Token, name: String): AST.Call = {
    val before = reportedBefore
    val mark = lexer.mark()
    val token = next(KeyMode)
    val read = token.kind match {
      case LeftParen if !token.spaced =>
        enter(token)
        arguments(token)
      case LeftParen => fail(token, DetachedArguments(token.location))
      case Newline =>
        val after = skipNewlines(KeyMode)
        if (after.kind == LeftParen) fail(after, DetachedArguments(after.location))
        rewind(mark)
        Vector.empty
      case _ =>
        rewind(mark)
        Vector.empty
    }
    ended()
    if (lexer.errorCount > before) throw Dropped
    AST.Call(name, read, directive.location)
  }

  /** The arguments after `open`, positional ones first and no name twice (§6). Each one out of place is reported as it
    * is read, so that an error further on that ends the source does not hide it.
    */
  private def arguments(open: Token): Vector[AST.Argument] = {
    var named = Set.empty[String]
    val arguments = Vector.newBuilder[AST.Argument]
    items(ArgumentMode, Some(Closing(RightParen, open.location)), documented = false)(new Item {
      def apply(first: Token, doc: Option[String]): Unit = {
        val read = argument(first)
        read.name match {
          case Some(name) =>
            if (named(name)) record(DuplicateArgument(read.location, name), lexer.errorCount)
            named += name
          case None => if (named.nonEmpty) record(PositionalAfterNamed(read.location), lexer.errorCount)
        }
        arguments += read
        ()
      }
    })
    arguments.result()
  }

  /** `name: literal`, or a literal alone; `name = literal` is an error that says to write `:` (§6). */
  private def argument(first: Token): AST.Argument =
    first.kind match {
      case Name(name, _) =>
        val mark = lexer.mark()
        val token = next(ArgumentMode)
        token.kind match {
          case Colon =>
            val value = next(ArgumentMode)
            AST.Argument(Some(name), literal(value), first.location, value.location)
          case Equals => fail(token, EqualsInArgument(token.location))
          case _ =>
            rewind(mark)
            AST.Argument(None, literal(first), first.location, first.location)
        }
      case Integer(_) | Scalar(_, _, _) => AST.Argument(None, literal(first), first.location, first.location)
      case _ => fail(first, ExpectedArgument(first.location, describe(first.kind)))
    }

  /** An argument's value, as the string the handler gets (§6): a bare name, an integer as written, a quoted string. */
  private def literal(token: Token): String =
    token.kind match {
      case Name(name, false) => name
      case Name(_, true) => fail(token, BacktickedValue(token.location))
      case Integer(value) => value
      case Scalar(parts, _, _) =>
        parts.collectFirst {
          case reference: Reference => reference
          case Formatted(reference, _) => reference
        } match {
          case Some(reference) => fail(token, SubstitutionInArgument(reference.location))
          case None => text(parts)
        }
      case _ => fail(token, MissingValue(token.location, describe(token.kind)))
    }

  private def text(parts: Vector[Part]): String = parts.collect { case Text(value) => value }.mkString
}

private[psi] object Parser {

  /** `source` parsed, standing `base` levels deep, where a directive placed it, its tree limited to `limit` levels in
    * all (`format.md` §11).
    */
  def parse(
      source: Source,
      markers: Map[String, Marker] = Map.empty,
      base: Int = 0,
      limit: Int = PsiLoader.DefaultDepthLimit,
      formatters: Map[String, Formatter] = Map.empty): Result[AST.Document] =
    new Parser(new Lexer(source, markers, formatters), base, limit).document()

  /** The lines of a block, which start at `width`, unknown in a `{…}` until a line starts in it; `indented` for a block
    * opened by indentation, which a line further left ends (`format.md` §4).
    */
  private final class Region(var width: Int, val indented: Boolean)

  /** What a block opened by indentation turns out to hold, as its items are read. */
  private sealed trait BlockKind
  private case object Unknown extends BlockKind
  private case object Elements extends BlockKind
  private case object Fields extends BlockKind
  private case object Values extends BlockKind

  /** Reads one item of a container from its first token, with the doc comment before it: a class rather than a
    * function, which takes the stack a frame more for each item nested in another, and a few with its adapters.
    */
  private abstract class Item {
    def apply(first: Token, doc: Option[String]): Unit
  }

  /** The bracket that closes a container, and where the one that opened it stands. */
  private final case class Closing(kind: Kind, opening: SourceLocation)

  /** An item broken at `error`, found at `at`, with how many of the lexer's errors stand before it: the item is skipped
    * from `at` on.
    */
  private final case class Broken(error: PsiError, reported: Int, at: Token) extends ControlThrowable

  /** The end of the source before its end: at `error`, if any but the lexer's fatal one, with how many of the lexer's
    * errors stand before it.
    */
  private final case class Halt(error: Option[PsiError], reported: Int) extends ControlThrowable

  /** An item read to its end that is left out, its errors already reported. */
  private case object Dropped extends ControlThrowable
}
