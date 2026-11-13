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
private[psi] final class Parser(lexer: Lexer) {

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

  def document(): Result[Ast.Document] = {
    val statements = items(KeyMode, None, documented = true)(statement)
    Result(Ast.Document(statements), lexer.errors)
  }

  private def next(mode: Mode): Token = {
    reportedBefore = lexer.errorCount
    reading = mode
    val token = lexer.next(mode)
    reportedAfter = lexer.errorCount
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
    * A broken item is skipped (§11). A bracket that closes another container than this one ends the source when any is
    * open, and is skipped when none is.
    *
    * @param closing
    *   the bracket that ends the container, with the location of the one that opened it; none for a document, which the
    *   end of the source ends
    * @param documented
    *   whether an item may have a doc comment before it; arguments may not
    */
  private def items[A](mode: Mode, closing: Option[Closing], documented: Boolean)(
      item: (Token, Option[String]) => A): Vector[A] = {
    val read = Vector.newBuilder[A]
    var separated = true
    var afterItem = false
    var mark = lexer.mark()
    var token = next(mode)
    var done = false
    def advance(): Unit = {
      mark = lexer.mark()
      token = next(mode)
    }
    def itemAt(first: Token, doc: Option[String]): Unit = {
      try {
        if (!separated) afterValue(first, mode, mark)
        read += item(first, doc)
        advance()
      } catch {
        case Broken(error, reported, at) =>
          record(error, reported)
          token = skip(at, mode)
        case Dropped => advance()
      }
      afterItem = true
      separated = false
    }
    open = closing.toList ++ open
    try
      while (!done)
        token.kind match {
          case Newline =>
            separated = true
            advance()
          case Comma =>
            if (!afterItem) record(EmptyElement(token.location))
            afterItem = false
            separated = true
            advance()
          case End =>
            closing match {
              case Some(closing) => halt(token, unclosed(closing), reportedAfter)
              case None => done = true
            }
          case RightBrace | RightBracket | RightParen =>
            if (closing.exists(_.kind == token.kind)) done = true
            else if (open.isEmpty) {
              record(UnmatchedBracket(token.location, bracket(token), fatal = false))
              advance()
            } else closedElsewhere(token, reportedAfter)
          case DocComment(text) if documented && separated =>
            documentedItem(token, mode) match {
              case Right(first) => itemAt(first, Some(text))
              case Left(after) =>
                // at the end a fatal error made, the doc comment may have stood before what it cut off
                if (after.kind != End || !lexer.halted) record(MisplacedDocComment(token.location))
                mark = lexer.mark()
                token = after
            }
          case DocComment(_) =>
            record(MisplacedDocComment(token.location))
            advance()
          case _ => itemAt(token, None)
        }
    catch {
      case Halt(error, reported) if closing.isEmpty => lexer.settle(reported, error)
    }
    open = open.drop(closing.size)
    read.result()
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
        skipNested(Closing(RightBrace, token.location), KeyMode, reported)
        mode
      case LeftBracket =>
        skipNested(Closing(RightBracket, token.location), ValueMode, reported)
        mode
      case LeftParen =>
        skipNested(Closing(RightParen, token.location), ArgumentMode, reported)
        mode
      case Directive(_) =>
        // arguments apart from the name on its line are its still, or their separators would end the skipping
        val mark = lexer.mark()
        val paren = lexer.next(KeyMode)
        if (paren.kind == LeftParen) skipNested(Closing(RightParen, paren.location), ArgumentMode, reported)
        else lexer.reset(mark)
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

  /** The first token of the item `doc` stands before, on its line or a later one; or, when something else follows,
    * which makes it a misplaced doc comment, that.
    */
  @tailrec private def documentedItem(doc: Token, mode: Mode): Either[Token, Token] = {
    val token = next(mode)
    token.kind match {
      case Newline => documentedItem(doc, mode)
      case Comma | RightBrace | RightBracket | End | DocComment(_) => Left(token)
      case _ => Right(token)
    }
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
        val field = startsField(lexer.next(KeyMode))
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
      case Scalar(parts, _, _) => parts.exists(_.isInstanceOf[Reference])
      case _ => false
    }

  private def startsField(token: Token): Boolean =
    token.kind match {
      case Name(_, _) =>
        lexer.next(KeyMode).kind match {
          case Colon | Replace | Dot | LeftBrace => true
          case _ => false
        }
      case _ => false
    }

  private def statement(first: Token, doc: Option[String]): Ast.Statement =
    first.kind match {
      case Name(name, _) => keys(Ast.Key(name, first.location), Nil, doc)
      case Directive(name) => Ast.DirectiveStatement(call(first, name), doc)
      case Tag(_) => fail(first, TagBeforeKey(first.location))
      case Scalar(parts, _, _) => fail(first, QuotedKey(first.location, written(parts)))
      case _ => fail(first, ExpectedKey(first.location, describe(first.kind)))
    }

  /** The rest of a field after its first key; `rest` holds the keys after the first, last first. */
  @tailrec private def keys(head: Ast.Key, rest: List[Ast.Key], doc: Option[String]): Ast.Field = {
    def path = ::(head, rest.reverse)
    val mark = lexer.mark()
    val token = next(KeyMode)
    token.kind match {
      case Dot => keys(head, dotted(token) :: rest, doc)
      case Colon => Ast.Field(path, Ast.Operator.Merge, value(next(ValueMode), Set.empty), doc)
      case Replace => Ast.Field(path, Ast.Operator.Replace, value(next(ValueMode), Set.empty), doc)
      case LeftBrace => Ast.Field(path, Ast.Operator.Merge, block(token, Set.empty), doc)
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
  private def dotted(dot: Token): Ast.Key = {
    if (dot.spaced) fail(dot, SpaceAroundDot(dot.location))
    val token = next(KeyMode)
    token.kind match {
      case Name(name, _) =>
        if (token.spaced) fail(token, SpaceAroundDot(dot.location))
        Ast.Key(name, token.location)
      case Scalar(parts, _, _) => fail(token, QuotedKey(token.location, written(parts)))
      case _ => fail(token, ExpectedKey(token.location, describe(token.kind)))
    }
  }

  /** A value from `token`, after the tags already read before it. */
  @tailrec private def value(token: Token, tags: Set[String]): Ast.Value =
    token.kind match {
      case Tag(name) => value(next(ValueMode), tags + name)
      case Scalar(parts, quoted, markers) =>
        val value = scalar(token, parts, quoted, tags)
        if (markers.isEmpty) value else Ast.Marked(value, markers)
      case LeftBrace => block(token, tags)
      case LeftBracket =>
        val elements = items(ValueMode, Some(Closing(RightBracket, token.location)), documented = true)(element)
        ended()
        Ast.Sequence(elements, tags, token.location)
      case Directive(name) => Ast.DirectiveValue(call(token, name), tags)
      case Name(_, _) => fail(token, BacktickedValue(token.location))
      case Newline =>
        val after = skipNewlines(ValueMode)
        after.kind match {
          case Scalar(_, _, _) | LeftBrace | LeftBracket | Tag(_) | Directive(_) | Name(_, _) =>
            fail(after, ValueOnNextLine(after.location))
          // a separator, a closing bracket, the end or a doc comment, where skipping stops or which it drops
          case _ => fail(after, MissingValue(token.location, describe(token.kind)))
        }
      case DocComment(_) => fail(token, MisplacedDocComment(token.location))
      case _ => fail(token, MissingValue(token.location, describe(token.kind)))
    }

  /** A scalar: a substitution when it is one unquoted `${…}` and nothing else, a concatenation when it holds any other,
    * `null` when it is that word unquoted (§3, §9).
    */
  private def scalar(token: Token, parts: Vector[Part], quoted: Boolean, tags: Set[String]): Ast.Value = {
    unquoted = !quoted
    substituted = parts.exists(_.isInstanceOf[Reference])
    parts match {
      case Vector(Reference(path, optional, _)) if !quoted => Ast.Substitution(path, optional, tags, token.location)
      case _ if substituted => Ast.Concatenation(parts, tags, token.location)
      case _ =>
        val value = text(parts)
        if (!quoted && value == "null") Ast.Null(tags, token.location) else Ast.Scalar(value, tags, token.location)
    }
  }

  private def block(open: Token, tags: Set[String]): Ast.Block = {
    val statements = items(KeyMode, Some(Closing(RightBrace, open.location)), documented = true)(statement)
    ended()
    Ast.Block(statements, tags, open.location)
  }

  /** After a container or a directive, a word that follows is no second word of a scalar. */
  private def ended(): Unit = {
    unquoted = false
    substituted = false
  }

  private def element(first: Token, doc: Option[String]): Ast.Element = Ast.Element(value(first, Set.empty), doc)

  /** A directive from its `#name` token, the last one read: with its arguments when a `(` touches the name, with none
    * when nothing does — a `(` apart from the name, on its line or a later one, is an error (§6).
    *
    * A call with any error in it, its name's or its arguments', is read to its end and then left out, its item with it:
    * a handler must not run on arguments that are not what was meant (§11).
    */
  private def call(directive: Token, name: String): Ast.Call = {
    val before = reportedBefore
    val mark = lexer.mark()
    val token = next(KeyMode)
    val read = token.kind match {
      case LeftParen if !token.spaced => arguments(token)
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
    Ast.Call(name, read, directive.location)
  }

  /** The arguments after `open`, positional ones first and no name twice (§6). Each one out of place is reported as it
    * is read, so that an error further on that ends the source does not hide it.
    */
  private def arguments(open: Token): Vector[Ast.Argument] = {
    var named = Set.empty[String]
    items(ArgumentMode, Some(Closing(RightParen, open.location)), documented = false) { (first, _) =>
      val read = argument(first)
      read.name match {
        case Some(name) =>
          if (named(name)) record(DuplicateArgument(read.location, name), lexer.errorCount)
          named += name
        case None => if (named.nonEmpty) record(PositionalAfterNamed(read.location), lexer.errorCount)
      }
      read
    }
  }

  /** `name: literal`, or a literal alone; `name = literal` is an error that says to write `:` (§6). */
  private def argument(first: Token): Ast.Argument =
    first.kind match {
      case Name(name, _) =>
        val mark = lexer.mark()
        val token = next(ArgumentMode)
        token.kind match {
          case Colon =>
            val value = next(ArgumentMode)
            Ast.Argument(Some(name), literal(value), first.location, value.location)
          case Equals => fail(token, EqualsInArgument(token.location))
          case _ =>
            rewind(mark)
            Ast.Argument(None, literal(first), first.location, first.location)
        }
      case Integer(_) | Scalar(_, _, _) => Ast.Argument(None, literal(first), first.location, first.location)
      case _ => fail(first, ExpectedArgument(first.location, describe(first.kind)))
    }

  /** An argument's value, as the string the handler gets (§6): a bare name, an integer as written, a quoted string. */
  private def literal(token: Token): String =
    token.kind match {
      case Name(name, false) => name
      case Name(_, true) => fail(token, BacktickedValue(token.location))
      case Integer(value) => value
      case Scalar(parts, _, _) =>
        parts.collectFirst { case reference: Reference => reference } match {
          case Some(reference) => fail(token, SubstitutionInArgument(reference.location))
          case None => text(parts)
        }
      case _ => fail(token, MissingValue(token.location, describe(token.kind)))
    }

  private def text(parts: Vector[Part]): String = parts.collect { case Text(value) => value }.mkString
}

private[psi] object Parser {
  def parse(source: Source, markers: Map[String, Marker] = Map.empty): Result[Ast.Document] =
    new Parser(new Lexer(source, markers)).document()

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
