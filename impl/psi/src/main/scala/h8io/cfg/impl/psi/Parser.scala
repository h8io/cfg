package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Parser.*
import h8io.cfg.impl.psi.Token.*

import scala.annotation.tailrec
import scala.util.control.ControlThrowable

/** Reads the syntax tree of one source by recursive descent over its tokens (`format.md` §3, §4, §6, §9).
  *
  * The lexer's errors are collected as it reports them, and it goes on past them. The parser itself stops at its first
  * error for now; recovery at the synchronisation points of §11 is step 5 of the plan.
  */
private[psi] final class Parser(lexer: Lexer) {

  /** How many errors the lexer had reported once the last token taken through [[next]] was read. */
  private var reportedAfter = 0

  /** Whether the last value read was an unquoted scalar, which a second word after it makes `a: foo bar`. */
  private var unquoted = false

  /** Whether the last value read held a substitution, which a value after it makes HOCON's `${a}"b"`. */
  private var substituted = false

  def document(): Result[Ast.Document] =
    try {
      val statements = items(KeyMode, None, documented = true)(statement)
      Result(Ast.Document(statements), lexer.errors)
    } catch {
      case Stop(error, reported) =>
        val all = lexer.errors
        // a fatal error is the last the lexer reports, and is kept even when found reading ahead
        val fatal = if (lexer.halted && reported < all.length) List(all.last) else Nil
        Result(Ast.Document(Vector.empty), all.take(reported) ++ error ++ fatal)
    }

  private def next(mode: Mode): Token = {
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

  /** Stops at `error`, found at `token`, keeping the lexer's errors up to `reported`, and a fatal one wherever it was
    * found. An error at the end of a source that a fatal error has ended is left out, and every error of the lexer
    * kept: the fatal one is its cause (§11).
    */
  private def fail(token: Token, error: PsiError, reported: Int = reportedAfter): Nothing =
    if (token.kind == End && lexer.halted) throw Stop(None, lexer.errorCount)
    else throw Stop(Some(error), reported)

  /** The fields of a block, the elements of a sequence or the arguments of a directive, separated by `,` and newlines
    * (§3): a run of newlines with at most one `,` is one separator, and a `,` with no item before it in its container
    * is an empty element.
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
    while (!done)
      token.kind match {
        case Newline =>
          separated = true
          mark = lexer.mark()
          token = next(mode)
        case Comma =>
          if (!afterItem) fail(token, EmptyElement(token.location))
          afterItem = false
          separated = true
          mark = lexer.mark()
          token = next(mode)
        case End =>
          closing match {
            case Some(Closing(_, opening)) => fail(token, UnclosedBracket(opening))
            case None => done = true
          }
        case RightBrace | RightBracket | RightParen =>
          if (closing.exists(_.kind == token.kind)) done = true
          else fail(token, UnmatchedBracket(token.location))
        case kind =>
          val (first, doc) = kind match {
            case DocComment(text) if documented && separated => (documentedItem(token, mode), Some(text))
            case DocComment(_) => fail(token, MisplacedDocComment(token.location))
            case _ =>
              if (!separated) afterValue(token, mode, mark)
              (token, None)
          }
          read += item(first, doc)
          afterItem = true
          separated = false
          mark = lexer.mark()
          token = next(mode)
      }
    read.result()
  }

  /** The first token of the item `doc` stands before, on its line or a later one; anything else after it makes it a
    * misplaced doc comment.
    */
  @tailrec private def documentedItem(doc: Token, mode: Mode): Token = {
    val token = next(mode)
    token.kind match {
      case Newline => documentedItem(doc, mode)
      case Comma | RightBrace | RightBracket | End | DocComment(_) => fail(token, MisplacedDocComment(doc.location))
      case _ => token
    }
  }

  /** Something other than a separator after an item, `token`, read from `mark`.
    *
    * In a block that is a second field on the line, or a second word of a value, told apart by whether a field starts
    * there. A word of a value is read again from `mark` in value mode, which is what the lexer reports on then: in key
    * mode it may have found an invalid key in what is a word of a value. Whatever reading ahead in key mode found is
    * left out but a fatal error.
    */
  private def afterValue(token: Token, mode: Mode, mark: Lexer.Mark): Nothing =
    mode match {
      case KeyMode =>
        lexer.reset(mark)
        val word = next(ValueMode)
        val reported = reportedAfter
        lexer.reset(mark)
        // a word with a substitution in it starts no field, keys being never computed
        val field = !substitutes(word) && startsField(lexer.next(KeyMode))
        fail(word, if (field) MissingSeparator(word.location) else wordError(word, inBlock = true), reported)
      case ValueMode => fail(token, wordError(token, inBlock = false))
      case ArgumentMode => fail(token, MissingSeparator(token.location))
    }

  /** A second value after one: joined, as HOCON joins them, when either holds a substitution; a second word of an
    * unquoted scalar in a block; two values with no separator otherwise.
    */
  private def wordError(word: Token, inBlock: Boolean): PsiError =
    word.kind match {
      case Scalar(_, _) if substituted || substitutes(word) => JoinedValues(word.location)
      case Scalar(_, _) if inBlock && unquoted => SpaceInValue(word.location)
      case _ => MissingSeparator(word.location)
    }

  private def substitutes(token: Token): Boolean =
    token.kind match {
      case Scalar(parts, _) => parts.exists(_.isInstanceOf[Reference])
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
      case Directive(name) =>
        val directive = call(first, name)
        ended()
        Ast.DirectiveStatement(directive, doc)
      case Tag(_) => fail(first, TagBeforeKey(first.location))
      case Scalar(_, _) => fail(first, QuotedKey(first.location))
      case _ => fail(first, ExpectedKey(first.location))
    }

  /** The rest of a field after its first key; `rest` holds the keys after the first, last first. */
  @tailrec private def keys(head: Ast.Key, rest: List[Ast.Key], doc: Option[String]): Ast.Field = {
    def path = ::(head, rest.reverse)
    val token = next(KeyMode)
    token.kind match {
      case Dot => keys(head, dotted(token) :: rest, doc)
      case Colon => Ast.Field(path, Ast.Operator.Merge, value(next(ValueMode), Set.empty), doc)
      case Replace => Ast.Field(path, Ast.Operator.Replace, value(next(ValueMode), Set.empty), doc)
      case LeftBrace => Ast.Field(path, Ast.Operator.Merge, block(token, Set.empty), doc)
      case Tag(_) => fail(token, TagAfterKey(token.location))
      case Newline =>
        val after = skipNewlines(KeyMode)
        fail(token, if (after.kind == LeftBrace) BraceOnNextLine(after.location) else ExpectedColon(token.location))
      case DocComment(_) => fail(token, MisplacedDocComment(token.location))
      case _ => fail(token, ExpectedColon(token.location))
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
      case Scalar(_, _) => fail(token, QuotedKey(token.location))
      case _ => fail(token, ExpectedKey(token.location))
    }
  }

  /** A value from `token`, after the tags already read before it. */
  @tailrec private def value(token: Token, tags: Set[String]): Ast.Value =
    token.kind match {
      case Tag(name) => value(next(ValueMode), tags + name)
      case Scalar(parts, quoted) => scalar(token, parts, quoted, tags)
      case LeftBrace => block(token, tags)
      case LeftBracket =>
        val elements = items(ValueMode, Some(Closing(RightBracket, token.location)), documented = true)(element)
        ended()
        Ast.Sequence(elements, tags, token.location)
      case Directive(name) =>
        val directive = call(token, name)
        ended()
        Ast.DirectiveValue(directive, tags)
      case Name(_, _) => fail(token, BacktickedValue(token.location))
      case Newline =>
        val after = skipNewlines(ValueMode)
        after.kind match {
          case Scalar(_, _) | LeftBrace | LeftBracket | Tag(_) | Directive(_) | Name(_, _) =>
            fail(after, ValueOnNextLine(after.location))
          case _ => fail(after, MissingValue(token.location))
        }
      case DocComment(_) => fail(token, MisplacedDocComment(token.location))
      case _ => fail(token, MissingValue(token.location))
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

  /** A directive from its `#name` token: with its arguments when a `(` touches the name, with none when nothing does —
    * a `(` apart from the name, on its line or a later one, is an error (§6).
    */
  private def call(directive: Token, name: String): Ast.Call = {
    val mark = lexer.mark()
    val token = next(KeyMode)
    token.kind match {
      case LeftParen if !token.spaced => Ast.Call(name, arguments(token), directive.location)
      case LeftParen => fail(token, DetachedArguments(token.location))
      case Newline =>
        val after = skipNewlines(KeyMode)
        if (after.kind == LeftParen) fail(after, DetachedArguments(after.location))
        rewind(mark)
        Ast.Call(name, Vector.empty, directive.location)
      case _ =>
        rewind(mark)
        Ast.Call(name, Vector.empty, directive.location)
    }
  }

  /** The arguments after `open`, positional ones first and no name twice (§6). */
  private def arguments(open: Token): Vector[Ast.Argument] = {
    val read = items(ArgumentMode, Some(Closing(RightParen, open.location)), documented = false)((first, _) =>
      argument(first))
    check(open, read.toList, Set.empty)
    read
  }

  /** Stops at an argument after a named one that is positional, or named as one before it. */
  @tailrec private def check(open: Token, arguments: List[Ast.Argument], named: Set[String]): Unit =
    arguments match {
      case Ast.Argument(Some(name), _, location, _) :: rest =>
        if (named(name)) fail(open, DuplicateArgument(location, name))
        check(open, rest, named + name)
      case Ast.Argument(None, _, location, _) :: rest =>
        if (named.nonEmpty) fail(open, PositionalAfterNamed(location))
        check(open, rest, named)
      case Nil => ()
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
      case Integer(_) | Scalar(_, _) => Ast.Argument(None, literal(first), first.location, first.location)
      case _ => fail(first, ExpectedArgument(first.location))
    }

  /** An argument's value, as the string the handler gets (§6): a bare name, an integer as written, a quoted string. */
  private def literal(token: Token): String =
    token.kind match {
      case Name(name, false) => name
      case Name(_, true) => fail(token, BacktickedValue(token.location))
      case Integer(value) => value
      case Scalar(parts, _) =>
        parts.collectFirst { case reference: Reference => reference } match {
          case Some(reference) => fail(token, SubstitutionInArgument(reference.location))
          case None => text(parts)
        }
      case _ => fail(token, MissingValue(token.location))
    }

  private def text(parts: Vector[Part]): String = parts.collect { case Text(value) => value }.mkString
}

private[psi] object Parser {
  def parse(source: Source): Result[Ast.Document] = new Parser(new Lexer(source)).document()

  /** The bracket that closes a container, and where the one that opened it stands. */
  private final case class Closing(kind: Kind, opening: SourceLocation)

  /** The first error, and how many of the lexer's errors stand before it. */
  private final case class Stop(error: Option[PsiError], reported: Int) extends ControlThrowable
}
