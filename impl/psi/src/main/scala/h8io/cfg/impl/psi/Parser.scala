package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Parser.*
import h8io.cfg.impl.psi.Token.*

import scala.annotation.tailrec
import scala.util.control.ControlThrowable

/** Reads the syntax tree of one source by recursive descent over its tokens (`format.md` §3, §4).
  *
  * The lexer's errors are collected as it reports them, and it goes on past them. The parser itself stops at its first
  * error for now; recovery at the synchronisation points of §11 is step 5 of the plan.
  */
private[psi] final class Parser(lexer: Lexer) {

  /** How many errors the lexer had reported when the last token taken through [[next]] started, and once it was read.
    */
  private var reportedBeforeToken = 0
  private var reportedAfter = 0

  /** Whether the last value read was an unquoted scalar, which a second word after it makes `a: foo bar`. */
  private var unquoted = false

  def document(): Result[Ast.Document] =
    try {
      val statements = items(KeyMode, None)(field)
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
    reportedBeforeToken = lexer.errorCountBeforeToken
    reportedAfter = lexer.errorCount
    token
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

  /** The fields of a block or the elements of a sequence, separated by `,` and newlines (§3): a run of newlines with at
    * most one `,` is one separator, and a `,` with no item before it in its container is an empty element.
    *
    * @param closing
    *   the bracket that ends the container, with the location of the one that opened it; none for a document, which the
    *   end of the source ends
    */
  private def items[A](mode: Mode, closing: Option[Closing])(item: (Token, Option[String]) => A): Vector[A] = {
    val read = Vector.newBuilder[A]
    var separated = true
    var afterItem = false
    var token = next(mode)
    var done = false
    while (!done)
      token.kind match {
        case Newline =>
          separated = true
          token = next(mode)
        case Comma =>
          if (!afterItem) fail(token, EmptyElement(token.location))
          afterItem = false
          separated = true
          token = next(mode)
        case End =>
          closing match {
            case Some(Closing(_, opening)) => fail(token, UnclosedBracket(opening))
            case None => done = true
          }
        case RightBrace | RightBracket =>
          if (closing.exists(_.kind == token.kind)) done = true
          else fail(token, UnmatchedBracket(token.location))
        case kind =>
          val (first, doc) = kind match {
            case DocComment(text) =>
              if (!separated) fail(token, MisplacedDocComment(token.location))
              (documented(token, mode), Some(text))
            case _ =>
              if (!separated) afterValue(token, mode)
              (token, None)
          }
          read += item(first, doc)
          afterItem = true
          separated = false
          token = next(mode)
      }
    read.result()
  }

  /** The first token of the item `doc` stands before, on its line or a later one; anything else after it makes it a
    * misplaced doc comment.
    */
  @tailrec private def documented(doc: Token, mode: Mode): Token = {
    val token = next(mode)
    token.kind match {
      case Newline => documented(doc, mode)
      case Comma | RightBrace | RightBracket | End | DocComment(_) => fail(token, MisplacedDocComment(doc.location))
      case _ => token
    }
  }

  /** Something other than a separator after a value: in a block, a second field on the line, or a second word of an
    * unquoted scalar, told apart by whether a field starts there.
    *
    * In a block the token was read in key mode, a guess. Where the guess shaped what the lexer said about it — a bare
    * key it found invalid, a `:` with no space after it — the token's own errors are left out, since it may be a word
    * of a value. Everything else is kept: errors in the whitespace and comments before it, and in a token that reads
    * the same in either mode, such as a quoted one.
    */
  private def afterValue(token: Token, mode: Mode): Nothing = {
    val error = token.kind match {
      case Name(_, _) if mode == KeyMode && startsField => MissingSeparator(token.location)
      case Name(_, _) | Scalar(_, _) if mode == KeyMode && unquoted => SpaceInValue(token.location)
      case _ => MissingSeparator(token.location)
    }
    val guessed = mode == KeyMode &&
      (token.kind match {
        case Name(_, false) | Colon | Replace => true
        case _ => false
      })
    fail(token, error, if (guessed) reportedBeforeToken else reportedAfter)
  }

  private def startsField: Boolean =
    lexer.next(KeyMode).kind match {
      case Colon | Replace | Dot | LeftBrace => true
      case _ => false
    }

  private def field(first: Token, doc: Option[String]): Ast.Field =
    first.kind match {
      case Name(name, _) => keys(Ast.Key(name, first.location), Nil, doc)
      case Tag(_) => fail(first, TagBeforeKey(first.location))
      case Scalar(_, _) => fail(first, QuotedKey(first.location))
      case Directive(_) => fail(first, NotSupportedYet(first.location, "directives"))
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
      case Scalar(parts, quoted) =>
        parts.collectFirst { case reference: Reference => reference } match {
          case Some(reference) => fail(token, NotSupportedYet(reference.location, "substitutions"))
          case None =>
            val text = parts.collect { case Text(value) => value }.mkString
            unquoted = !quoted
            if (!quoted && text == "null") Ast.Null(tags, token.location) else Ast.Scalar(text, tags, token.location)
        }
      case LeftBrace => block(token, tags)
      case LeftBracket =>
        val elements = items(ValueMode, Some(Closing(RightBracket, token.location)))(element)
        unquoted = false
        Ast.Sequence(elements, tags, token.location)
      case Name(_, _) => fail(token, BacktickedValue(token.location))
      case Directive(_) => fail(token, NotSupportedYet(token.location, "directives"))
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

  private def block(open: Token, tags: Set[String]): Ast.Block = {
    val statements = items(KeyMode, Some(Closing(RightBrace, open.location)))(field)
    unquoted = false
    Ast.Block(statements, tags, open.location)
  }

  private def element(first: Token, doc: Option[String]): Ast.Element = Ast.Element(value(first, Set.empty), doc)
}

private[psi] object Parser {
  def parse(source: Source): Result[Ast.Document] = new Parser(new Lexer(source)).document()

  /** The bracket that closes a container, and where the one that opened it stands. */
  private final case class Closing(kind: Kind, opening: SourceLocation)

  /** The first error, and how many of the lexer's errors stand before it. */
  private final case class Stop(error: Option[PsiError], reported: Int) extends ControlThrowable
}
