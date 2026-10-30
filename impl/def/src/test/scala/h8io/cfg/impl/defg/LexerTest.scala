package h8io.cfg.impl.defg

import h8io.cfg.Id
import h8io.cfg.impl.defg.Token.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** What was read up to the end, and the errors found. */
private final case class Lexed[+A](read: List[A], errors: List[DefError])

class LexerTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  private def tokens(text: String, mode: Mode): Lexed[Token] = {
    val lexer = new Lexer(Source("s", text))
    val read = Iterator.continually(lexer.next(mode)).takeWhile(_.kind != End).toList
    Lexed(read, lexer.errors)
  }

  private def lex(text: String, mode: Mode = ValueMode): Lexed[Kind] = {
    val read = tokens(text, mode)
    Lexed(read.read.map(_.kind), read.errors)
  }

  private def key(text: String): Lexed[Kind] = lex(text, KeyMode)

  private def loc(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def scalar(parts: Part*): Scalar = Scalar(parts.toVector, quoted = false)
  private def quoted(parts: Part*): Scalar = Scalar(parts.toVector, quoted = true)
  private def plain(value: String): Scalar = scalar(Text(value))
  private def bare(value: String): Name = Name(value, quoted = false)

  private def ref(path: Id.Branch, line: Int, column: Int): Reference =
    Reference(path, optional = false, loc(line, column))

  private def at(key: String): Id.Key = Id.Key(key, Id.Root)

  "next" should "read punctuation in key mode" in {
    key("{ } [ ] ( ) , . = a: b := c") shouldBe
      Lexed(
        List(
          LeftBrace,
          RightBrace,
          LeftBracket,
          RightBracket,
          LeftParen,
          RightParen,
          Comma,
          Dot,
          Equals,
          bare("a"),
          Colon,
          bare("b"),
          Replace,
          bare("c")),
        Nil)
  }

  it should "read brackets and commas in value mode, and the rest as scalars" in {
    lex("{ } [ ] , ( ) . = :") shouldBe
      Lexed(
        List(LeftBrace, RightBrace, LeftBracket, RightBracket, Comma, plain("("), plain(")"), plain("."), plain("="),
          plain(":")),
        Nil)
  }

  it should "give the end of the source again once it is reached" in {
    val lexer = new Lexer(Source("s", "a"))
    lexer.next(ValueMode).kind shouldBe plain("a")
    lexer.next(ValueMode) shouldBe Token(End, loc(1, 2), startsLine = false)
    lexer.next(KeyMode) shouldBe Token(End, loc(1, 2), startsLine = false)
  }

  it should "read LF and CRLF as newlines, and record where each token stands" in {
    tokens("a b\n  c\r\nd", KeyMode) shouldBe
      Lexed(
        List(
          Token(bare("a"), loc(1, 1), startsLine = true),
          Token(bare("b"), loc(1, 3), startsLine = false),
          Token(Newline, loc(1, 4), startsLine = false),
          Token(bare("c"), loc(2, 3), startsLine = true),
          Token(Newline, loc(2, 4), startsLine = false),
          Token(bare("d"), loc(3, 1), startsLine = true)
        ),
        Nil
      )
  }

  it should "report a lone CR and read it as whitespace" in {
    lex("a\rb") shouldBe Lexed(List(plain("a"), plain("b")), List(LoneCarriageReturn(loc(1, 2))))
  }

  "Comments" should "run to the end of the line, leaving the newline" in {
    lex("a // x\rb\nc") shouldBe Lexed(List(plain("a"), Newline, plain("c")), List(LoneCarriageReturn(loc(1, 7))))
  }

  they should "nest, and count as whitespace when they span lines" in {
    lex("a /* x /* y\r */\n z */ b /**/ c") shouldBe
      Lexed(List(plain("a"), plain("b"), plain("c")), List(LoneCarriageReturn(loc(1, 12))))
  }

  they should "end the source when a block comment is unterminated" in {
    lex("a /* x /* y */ b") shouldBe Lexed(List(plain("a")), List(UnterminatedComment(loc(1, 3))))
  }

  they should "be ordinary characters inside an unquoted scalar" in {
    lex("http://host 1//x a/*b") shouldBe Lexed(List(plain("http://host"), plain("1//x"), plain("a/*b")), Nil)
  }

  they should "open where a value would start" in {
    lex("/*.log */ x") shouldBe Lexed(List(plain("x")), Nil)
  }

  "A stray end of a comment" should "be reported and skipped" in {
    lex("*/ a") shouldBe Lexed(List(plain("a")), List(StrayCommentEnd(loc(1, 1))))
  }

  "A doc comment" should "be kept with its text, nesting included" in {
    lex("/** doc /* x */ */ a") shouldBe Lexed(List(DocComment(" doc /* x */ "), plain("a")), Nil)
    key("/***/") shouldBe Lexed(List(DocComment("")), Nil)
  }

  it should "end the source when unterminated" in {
    key("/** doc") shouldBe Lexed(Nil, List(UnterminatedComment(loc(1, 1))))
  }

  "A bare key" should "match Id.SafeKeyPattern" in {
    key("a_b-1 ключ _x") shouldBe Lexed(List(bare("a_b-1"), bare("ключ"), bare("_x")), Nil)
  }

  it should "be reported when it does not match, and read anyway" in {
    key("1abc a@b x$y") shouldBe
      Lexed(List(bare("1abc"), bare("a@b"), bare("x$y")),
        List(InvalidKey(loc(1, 1), "1abc"), InvalidKey(loc(1, 6), "a@b"), InvalidKey(loc(1, 10), "x$y")))
  }

  it should "end where a comment opens" in {
    key("a// x\nb/* x */c") shouldBe Lexed(List(bare("a"), Newline, bare("b"), bare("c")), Nil)
  }

  it should "end at a quote" in {
    key("a\"b\"") shouldBe Lexed(List(bare("a"), quoted(Text("b"))), Nil)
    key("a`b`") shouldBe Lexed(List(bare("a"), Name("b", quoted = true)), Nil)
  }

  "A colon" should "be followed by whitespace or the end of the line" in {
    key("a: b\na:\tb\na:\na:") shouldBe
      Lexed(
        List(bare("a"), Colon, bare("b"), Newline, bare("a"), Colon, bare("b"), Newline, bare("a"), Colon, Newline,
          bare("a"), Colon),
        Nil)
  }

  it should "be reported otherwise, and so should :=" in {
    key("a:b c:=d") shouldBe
      Lexed(List(bare("a"), Colon, bare("b"), bare("c"), Replace, bare("d")),
        List(ColonWithoutSpace(loc(1, 2)), ColonWithoutSpace(loc(1, 6))))
  }

  "A name in backticks" should "be read in either mode, unescaped" in {
    val written = "`a \\\" \\` \\\\ \\$ \\b \\f \\n \\r \\t \\u0041 ${x}`"
    val name = Name("a \" ` \\ $ \b \f \n \r \t A ${x}", quoted = true)
    key(written) shouldBe Lexed(List(name), Nil)
    lex(written) shouldBe Lexed(List(name), Nil)
  }

  it should "report an escape outside the set and drop it" in {
    key("`a\\qb\\u12\\u00G1`") shouldBe
      Lexed(List(Name("ab1200G1", quoted = true)),
        List(InvalidEscape(loc(1, 3), "\\q"), InvalidEscape(loc(1, 6), "\\u"), InvalidEscape(loc(1, 10), "\\u")))
    key("`\\u12`") shouldBe Lexed(List(Name("12", quoted = true)), List(InvalidEscape(loc(1, 2), "\\u")))
  }

  it should "report a lone CR inside it" in {
    key("`a\rb`") shouldBe Lexed(List(Name("ab", quoted = true)), List(LoneCarriageReturn(loc(1, 3))))
  }

  it should "end the source when it is not closed on its line" in {
    key("`a\nb` c") shouldBe Lexed(Nil, List(UnterminatedName(loc(1, 1))))
    key("x `a\r\n") shouldBe Lexed(List(bare("x")), List(UnterminatedName(loc(1, 3))))
    key("`a\\") shouldBe Lexed(Nil, List(InvalidEscape(loc(1, 3), "\\"), UnterminatedName(loc(1, 1))))
    key("`a\\\n`") shouldBe Lexed(Nil, List(InvalidEscape(loc(1, 3), "\\"), UnterminatedName(loc(1, 1))))
  }

  "A quoted scalar" should "be read with its escapes and substitutions" in {
    lex(""""a \" \` \\ \$ \${x} A" "" "${a}" "x${a}y"""") shouldBe
      Lexed(
        List(
          quoted(Text("a \" ` \\ $ ${x} A")),
          quoted(),
          quoted(ref(at("a"), 1, 29)),
          quoted(Text("x"), ref(at("a"), 1, 37), Text("y"))),
        Nil)
  }

  it should "be read in key mode too, for the parser to reject" in {
    key("\"port\": 1") shouldBe Lexed(List(quoted(Text("port")), Colon, bare("1")), List(InvalidKey(loc(1, 9), "1")))
  }

  it should "report a lone CR inside it" in {
    lex("\"a\rb\"") shouldBe Lexed(List(quoted(Text("ab"))), List(LoneCarriageReturn(loc(1, 3))))
  }

  it should "end the source when it is not closed on its line" in {
    lex("a \"b\nc\"") shouldBe Lexed(List(plain("a")), List(UnterminatedString(loc(1, 3))))
    lex("\"b") shouldBe Lexed(Nil, List(UnterminatedString(loc(1, 1))))
    lex("\"b\r\n\"") shouldBe Lexed(Nil, List(UnterminatedString(loc(1, 1))))
  }

  "A triple-quoted string" should "end the source until it is supported" in {
    lex("a \"\"\"b\"\"\"") shouldBe Lexed(List(plain("a")), List(TripleQuoteNotSupported(loc(1, 3))))
  }

  "An unquoted scalar" should "run to whitespace, a comma, a closing brace or bracket" in {
    lex("a\tb,c}d]e\nf{g[h") shouldBe
      Lexed(
        List(plain("a"), plain("b"), Comma, plain("c"), RightBrace, plain("d"), RightBracket, plain("e"), Newline,
          plain("f{g[h")),
        Nil)
  }

  it should "be reported when it starts with an apostrophe" in {
    lex("'x' it's") shouldBe Lexed(List(plain("'x'"), plain("it's")), List(ApostropheAtStart(loc(1, 1))))
  }

  it should "report quotes inside it and keep them" in {
    lex("abc\"def\" a`b") shouldBe
      Lexed(List(plain("abc\"def\""), plain("a`b")),
        List(QuoteInUnquotedScalar(loc(1, 4)), QuoteInUnquotedScalar(loc(1, 8)), QuoteInUnquotedScalar(loc(1, 11))))
  }

  it should "hold substitutions" in {
    lex("jdbc:${host}/db ${a} ${?b} $x a\\b") shouldBe
      Lexed(
        List(
          scalar(Text("jdbc:"), ref(at("host"), 1, 6), Text("/db")),
          scalar(ref(at("a"), 1, 17)),
          scalar(Reference(at("b"), optional = true, loc(1, 22))),
          plain("$x"),
          plain("a\\b")
        ),
        Nil
      )
  }

  "A substitution" should "take a path as Id.path renders one" in {
    lex("${x.y[7].z} ${a[0][1]} ${server.`odd key`[0]} ${ключ[2147483647]}") shouldBe
      Lexed(
        List(
          scalar(ref(Id.Key("z", Id.Index(7, Id.Key("y", at("x")))), 1, 1)),
          scalar(ref(Id.Index(1, Id.Index(0, at("a"))), 1, 13)),
          scalar(ref(Id.Index(0, Id.Key("odd key", at("server"))), 1, 24)),
          scalar(ref(Id.Index(Int.MaxValue, at("ключ")), 1, 47))
        ),
        Nil
      )
  }

  it should "be reported at its start when its path cannot be read, and skipped" in {
    val broken =
      List("${}", "${[0]}", "${a.}", "${a[]}", "${a[07]}", "${a[-1]}", "${a[1x]}", "${a[0}", "${a[12345678901]}",
        "${a[2147483648]}", "${-a}")
    broken.foreach { written =>
      withClue(written) {
        lex(written + " x") shouldBe Lexed(List(scalar(), plain("x")), List(InvalidSubstitution(loc(1, 1))))
      }
    }
  }

  it should "report whitespace inside it where it stands" in {
    lex("${ a} x") shouldBe Lexed(List(scalar(), plain("x")), List(WhitespaceInSubstitution(loc(1, 3))))
    lex("\"${a b}c\"") shouldBe Lexed(List(quoted(Text("c"))), List(WhitespaceInSubstitution(loc(1, 5))))
  }

  it should "be skipped no further than the scalar around it or the end of the line" in {
    lex("\"${a b\" c") shouldBe Lexed(List(quoted(), plain("c")), List(WhitespaceInSubstitution(loc(1, 5))))
    lex("a${b c,d") shouldBe
      Lexed(List(scalar(Text("a")), plain("c"), Comma, plain("d")), List(WhitespaceInSubstitution(loc(1, 5))))
    lex("a${b,c") shouldBe Lexed(List(scalar(Text("a")), Comma, plain("c")), List(InvalidSubstitution(loc(1, 2))))
    lex("${a\nb") shouldBe Lexed(List(scalar(), Newline, plain("b")), List(InvalidSubstitution(loc(1, 1))))
    lex("${a\r\nb") shouldBe Lexed(List(scalar(), Newline, plain("b")), List(InvalidSubstitution(loc(1, 1))))
    lex("${a") shouldBe Lexed(List(scalar()), List(InvalidSubstitution(loc(1, 1))))
  }

  it should "end the source with an unterminated backticked segment" in {
    lex("a ${`b") shouldBe Lexed(List(plain("a")), List(UnterminatedName(loc(1, 5))))
    lex("\"${a.`b\"") shouldBe Lexed(Nil, List(UnterminatedName(loc(1, 6))))
  }

  "A tag" should "be read bare or in backticks" in {
    lex("@duration 5s @`tag:yaml.org,2002:int` 1") shouldBe
      Lexed(List(Tag("duration"), plain("5s"), Tag("tag:yaml.org,2002:int"), plain("1")), Nil)
  }

  it should "be reported without a name" in {
    lex("@ x @1") shouldBe
      Lexed(List(Tag(""), plain("x"), Tag(""), plain("1")), List(MissingTagName(loc(1, 1)), MissingTagName(loc(1, 5))))
  }

  it should "be reported as reserved after @-" in {
    lex("@-a x") shouldBe Lexed(List(Tag("a"), plain("x")), List(ReservedTagRemoval(loc(1, 1))))
  }

  it should "be reported with arguments" in {
    lex("@a(x) @`b`(") shouldBe
      Lexed(List(Tag("a"), plain("(x)"), Tag("b"), plain("(")), List(TagArguments(loc(1, 1)), TagArguments(loc(1, 7))))
  }

  it should "end the source when its backticks are not closed" in {
    lex("@`a") shouldBe Lexed(Nil, List(UnterminatedName(loc(1, 2))))
  }

  it should "be part of a scalar inside it" in {
    lex("admin@host") shouldBe Lexed(List(plain("admin@host")), Nil)
  }

  "A directive" should "be read in either mode, bare or in backticks" in {
    key("#include(file = \"x\")") shouldBe
      Lexed(List(Directive("include"), LeftParen, bare("file"), Equals, quoted(Text("x")), RightParen), Nil)
    lex("#`my.directive`(") shouldBe Lexed(List(Directive("my.directive"), plain("(")), Nil)
  }

  it should "be reported without a name" in {
    lex("# text") shouldBe Lexed(List(Directive(""), plain("text")), List(MissingDirectiveName(loc(1, 1))))
    key("#-x") shouldBe
      Lexed(List(Directive(""), bare("-x")), List(MissingDirectiveName(loc(1, 1)), InvalidKey(loc(1, 2), "-x")))
  }

  it should "end the source when its backticks are not closed" in {
    key("#`a") shouldBe Lexed(Nil, List(UnterminatedName(loc(1, 2))))
  }

  "Id.path" should "lex in key mode to a single key with the same name, whatever the name" in
    forAll { (name: String) =>
      key(Id.Key(name, Id.Root).path) shouldBe
        Lexed(List(Name(name, quoted = !Id.SafeKeyPattern.matcher(name).matches())), Nil)
    }
}
