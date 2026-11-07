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

  it should "read a tab between tokens as whitespace" in {
    key("a:\tb\t,\tc") shouldBe Lexed(List(bare("a"), Colon, bare("b"), Comma, bare("c")), Nil)
  }

  "A tab in indentation" should "be reported at the first one, once per line" in {
    key("\ta\n  \t\t b\nc\t d") shouldBe
      Lexed(List(bare("a"), Newline, bare("b"), Newline, bare("c"), bare("d")),
        List(TabInIndentation(loc(1, 1)), TabInIndentation(loc(2, 3))))
  }

  it should "be reported before the errors found after it on its line" in {
    lex("\t\u00A0a") shouldBe
      Lexed(List(plain("a")), List(TabInIndentation(loc(1, 1)), ForeignWhitespace(loc(1, 2), '\u00A0')))
  }

  it should "not be reported on a blank or comment-only line" in {
    lex("\t\n\t// x\n\t/* x */\r\n\t/* x\n */ a\n\t") shouldBe
      Lexed(List(Newline, Newline, Newline, plain("a"), Newline), Nil)
  }

  "Whitespace other than space and tab" should "be reported and read as whitespace" in {
    val foreign = List('\u00A0', '\u2007', '\u202F', '\u3000', '\uFEFF')
    foreign.foreach { c =>
      withClue(c.toInt.toHexString) {
        lex(s"a${c}b") shouldBe Lexed(List(plain("a"), plain("b")), List(ForeignWhitespace(loc(1, 2), c)))
        key(s"a:${c}b") shouldBe Lexed(List(bare("a"), Colon, bare("b")), List(ForeignWhitespace(loc(1, 3), c)))
      }
    }
  }

  it should "be content inside quotes and comments" in {
    lex("\"a\u00A0b\" // \u00A0\n/* \u3000 */ x") shouldBe
      Lexed(List(quoted(Text("a\u00A0b")), Newline, plain("x")), Nil)
    key("`a\u00A0b`") shouldBe Lexed(List(Name("a\u00A0b", quoted = true)), Nil)
  }

  "NEL, U+2028 and U+2029" should "be reported anywhere, read as whitespace outside quotes and dropped inside" in
    List('\u0085', '\u2028', '\u2029').foreach { c =>
      withClue(c.toInt.toHexString) {
        lex(s"a${c}b") shouldBe Lexed(List(plain("a"), plain("b")), List(ForeignLineBreak(loc(1, 2), c)))
        lex("\"a" + c.toString + "b\" `c" + c.toString + "d`") shouldBe
          Lexed(List(quoted(Text("ab")), Name("cd", quoted = true)),
            List(ForeignLineBreak(loc(1, 3), c), ForeignLineBreak(loc(1, 9), c)))
        lex(s"// x${c}y\n/* x${c}y */ z") shouldBe
          Lexed(List(Newline, plain("z")), List(ForeignLineBreak(loc(1, 5), c), ForeignLineBreak(loc(2, 5), c)))
        lex(s"$${a${c}b} x") shouldBe
          Lexed(List(scalar(), plain("b"), RightBrace, plain("x")),
            List(InvalidSubstitution(loc(1, 1)), ForeignLineBreak(loc(1, 4), c)))
      }
    }

  they should "be written as an escape inside quotes" in {
    lex("\"\\u2028\"") shouldBe Lexed(List(quoted(Text("\u2028"))), Nil)
  }

  "Control characters other than tab" should
    "be reported anywhere, read as whitespace outside quotes and dropped inside" in
    List('\u0000', '\u0001', '\u0008', '\f', '\u000B', '\u001B', '\u001F', '\u007F', '\u0080', '\u009F').foreach { c =>
      withClue(c.toInt.toHexString) {
        lex(s"a${c}b") shouldBe Lexed(List(plain("a"), plain("b")), List(ControlCharacter(loc(1, 2), c)))
        key(s"a:${c}b") shouldBe Lexed(List(bare("a"), Colon, bare("b")), List(ControlCharacter(loc(1, 3), c)))
        lex("\"a" + c.toString + "b\" `c" + c.toString + "d`") shouldBe
          Lexed(List(quoted(Text("ab")), Name("cd", quoted = true)),
            List(ControlCharacter(loc(1, 3), c), ControlCharacter(loc(1, 9), c)))
        lex("'a" + c.toString + "b'") shouldBe Lexed(List(quoted(Text("ab"))), List(ControlCharacter(loc(1, 3), c)))
        lex("\"\"\"<" + c.toString + "\n  a" + c.toString + "b\n  \"\"\"") shouldBe
          Lexed(List(quoted(Text("ab"))), List(ControlCharacter(loc(1, 5), c), ControlCharacter(loc(2, 4), c)))
        lex("'''\n  a" + c.toString + "b\n  '''") shouldBe
          Lexed(List(quoted(Text("ab"))), List(ControlCharacter(loc(2, 4), c)))
        lex(s"// x${c}y\n/* x${c}y */ z") shouldBe
          Lexed(List(Newline, plain("z")), List(ControlCharacter(loc(1, 5), c), ControlCharacter(loc(2, 5), c)))
        lex(s"$${a${c}b} x") shouldBe
          Lexed(List(scalar(), plain("b"), RightBrace, plain("x")),
            List(InvalidSubstitution(loc(1, 1)), ControlCharacter(loc(1, 4), c)))
      }
    }

  they should "be written as an escape inside quotes and backticks" in {
    lex("\"\\u0001\\f\"") shouldBe Lexed(List(quoted(Text("\u0001\f"))), Nil)
    key("`\\u0000`") shouldBe Lexed(List(Name("\u0000", quoted = true)), Nil)
  }

  "A tab" should "be content inside quotes" in {
    lex("\"a\tb\" 'c\td'") shouldBe Lexed(List(quoted(Text("a\tb")), quoted(Text("c\td"))), Nil)
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

  "A raw string" should "take \\' and \\\\ as its only escapes, and ${ as text" in {
    lex("""'a\'b\\c\d${x}"' ''""") shouldBe Lexed(List(quoted(Text("a'b\\c\\d${x}\"")), quoted()), Nil)
  }

  it should "be read in key mode too, for the parser to reject" in {
    key("'a': b") shouldBe Lexed(List(quoted(Text("a")), Colon, bare("b")), Nil)
  }

  it should "not open inside an unquoted scalar" in {
    lex("it's") shouldBe Lexed(List(plain("it's")), Nil)
  }

  it should "report a lone CR inside it" in {
    lex("'a\rb'") shouldBe Lexed(List(quoted(Text("ab"))), List(LoneCarriageReturn(loc(1, 3))))
  }

  it should "end the source when it is not closed on its line" in {
    lex("a 'b\nc'") shouldBe Lexed(List(plain("a")), List(UnterminatedString(loc(1, 3))))
    lex("'b\\'") shouldBe Lexed(Nil, List(UnterminatedString(loc(1, 1))))
  }

  /** `text` with each "```" standing for a triple quote, which a Scala string cannot hold unescaped. */
  private def tq(text: String): String = text.replace("```", "\"\"\"")

  private def block(text: String): Lexed[Kind] = lex(tq(text))

  private def value(text: String, value: String, errors: DefError*): Unit =
    block(text) shouldBe Lexed(List(quoted(Text(value))), errors.toList)

  "A triple-quoted string" should "always be a block, what follows its delimiter on the line its markers" in {
    value("```abc```\n  d\n  ```", "d", InvalidMarker(loc(1, 7)))
    block("x ```a b\nc```") shouldBe
      Lexed(List(plain("x"), quoted(Text("c"))), List(UnknownMarker(loc(1, 6), "a"), UnknownMarker(loc(1, 8), "b")))
    block("```a b") shouldBe
      Lexed(Nil, List(UnknownMarker(loc(1, 4), "a"), UnknownMarker(loc(1, 6), "b"), UnterminatedString(loc(1, 1))))
    value("```a\\\n  b\n  ```", "b", InvalidMarker(loc(1, 5)))
  }

  it should "close at the last three quotes of a longer run" in {
    value("```\n  say \"hi\"\"\"\"", "say \"hi\"")
    value("```\n  a\"\"\"\"\"\"\"\"", "a\"\"\"\"\"")
  }

  "A triple-quoted block" should "drop the first and the last newline and the common indent" in {
    value("```\n  abc\n  def\n\n  ```", "abc\ndef\n")
    value("```\n    Welcome.\n      Indented line.\n    ```", "Welcome.\n  Indented line.")
  }

  it should "keep an indent left of which the closing delimiter stands" in {
    value("```\n    abc\n    def\n  ```", "  abc\n  def")
    value("```\n    abc\n```", "    abc")
  }

  it should "count the closing delimiter's line only when it stands alone" in {
    value("```\n    abc\n  def```", "  abc\ndef")
    value("```\n    abc```", "abc")
  }

  it should "leave blank lines out of the common indent" in
    value("```\n    a\n\n  \n    b\n    ```", "a\n\n\nb")

  it should "be empty with no lines" in {
    block("```\n```") shouldBe Lexed(List(quoted()), Nil)
    block("```\n  ```") shouldBe Lexed(List(quoted()), Nil)
  }

  it should "turn CRLF into LF" in
    value("```\r\n  a\r\n  b\r\n  ```", "a\nb")

  it should "strip trailing whitespace before its escapes are interpreted" in
    value("```\n  a  \n  b\\t \n  \\u0020c\n  \\td\n  ```", "a\nb\t\n c\n\td")

  it should "strip all of the indent under <" in
    value("```<\n    a\n  b\n    \\u0020\\u0020c\n  ```", "a\nb\n  c")

  it should "keep the indent under >" in
    value("```>\n  a\n    b\n  ```", "  a\n    b")

  it should "fold under + as YAML does under >" in {
    value("```+\n  a\n  b\n\n  c\n    d\n  e\n  f\n  ```", "a b\nc\n  d\ne f")
    value("```+\n  a\n\n\n  b\\nc\n  d\n  ```", "a\n\nb\nc d")
    value("```+\n\n  a\n\n  ```", "\na\n")
    value("```+\n\n\n```", "\n")
  }

  it should "strip the opening line's indent and an explicit width" in {
    value("  ```width(n = 2)\n      abc\n\n    ```", "  abc\n")
    value("```width(n=1)\n  a\n   b\n```", " a\n  b")
    value("  ```width(2)\n      abc\n\n    ```", "  abc\n")
    value("  ```width( n = 2 ) +\n    a\n    b\n      c\n  ```", "a b\n  c")
  }

  it should "report a line short of an explicit width" in
    value("  ```width(n = 2)\n    a\n   b\n\tc\n  ```", "a\nb\nc", InsufficientIndent(loc(3, 4)),
      InsufficientIndent(loc(4, 1)))

  it should "apply its markers left to right, Java's rule first when none sets the indent" in {
    value("```< +\n    a\n  b\n  ```", "a b")
    value("```+ <\n    a\n  b\n  ```", "a\nb")
    value("```> +\n  a\n  b\n```", "  a\n  b")
    value("```> +\na\nb\n```", "a b")
    value("```+ +\n  a\n\n\n\n  b\n  ```", "a\n\nb")
    value("```< \t +\n  a\n  b\n  ```", "a b")
    value("```<()\n    a\n  ```", "a")
  }

  it should "report a second marker that sets the indent, and read the block as with no marker" in {
    value("```< >\n    a\n  ```", "  a", ConflictingIndent(loc(1, 6)))
    value("```width(n = 2) <\n    a\n  ```", "  a", ConflictingIndent(loc(1, 17)))
  }

  it should "report an unknown marker, and read the block as with none" in
    List("=", "|", ">>", "++", "<>", "<+", "-", "abc", "a-b", "x1").foreach { marker =>
      withClue(marker) {
        value(s"```$marker\n    a\n  ```", "  a", UnknownMarker(loc(1, 4), marker))
      }
    }

  it should "report markers that cannot be read where they stop fitting, and read the block as with none" in
    List(
      "0" -> 4,
      "12" -> 4,
      "<2" -> 5,
      "+2" -> 5,
      "<x" -> 5,
      "//" -> 4,
      "a\"b" -> 5,
      "< (x = 1)" -> 6,
      "<(" -> 6,
      "<(x =)" -> 9,
      "<(x = 1" -> 11,
      "<(x = 1 2)" -> 12,
      "<(x = 1,)" -> 12,
      "<(x = 01)" -> 11,
      "<(x = -)" -> 11,
      "<(x = \"a)" -> 10,
      "<(x = 'a)" -> 10
    ).foreach { case (marker, column) =>
      withClue(marker) {
        value(s"```$marker\n    a\n  ```", "  a", InvalidMarker(loc(1, column)))
      }
    }

  it should "report the arguments a marker does not take, and read the block as with no marker" in {
    value("```<(x = 1)\n    a\n  ```", "  a", UnknownArgument(loc(1, 6), "x"))
    value("```<(x = abc, y = -5, z = 0)\n    a\n  ```", "  a", UnknownArgument(loc(1, 6), "x"),
      UnknownArgument(loc(1, 15), "y"), UnknownArgument(loc(1, 23), "z"))
    value("```width\n    a\n  ```", "  a", MissingArgument(loc(1, 4), "n"))
    value("```width(n = 1, n = 2)\n    a\n  ```", "  a", DuplicateArgument(loc(1, 17), "n"))
    value("```width(2, n = 3)\n    a\n  ```", "  a", DuplicateArgument(loc(1, 13), "n"))
    value("```<(x)\n    a\n  ```", "  a", ExtraArgument(loc(1, 6)))
    value("```width(2, 3)\n    a\n  ```", "  a", ExtraArgument(loc(1, 13)))
    value("```width(n = 2, 3)\n    a\n  ```", "  a", PositionalAfterNamed(loc(1, 17)))
    value("```width(two)\n    a\n  ```", "  a", InvalidArgument(loc(1, 10), "n"))
    List("0", "-1", "\"2\"", "'2'", "two", "2147483648").foreach { width =>
      withClue(width) {
        value(s"```width(n = $width)\n    a\n  ```", "  a", InvalidArgument(loc(1, 14), "n"))
      }
    }
  }

  it should "read strings in arguments with the escapes of their quotes, and no substitutions" in {
    value("```<(x = \"\\t\")\n    a\n  ```", "  a", UnknownArgument(loc(1, 6), "x"))
    value("```<(x = 'a\\'b\\\\c\\d')\n    a\n  ```", "  a", UnknownArgument(loc(1, 6), "x"))
    value("```<(x = \"a\\qb\")\n    a\n  ```", "  a", InvalidEscape(loc(1, 12), "\\q"),
      UnknownArgument(loc(1, 6), "x"))
    value("```<(x = \"$" + "{y}\")\n    a\n  ```", "  a", SubstitutionInArgument(loc(1, 11)),
      UnknownArgument(loc(1, 6), "x"))
    value("```<(x = \"a\\\n    a\n  ```", "  a", InvalidEscape(loc(1, 12), "\\"), InvalidMarker(loc(1, 10)))
  }

  it should "report whitespace after the delimiter or the markers, and apply the markers" in {
    value("```  \n  a\n  ```", "a", WhitespaceAfterDelimiter(loc(1, 4)))
    value("```<\t\n    a\n  b\n  ```", "a\nb", WhitespaceAfterDelimiter(loc(1, 5)))
    value("``` >\n    a\n  ```", "    a", WhitespaceAfterDelimiter(loc(1, 4)))
  }

  it should "report a line break of another kind after the delimiter, and leave it out of the marker" in
    value("```<\u2028\n  a\n  ```", "a", ForeignLineBreak(loc(1, 5), '\u2028'))

  it should "report indents of which neither is a prefix of the other" in {
    value("```\n  a\n\tb\n  ```", "  a\n\tb", InconsistentIndent(loc(3, 1)))
    value("```\n  a\n    b\n  \tc\n  ```", "a\n  b\n\tc", InconsistentIndent(loc(4, 3)))
    value("```\n    a\n  b\n  ```", "  a\nb")
  }

  it should "keep quotes short of a run of three" in
    value("```\n  say \"hi\" \"\"\n  a\"\"\"\"", "say \"hi\" \"\"\na\"")

  it should "hold substitutions and escapes" in {
    block("```\n  ${a}\n  x${b} \\\"\"\" \\${c}\n  ```") shouldBe
      Lexed(List(quoted(ref(at("a"), 2, 3), Text("\nx"), ref(at("b"), 3, 4), Text(" \"\"\" ${c}"))), Nil)
  }

  it should "report an escape outside the set, and a lone CR" in
    value("```\n  a\\qb\rc\\\n  ```", "abc", InvalidEscape(loc(2, 4), "\\q"), LoneCarriageReturn(loc(2, 7)),
      InvalidEscape(loc(2, 9), "\\"))

  it should "be followed by the next token" in {
    block("```\n  a\n  ``` b") shouldBe Lexed(List(quoted(Text("a")), plain("b")), Nil)
  }

  it should "end the source when it is not closed" in {
    block("x ```\n  a\n") shouldBe Lexed(List(plain("x")), List(UnterminatedString(loc(1, 3))))
    block("```") shouldBe Lexed(Nil, List(UnterminatedString(loc(1, 1))))
    block("```<") shouldBe Lexed(Nil, List(UnterminatedString(loc(1, 1))))
    block("```\n  ${`a\n```") shouldBe Lexed(Nil, List(UnterminatedName(loc(2, 5))))
  }

  "A raw triple-quoted string" should "take the layout of a double-quoted one, and the escapes of '…'" in {
    lex("'''\n  it's\n  a''''") shouldBe Lexed(List(quoted(Text("it's\na'"))), Nil)
    lex("'''\n  ${x} \\d\\\\ \\''' \\t\n  \"\"\"\n  '''") shouldBe
      Lexed(List(quoted(Text("${x} \\d\\ ''' \\t\n\"\"\""))), Nil)
    lex("'''+\n  a\n  b\rc\n  '''") shouldBe Lexed(List(quoted(Text("a bc"))), List(LoneCarriageReturn(loc(3, 4))))
  }

  it should "end the source when it is not closed" in {
    lex("'''a b") shouldBe
      Lexed(Nil, List(UnknownMarker(loc(1, 4), "a"), UnknownMarker(loc(1, 6), "b"), UnterminatedString(loc(1, 1))))
    lex("'''\n  a") shouldBe Lexed(Nil, List(UnterminatedString(loc(1, 1))))
  }

  "An unquoted scalar" should "run to whitespace, a comma, a closing brace or bracket" in {
    lex("a\tb,c}d]e\nf{g[h") shouldBe
      Lexed(
        List(plain("a"), plain("b"), Comma, plain("c"), RightBrace, plain("d"), RightBracket, plain("e"), Newline,
          plain("f{g[h")),
        Nil)
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

  it should "leave a lone CR inside it to be reported as one" in {
    lex("${a\rb} x") shouldBe
      Lexed(List(scalar(), plain("b"), RightBrace, plain("x")),
        List(InvalidSubstitution(loc(1, 1)), LoneCarriageReturn(loc(1, 4))))
    lex("\"${a\rb}\"") shouldBe
      Lexed(List(quoted(Text("b}"))), List(InvalidSubstitution(loc(1, 2)), LoneCarriageReturn(loc(1, 5))))
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
