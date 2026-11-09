package h8io.cfg.impl.psi

import h8io.cfg.impl.psi.Ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFlatSpec with Matchers {
  private def parse(text: String): Result[Document] = Parser.parse(Source("s", text))

  /** The statements of a source that parses with no error. */
  private def statements(text: String): Vector[Statement] = {
    val parsed = parse(text)
    parsed.errors shouldBe Nil
    parsed.value.statements
  }

  /** The errors of a source the parser stops on, which leaves an empty document. */
  private def errors(text: String): List[PsiError] = {
    val parsed = parse(text)
    parsed.value shouldBe Document(Vector.empty)
    parsed.errors
  }

  private def loc(line: Int, column: Int): SourceLocation = SourceLocation("s", line, column)

  private def k(name: String, line: Int, column: Int): Key = Key(name, loc(line, column))

  private def field(key: Key, value: Value): Field = Field(::(key, Nil), Operator.Merge, value, None)

  private def str(value: String, line: Int, column: Int, tags: String*): Scalar =
    Scalar(value, tags.toSet, loc(line, column))

  "parse" should "read an empty source, and one of blank lines and comments, as no statement" in {
    statements("") shouldBe Vector.empty
    statements("\n// c\n\n/* d */\n") shouldBe Vector.empty
  }

  it should "read a field with `:` as a merge and with `:=` as a replacement" in {
    statements("a: 1\nb := 2") shouldBe
      Vector(field(k("a", 1, 1), str("1", 1, 4)), Field(::(k("b", 2, 1), Nil), Operator.Replace, str("2", 2, 6), None))
  }

  it should "split a dotted key into its keys, a backticked one indivisible" in {
    statements("a.`b.c`.d: x") shouldBe
      Vector(Field(::(k("a", 1, 1), List(k("b.c", 1, 3), k("d", 1, 9))), Operator.Merge, str("x", 1, 12), None))
  }

  it should "read the block form as a merge with a block, as `key: { … }` is" in {
    statements("a { b: 1 }\nc: {}\nd := { }\ne{}") shouldBe
      Vector(
        field(k("a", 1, 1), Block(Vector(field(k("b", 1, 5), str("1", 1, 8))), Set.empty, loc(1, 3))),
        field(k("c", 2, 1), Block(Vector.empty, Set.empty, loc(2, 4))),
        Field(::(k("d", 3, 1), Nil), Operator.Replace, Block(Vector.empty, Set.empty, loc(3, 6)), None),
        field(k("e", 4, 1), Block(Vector.empty, Set.empty, loc(4, 2)))
      )
  }

  it should "read sequences of any values, newlines after the opening bracket and before the closing one no element" in {
    statements("a: [1, \"two\", [], {x: 4}]\nb: [\n  1\n  2,\n]") shouldBe
      Vector(
        field(
          k("a", 1, 1),
          Sequence(
            Vector(
              Element(str("1", 1, 5), None),
              Element(str("two", 1, 8), None),
              Element(Sequence(Vector.empty, Set.empty, loc(1, 15)), None),
              Element(Block(Vector(field(k("x", 1, 20), str("4", 1, 23))), Set.empty, loc(1, 19)), None)
            ),
            Set.empty,
            loc(1, 4)
          )
        ),
        field(
          k("b", 2, 1),
          Sequence(Vector(Element(str("1", 3, 3), None), Element(str("2", 4, 3), None)), Set.empty, loc(2, 4)))
      )
  }

  it should "take a run of newlines with one `,` among them as one separator, wherever the `,` stands" in {
    val expected = Vector(field(k("a", 1, 1), str("1", 1, 4)), field(k("b", 3, 3), str("2", 3, 6)))
    statements("a: 1,\n\n  b: 2") shouldBe expected
    statements("a: 1\n\n, b: 2") shouldBe expected
    statements("a: 1\n,\n  b: 2,\n") shouldBe expected
    statements("a: 1, b: 2") shouldBe Vector(field(k("a", 1, 1), str("1", 1, 4)), field(k("b", 1, 7), str("2", 1, 10)))
  }

  it should "read tags as a set, on any value" in {
    statements("a: @x @y @x 1\nb: @m {}\nc: @s [@e 1]\nd: @n null") shouldBe
      Vector(
        field(k("a", 1, 1), str("1", 1, 13, "x", "y")),
        field(k("b", 2, 1), Block(Vector.empty, Set("m"), loc(2, 7))),
        field(k("c", 3, 1), Sequence(Vector(Element(str("1", 3, 11, "e"), None)), Set("s"), loc(3, 7))),
        field(k("d", 4, 1), Null(Set("n"), loc(4, 7)))
      )
  }

  it should "read an unquoted null as null in value position only" in {
    statements("a: null\nb: \"null\"\nc: 'null'\nnull: nullx") shouldBe
      Vector(
        field(k("a", 1, 1), Null(Set.empty, loc(1, 4))),
        field(k("b", 2, 1), str("null", 2, 4)),
        field(k("c", 3, 1), str("null", 3, 4)),
        field(k("null", 4, 1), str("nullx", 4, 7))
      )
  }

  it should "read scalars of every kind" in {
    statements("a: \"x y\"\nb: 'z'\nc: \"\"\"\n  t\n  \"\"\"\nd: \"\"") shouldBe
      Vector(
        field(k("a", 1, 1), str("x y", 1, 4)),
        field(k("b", 2, 1), str("z", 2, 4)),
        field(k("c", 3, 1), str("t", 3, 4)),
        field(k("d", 6, 1), str("", 6, 4))
      )
  }

  it should "keep a doc comment on the field or element it stands before" in {
    statements("/** f */ a: [/** e */ 1]\n/** g */\n\nb { /** h */\n c: 2 }") shouldBe
      Vector(
        Field(
          ::(k("a", 1, 10), Nil),
          Operator.Merge,
          Sequence(Vector(Element(str("1", 1, 23), Some(" e "))), Set.empty, loc(1, 13)),
          Some(" f ")),
        Field(
          ::(k("b", 4, 1), Nil),
          Operator.Merge,
          Block(
            Vector(Field(::(k("c", 5, 2), Nil), Operator.Merge, str("2", 5, 5), Some(" h "))), Set.empty, loc(4, 3)),
          Some(" g "))
      )
  }

  it should "go on past the lexer's errors, and report them" in {
    parse("a:b\n1a: x") shouldBe
      Result(
        Document(Vector(field(k("a", 1, 1), str("b", 1, 3)), field(k("1a", 2, 1), str("x", 2, 5)))),
        List(ColonWithoutSpace(loc(1, 2)), InvalidKey(loc(2, 1), "1a")))
  }

  it should "stop at its first error, keeping the lexer's errors before it" in {
    errors("1a: x\nb = 2\nc d") shouldBe List(InvalidKey(loc(1, 1), "1a"), ExpectedColon(loc(2, 3)))
  }

  "An empty element" should "be reported at the `,` with nothing before it" in {
    errors("a: [x,,y]") shouldBe List(EmptyElement(loc(1, 7)))
    errors("a: [, x]") shouldBe List(EmptyElement(loc(1, 5)))
    errors("a: [x,\n,y]") shouldBe List(EmptyElement(loc(2, 1)))
    errors(", a: 1") shouldBe List(EmptyElement(loc(1, 1)))
    errors("a: 1,, b: 2") shouldBe List(EmptyElement(loc(1, 6)))
    errors("a { , }") shouldBe List(EmptyElement(loc(1, 5)))
  }

  "A bracket" should "be reported where it is opened when the source ends first" in {
    errors("a { b: 1") shouldBe List(UnclosedBracket(loc(1, 3)))
    errors("a: [1,\n") shouldBe List(UnclosedBracket(loc(1, 4)))
  }

  it should "be reported where it closes nothing, or the wrong bracket" in {
    errors("a: 1\n}") shouldBe List(UnmatchedBracket(loc(2, 1)))
    errors("]") shouldBe List(UnmatchedBracket(loc(1, 1)))
    errors("a: [1}") shouldBe List(UnmatchedBracket(loc(1, 6)))
    errors("a { ] }") shouldBe List(UnmatchedBracket(loc(1, 5)))
  }

  "A doc comment" should "be reported anywhere but before a field or an element" in {
    errors("a: 1 /** d */\nb: 2") shouldBe List(MisplacedDocComment(loc(1, 6)))
    errors("a: 1\n/** d */") shouldBe List(MisplacedDocComment(loc(2, 1)))
    errors("/** d */ /** e */ a: 1") shouldBe List(MisplacedDocComment(loc(1, 1)))
    errors("/** d */, a: 1") shouldBe List(MisplacedDocComment(loc(1, 1)))
    errors("a { /** d */ }") shouldBe List(MisplacedDocComment(loc(1, 5)))
    errors("a: [1, /** d */\n]") shouldBe List(MisplacedDocComment(loc(1, 8)))
    errors("a /** d */: 1") shouldBe List(MisplacedDocComment(loc(1, 3)))
    errors("a: /** d */ 1") shouldBe List(MisplacedDocComment(loc(1, 4)))
    errors("a: @t /** d */ 1") shouldBe List(MisplacedDocComment(loc(1, 7)))
  }

  "A key" should "be reported missing where a field starts with something else" in {
    errors(": 1") shouldBe List(ExpectedKey(loc(1, 1)))
    errors("{ a: 1 }") shouldBe List(ExpectedKey(loc(1, 1)))
    errors("a: 1\n= 2") shouldBe List(ExpectedKey(loc(2, 1)))
  }

  it should "be reported missing after a dot" in {
    errors("a.: 1") shouldBe List(ExpectedKey(loc(1, 3)))
    errors("a.\nb: 1") shouldBe List(ExpectedKey(loc(1, 3)))
    errors("a.@t: 1") shouldBe List(ExpectedKey(loc(1, 3)))
  }

  it should "be reported in double or single quotes, which are for values" in {
    errors("\"a\": 1") shouldBe List(QuotedKey(loc(1, 1)))
    errors("'a': 1") shouldBe List(QuotedKey(loc(1, 1)))
    errors("a.\"b\": 1") shouldBe List(QuotedKey(loc(1, 3)))
  }

  it should "be reported after a tag, which goes after the `:`" in {
    errors("@t a: 1") shouldBe List(TagBeforeKey(loc(1, 1)))
  }

  "A dot of a key" should "be reported with whitespace or a comment on either side" in {
    errors("a .b: 1") shouldBe List(SpaceAroundDot(loc(1, 3)))
    errors("a. b: 1") shouldBe List(SpaceAroundDot(loc(1, 2)))
    errors("a/**/.b: 1") shouldBe List(SpaceAroundDot(loc(1, 6)))
    errors("a.b./**/c: 1") shouldBe List(SpaceAroundDot(loc(1, 4)))
  }

  "A field" should "be reported when its key is followed by neither a dot, `:`, `:=` nor `{`" in {
    errors("a = 1") shouldBe List(ExpectedColon(loc(1, 3)))
    errors("a") shouldBe List(ExpectedColon(loc(1, 2)))
    errors("a\n\nb: 1") shouldBe List(ExpectedColon(loc(1, 2)))
    errors("a b: 1") shouldBe List(ExpectedColon(loc(1, 3)))
  }

  it should "be reported with its `{` on a line after the key" in {
    errors("a\n{ b: 1 }") shouldBe List(BraceOnNextLine(loc(2, 1)))
    errors("a.b\n\n  {}") shouldBe List(BraceOnNextLine(loc(3, 3)))
  }

  it should "be reported with a tag between its key and its value" in {
    errors("a @t { b: 1 }") shouldBe List(TagAfterKey(loc(1, 3)))
    errors("a @t: 1") shouldBe List(TagAfterKey(loc(1, 3)))
  }

  "A value" should "be reported missing" in {
    errors("a:") shouldBe List(MissingValue(loc(1, 3)))
    errors("a: , b: 1") shouldBe List(MissingValue(loc(1, 4)))
    errors("a { b: }") shouldBe List(MissingValue(loc(1, 8)))
    errors("a: ]") shouldBe List(MissingValue(loc(1, 4)))
    errors("a: @t") shouldBe List(MissingValue(loc(1, 6)))
    errors("a: [@t]") shouldBe List(MissingValue(loc(1, 7)))
  }

  it should "be reported missing at the end of the line of its `:` or tag when no value follows on a later one" in {
    errors("a:\n") shouldBe List(MissingValue(loc(1, 3)))
    errors("a { b:\n\n}") shouldBe List(MissingValue(loc(1, 7)))
    errors("a: @t\n, b: 1") shouldBe List(MissingValue(loc(1, 6)))
  }

  it should "be reported on a line after its `:`, `:=` or tag" in {
    errors("a:\n5") shouldBe List(ValueOnNextLine(loc(2, 1)))
    errors("a :=\n\n  [1]") shouldBe List(ValueOnNextLine(loc(3, 3)))
    errors("a: @t\n{}") shouldBe List(ValueOnNextLine(loc(2, 1)))
    errors("a:\n  @t x") shouldBe List(ValueOnNextLine(loc(2, 3)))
    errors("a:\n  #x()") shouldBe List(ValueOnNextLine(loc(2, 3)))
    errors("a:\n  `x`") shouldBe List(ValueOnNextLine(loc(2, 3)))
    errors("a: [\n  @t\n  1]") shouldBe List(ValueOnNextLine(loc(3, 3)))
  }

  it should "be reported in backticks, which are for names" in {
    errors("a: `x`") shouldBe List(BacktickedValue(loc(1, 4)))
  }

  "A separator" should "be reported missing between two fields on a line" in {
    errors("a: 1 b: 2") shouldBe List(MissingSeparator(loc(1, 6)))
    errors("a: 1 b := 2") shouldBe List(MissingSeparator(loc(1, 6)))
    errors("a: 1 b.c: 2") shouldBe List(MissingSeparator(loc(1, 6)))
    errors("a: 1 b { }") shouldBe List(MissingSeparator(loc(1, 6)))
    errors("a { x: 1 } b: 2") shouldBe List(MissingSeparator(loc(1, 12)))
    errors("a: x `b`: 2") shouldBe List(MissingSeparator(loc(1, 6)))
  }

  it should "be reported missing after a value that is not an unquoted scalar" in {
    errors("a: \"x\" y") shouldBe List(MissingSeparator(loc(1, 8)))
    errors("a: \"x\" \"y\"") shouldBe List(MissingSeparator(loc(1, 8)))
    errors("a: {} y") shouldBe List(MissingSeparator(loc(1, 7)))
    errors("a: [] [") shouldBe List(MissingSeparator(loc(1, 7)))
  }

  it should "be reported missing between two elements, whitespace never separating" in {
    errors("a: [x y]") shouldBe List(MissingSeparator(loc(1, 7)))
    errors("a: [x `y`]") shouldBe List(MissingSeparator(loc(1, 7)))
    errors("a: [{} {}]") shouldBe List(MissingSeparator(loc(1, 8)))
  }

  "A second word after an unquoted scalar" should "be reported as a value to quote" in {
    errors("a: foo bar") shouldBe List(SpaceInValue(loc(1, 8)))
    errors("a: foo bar baz") shouldBe List(SpaceInValue(loc(1, 8)))
    errors("a: foo \"bar\"") shouldBe List(SpaceInValue(loc(1, 8)))
  }

  it should "not be reported as a key the lexer cannot read" in {
    errors("a: foo bar!") shouldBe List(SpaceInValue(loc(1, 8)))
  }

  "Substitutions and directives" should "be reported as not supported yet" in {
    errors("a: ${b}") shouldBe List(NotSupportedYet(loc(1, 4), "substitutions"))
    errors("a: \"x ${b}\"") shouldBe List(NotSupportedYet(loc(1, 7), "substitutions"))
    errors("#include(file = x)") shouldBe List(NotSupportedYet(loc(1, 1), "directives"))
    errors("a: #x()") shouldBe List(NotSupportedYet(loc(1, 4), "directives"))
  }

  "A fatal error of the lexer" should "be the only error at the end it causes" in {
    errors("a: \"x") shouldBe List(UnterminatedString(loc(1, 4)))
    errors("a { b: \"x") shouldBe List(UnterminatedString(loc(1, 8)))
    errors("a:\n\"x") shouldBe List(UnterminatedString(loc(2, 1)))
    errors("/** d */ \"x") shouldBe List(UnterminatedString(loc(1, 10)))
  }

  it should "be reported after the parser's error when it was found reading ahead" in {
    errors("a\n`x") shouldBe List(ExpectedColon(loc(1, 2)), UnterminatedName(loc(2, 1)))
    errors("a: foo bar \"x") shouldBe List(SpaceInValue(loc(1, 8)), UnterminatedString(loc(1, 12)))
  }
}
