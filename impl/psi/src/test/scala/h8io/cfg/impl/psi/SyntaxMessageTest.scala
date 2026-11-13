package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyntaxMessageTest extends AnyFlatSpec with Matchers {
  private val at = SourceLocation("s", 1, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "Keys" should "say what was found instead, and how a key is written" in {
    text(ExpectedKey(at, "':'")) shouldBe (("expected a key, found ':'", None))
    text(QuotedKey(at, "a b")) shouldBe (("a key in quotes", Some("write a quoted key in backticks: `a b`")))
    text(QuotedKey(at, "k" * 30)) shouldBe
      (("a key in quotes", Some("write a quoted key in backticks: `" + "k" * 19 + "…")))
    text(SpaceAroundDot(at)) shouldBe
      (("whitespace around a dot of a key", Some("write the keys and the dots touching, as in 'a.b'")))
    text(TagBeforeKey(at)) shouldBe (("a tag before a key", Some("put it after the ':', as in 'key: @tag value'")))
  }

  "After a key" should "come ':', ':=' or '{', on its line" in {
    text(ExpectedColon(at, "'='")) shouldBe
      (("expected ':', ':=' or '{' after a key, found '='", Some("write a field as 'key: value' or 'key { … }'")))
    text(BraceOnNextLine(at)) shouldBe
      (("the '{' of a block on a line after its key", Some("put it on the key's line, as in 'key {'")))
    text(TagAfterKey(at)) shouldBe
      (("a tag between a key and its value", Some("put it after a ':', as in 'key: @tag { … }'")))
  }

  "Values" should "say what was found, and how a value is written" in {
    text(MissingValue(at, "','")) shouldBe (("expected a value, found ','", None))
    text(ValueOnNextLine(at)) shouldBe (("a value on a line after its ':'", Some("put it on the same line")))
    text(SpaceInValue(at, "bar")) shouldBe
      (("a second word, 'bar', after a value", Some("quote a value with whitespace in it, as in \"foo bar\"")))
    text(JoinedValues(at)) shouldBe
      ((
        "a value right after another; values never join",
        Some("write a concatenation as one string, as in \"jdbc:$" + "{host}\"")))
    text(BacktickedValue(at)) shouldBe (("a value in backticks, which are for names", Some("quote a value in \"…\"")))
  }

  "Separators" should "say what was found" in {
    text(MissingSeparator(at, "'b:'")) shouldBe
      ((
        "expected ',' or a line break, found 'b:'",
        Some("separate items with ',' or a line break; whitespace never separates")))
    text(EmptyElement(at)) shouldBe (("a ',' with nothing before it", Some("remove it")))
    text(MisplacedDocComment(at)) shouldBe
      ((
        "a doc comment that documents nothing",
        Some("put it before a field, an element or a directive, or make it a plain comment, '/* … */'")))
  }

  "Brackets" should "be named, and say when they end the source" in {
    text(UnmatchedBracket(at, '}', fatal = false)) shouldBe (("'}' closes no bracket open", None))
    text(UnmatchedBracket(at, ']', fatal = true)) shouldBe
      (("']' closes no bracket open; the rest of this source is not read", None))
    text(UnclosedBracket(at, '[')) shouldBe (("'[' is never closed; the rest of this source is not read", None))
  }

  "A directive's arguments" should "say how they are written" in {
    text(DetachedArguments(at)) shouldBe
      (("the '(' of a directive's arguments apart from its name", Some("write it touching the name, as in '#name(…)'")))
    text(ExpectedArgument(at, "'@t'")) shouldBe (("expected an argument, found '@t'", None))
    text(EqualsInArgument(at)) shouldBe
      (("'=' in an argument", Some("write a named argument as a field is, 'name: value'")))
    text(InvalidLiteral(at, "1.5")) shouldBe (("'1.5' is neither a name nor an integer", Some("quote it: \"1.5\"")))
    text(TripleQuotedArgument(at)) shouldBe
      (("a triple-quoted string in a directive's arguments; it is read all the same", Some("write it in \"…\" or '…'")))
    text(SubstitutionInArgument(at)) shouldBe
      (("a substitution in an argument, which is taken as written", Some("to mean the text, write '\\$" + "{'")))
    text(PositionalAfterNamed(at)) shouldBe
      (("a positional argument after a named one", Some("put positional arguments first, as in Scala")))
    text(UnknownArgument(at, "n")) shouldBe (("no argument named 'n' is taken here", None))
    text(ExtraArgument(at)) shouldBe (("an argument past the last parameter", None))
    text(DuplicateArgument(at, "n")) shouldBe (("argument 'n' is given twice", None))
    text(MissingArgument(at, "n")) shouldBe (("argument 'n' is required and not given", None))
    text(InvalidArgument(at, "n")) shouldBe (("the value of argument 'n' cannot be taken", None))
  }
}
