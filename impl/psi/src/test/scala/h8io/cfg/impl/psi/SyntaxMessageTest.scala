package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyntaxMessageTest extends AnyFlatSpec with Matchers {
  private val at = SourceLocation("s", 1, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "Keys" should "say what was found instead, and how a key is written" in {
    text(ExpectedKey(at, "':'")) shouldBe (("expected a key, found ':'", None))
    val quoted = "write a key bare, or in backticks when it has to be quoted: "
    text(QuotedKey(at, "ab")) shouldBe (("a key in quotes", Some(quoted + "ab")))
    text(QuotedKey(at, "a b")) shouldBe (("a key in quotes", Some(quoted + "`a b`")))
    text(QuotedKey(at, "a`\\\b")) shouldBe (("a key in quotes", Some(quoted + "`a\\`\\\\\\b`")))
    text(QuotedKey(at, "k" * 30)) shouldBe (("a key in quotes", Some(quoted + "k" * 20 + "…")))
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

  "Indentation" should "say what is wrong with a line, and how to align it" in {
    text(NothingIndented(at)) shouldBe
      ((
        "nothing indented deeper below to be the value",
        Some("indent the value on the lines below, or write it on this line; an empty block is '{}' or '[]'")))
    text(UnexpectedIndent(at)) shouldBe
      (("a line indented deeper than its block, where nothing opens one", Some("align it with the lines of its block")))
    text(MisalignedIndent(at)) shouldBe
      ((
        "a line indented less than its block lands on the width of no block still open",
        Some("align it with the lines of the block it belongs to")))
    text(MixedBlock(at)) shouldBe
      ((
        "a block holds '- ' elements or anything else, not both",
        Some("give the elements a key of their own, or write them all with '- '")))
    text(SecondValue(at)) shouldBe
      (("a second value in a block that holds one", Some("write each as an element, '- value', or give each a key")))
    text(ValueAmongFields(at)) shouldBe (("a value among the fields of a block", Some("write a key and ':' before it")))
    text(TagBeforeField(at)) shouldBe
      ((
        "a tag before the first field of a map, which has no place for it",
        Some(
          "write the map in braces after the tag, as in '- @t { host: a }', or the fields on the lines below the tag")))
  }

  "Values" should "say what was found, and how a value is written" in {
    text(MissingValue(at, "','")) shouldBe (("expected a value, found ','", None))
    text(ValueOnNextLine(at)) shouldBe (("a value on a line after its tag", Some("put it on the same line")))
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
      ((
        "a substitution in an argument, which is taken as written",
        Some("to mean the text, quote it, and write '\\$" + "{' for '$" + "{'")))
    text(PositionalAfterNamed(at)) shouldBe
      (("a positional argument after a named one", Some("put positional arguments first, as in Scala")))
    text(UnknownArgument(at, "n")) shouldBe (("no argument named 'n' is taken here", None))
    text(ExtraArgument(at)) shouldBe (("an argument past the last parameter", None))
    text(DuplicateArgument(at, "n")) shouldBe (("argument 'n' is given twice", None))
    text(MissingArgument(at, "n")) shouldBe (("argument 'n' is required and not given", None))
    text(InvalidArgument(at, "n")) shouldBe (("the value of argument 'n' cannot be taken", None))
  }
}
