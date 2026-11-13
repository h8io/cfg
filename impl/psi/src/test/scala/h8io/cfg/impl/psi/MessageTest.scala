package h8io.cfg.impl.psi

import h8io.cfg.Id
import h8io.cfg.impl.psi.Token.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MessageTest extends AnyFlatSpec with Matchers {
  "Found text" should "be cut with … past 20 code points" in {
    Message.cut("a" * 20) shouldBe "a" * 20
    Message.cut("a" * 21) shouldBe "a" * 20 + "…"
    Message.cut("😀" * 21) shouldBe "😀" * 20 + "…"
    Message.cut("\n" * 11) shouldBe "\\n" * 10 + "…"
    Message.quote("x" * 25) shouldBe "'" + "x" * 20 + "…'"
  }

  "A character" should "be shown by its code point and name, and the escape that writes it" in {
    Message.char(' ') shouldBe "U+00A0 NO-BREAK SPACE"
    Message.char('͸') shouldBe "U+0378"
    Message.escape('​') shouldBe "\\u200B"
  }

  "Found text" should "keep to one line and show what would not show" in {
    Message.visible("a b\b\f\n\r\tc") shouldBe "a b\\b\\f\\n\\r\\tc"
    Message.visible("\u0000\u007F\u0085\u00A0\u200B\u2028\u2029\u3000") shouldBe
      "\\u0000\\u007F\\u0085\\u00A0\\u200B\\u2028\\u2029\\u3000"
    Message.visible("é😀\\`") shouldBe "é😀\\`"
  }

  "A path" should "be written as Id.path writes it" in {
    val path = Id.Key("d", Id.Index(0, Id.Key("b c", Id.Key("a", Id.Root))))
    Message.path(path) shouldBe path.path
    Message.path(Id.Index(0, Id.Root)) shouldBe "[0]"
    Message.path(Id.Root) shouldBe ""
  }

  "A key" should "be written as a key is, bare or in backticks with its escapes" in {
    Message.key("ab") shouldBe "ab"
    Message.key("a\nb") shouldBe "`a\\nb`"
  }

  "A directive's name" should "be written after '#' as a key is" in {
    Message.directive("include") shouldBe "'#include'"
    Message.directive("a b") shouldBe "'#`a b`'"
  }

  "A token" should "be described as found" in {
    val described = List(
      LeftBrace,
      RightBrace,
      LeftBracket,
      RightBracket,
      LeftParen,
      RightParen,
      Comma,
      Dot,
      Colon,
      Replace,
      Equals,
      Newline,
      End,
      DocComment("d"),
      Name("a", quoted = false),
      Name("a b", quoted = true),
      Tag("t"),
      Integer("-5"),
      Directive("x"),
      Scalar(Vector(Text("v")), quoted = false),
      Scalar(Vector(Text("v")), quoted = true),
      Scalar(Vector(Text("v" * 30)), quoted = true)
    ).map(describe)
    described shouldBe List(
      "'{'",
      "'}'",
      "'['",
      "']'",
      "'('",
      "')'",
      "','",
      "'.'",
      "':'",
      "':='",
      "'='",
      "a line break",
      "the end of the source",
      "a doc comment",
      "'a'",
      "'`a b`'",
      "'@t'",
      "'-5'",
      "'#x'",
      "'v'",
      "a quoted string \"v\"",
      "a quoted string \"" + "v" * 19 + "…"
    )
  }

  "A scalar" should "be written with its substitutions" in {
    val at = SourceLocation("s", 1, 1)
    written(
      Vector(
        Text("x"),
        Reference(Id.Key("a", Id.Root), optional = false, at),
        Reference(Id.Index(0, Id.Key("b", Id.Root)), optional = true, at))) shouldBe "x$" + "{a}$" + "{?b[0]}"
  }
}
