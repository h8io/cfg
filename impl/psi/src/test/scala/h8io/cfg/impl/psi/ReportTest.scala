package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportTest extends AnyFlatSpec with Matchers {
  "A report" should "count the errors, and list each under its source with its hint" in {
    val errors = Psi.parse("s", "a = 1\nb: foo bar").fold(identity, root => fail(root.toString))
    errors.message shouldBe
      """2 errors in 1 source
        |s
        |  1:3: expected ':', ':=' or '{' after a key, found '='
        |       hint: write a field as 'key: value' or 'key { … }'
        |  2:8: a second word, 'bar', after a value
        |       hint: quote a value with whitespace in it, as in "foo bar"""".stripMargin
    errors.toString shouldBe errors.message
  }

  it should "keep every error on its lines, whatever the text found or the name of the source" in {
    Psi.parse("s\n2", "a: [] \"x\\ny\"").fold(_.message, root => fail(root.toString)) shouldBe
      """1 error in 1 source
        |s\n2
        |  1:7: expected ',' or a line break, found a quoted string "x\ny"
        |       hint: separate items with ',' or a line break; whitespace never separates""".stripMargin
  }

  it should "say one error in one source in the singular, and show an error with no hint on one line" in {
    PsiErrors(::(UnknownDirective(SourceLocation("s", 1, 1), "x"), Nil)).message shouldBe
      "1 error in 1 source\ns\n  1:1: unknown directive '#x'"
  }

  it should "group by source in the order found, each error's hint under it" in {
    val errors = PsiErrors(
      ::(
        UnresolvedSubstitution(SourceLocation("a", 1, 4), "x"),
        List(
          UnknownDirective(SourceLocation("b", 2, 1), "y"),
          UnresolvedSubstitution(SourceLocation("a", 5, 2), "z"))
      ))
    errors.message shouldBe List(
      "3 errors in 2 sources",
      "a",
      "  1:4: nothing at 'x' to substitute",
      "       hint: write '$" + "{?x}' if it may be missing",
      "  5:2: nothing at 'z' to substitute",
      "       hint: write '$" + "{?z}' if it may be missing",
      "b",
      "  2:1: unknown directive '#y'"
    ).mkString("\n")
  }
}
