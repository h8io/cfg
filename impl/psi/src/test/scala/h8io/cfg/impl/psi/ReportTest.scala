package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReportTest extends AnyFlatSpec with Matchers {
  private def merge(at: String): String =
    "':' merges with the value at " + at + ", and one of them is a substitution, which may not be a map"

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

  it should "say one error in one source in the singular, and show an error with no hint on one line" in {
    PsiErrors(::(UnknownDirective(SourceLocation("s", 1, 1), "x"), Nil)).message shouldBe
      "1 error in 1 source\ns\n  1:1: unknown directive '#x'"
  }

  it should "group by source in the order found, and write a composite location's origin after the message" in {
    val hint = "hint: write ':=' to replace the earlier value"
    val concatenation = ConcatenationLocation(
      SourceLocation("a", 5, 2),
      ::(ReferenceLocation(SourceLocation("a", 5, 7), "y", SourceLocation("a", 1, 4)), Nil))
    val errors = PsiErrors(
      ::(
        UnresolvedSubstitution(SourceLocation("a", 1, 4), "x"),
        List(
          MergeWithSubstitution(
            DirectiveLocation(SourceLocation("b", 2, 1), "yaml", SourceLocation("db.yaml", 3, 8)),
            SourceLocation("a", 1, 1)),
          MergeWithSubstitution(MergedLocation(concatenation, ::(SourceLocation("a", 4, 1), Nil)), NoSourceLocation),
          MergeWithSubstitution(SystemPropertyLocation("p"), SourceLocation("a", 1, 1)),
          MergeWithSubstitution(NoSourceLocation, SourceLocation("a", 1, 1)),
          MergeWithSubstitution(
            ReferenceLocation(SourceLocation("b", 3, 5), "z", SourceLocation("a", 1, 1)),
            SourceLocation("a", 1, 1))
        )
      ))
    errors.message shouldBe List(
      "6 errors in 4 sources",
      "a",
      "  1:4: nothing at 'x' to substitute",
      "       hint: write '$" + "{?x}' if it may be missing",
      "  5:2: " + merge("no source") + " ($" + "{y} from a:1:4) (also a:4:1)",
      "       " + hint,
      "b",
      "  2:1: " + merge("a:1:1") + " (#yaml from db.yaml:3:8)",
      "       " + hint,
      "  3:5: " + merge("a:1:1") + " ($" + "{z} from a:1:1)",
      "       " + hint,
      "system property p",
      "  " + merge("a:1:1"),
      "  " + hint,
      "no source",
      "  " + merge("a:1:1"),
      "  " + hint
    ).mkString("\n")
  }
}
