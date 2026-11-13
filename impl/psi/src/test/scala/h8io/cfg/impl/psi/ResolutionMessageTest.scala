package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResolutionMessageTest extends AnyFlatSpec with Matchers {
  private def at(line: Int): SourceLocation = SourceLocation("s", line, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "Depth" should "say the limit, whether the source ends there, and how to raise it" in {
    val hint = Some("nest it less, or build the loader with a higher withDepthLimit")
    text(DepthExceeded(at(1), 256, fatal = true)) shouldBe
      (("nested deeper than 256 levels; the rest of this source is not read", hint))
    text(DepthExceeded(at(1), 256, fatal = false)) shouldBe (("nested deeper than 256 levels", hint))
    text(ResolutionTooDeep(at(1), "a.b", 256)) shouldBe
      (
        (
          "resolving '$" + "{a.b}' goes more than 256 steps deep, levels and substitutions together",
          Some("shorten the chain of substitutions, or build the loader with a higher withDepthLimit")))
  }

  "A merge with a substitution" should "name the earlier value and offer ':='" in {
    text(MergeWithSubstitution(at(2), at(1))) shouldBe
      ((
        "':' merges with the value at s:1:1, and one of them is a substitution, which may not be a map",
        Some("write ':=' to replace the earlier value")))
  }

  "A substitution" should "name its path, and the origin of what it found" in {
    text(UnresolvedSubstitution(at(1), "a.b")) shouldBe
      (("nothing at 'a.b' to substitute", Some("write '$" + "{?a.b}' if it may be missing")))
    text(SubstitutionCycle(at(1), "a")) shouldBe (("'$" + "{a}' leads back to itself", None))
    text(NonScalarInConcatenation(at(2), "m", at(1))) shouldBe
      (("'$" + "{m}' in a string is not a scalar; it is defined at s:1:1", None))
  }
}
