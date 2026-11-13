package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DirectiveMessageTest extends AnyFlatSpec with Matchers {
  private val at = SourceLocation("s", 1, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "A directive" should "be named as written" in {
    text(UnknownDirective(at, "x")) shouldBe (("unknown directive '#x'", None))
    text(DisabledDirective(at, "a b")) shouldBe (("directive '#`a b`' is disabled in this loader", None))
    text(DirectiveNotMap(at, "x")) shouldBe
      (("'#x' as a statement gave something other than a map", Some("make it the value of a field")))
  }

  "A handler's failure" should "be shown under its directive, with its hint" in {
    text(DirectiveFailed(at, "include", IncludeNotFound("conf"))) shouldBe (("'#include': conf is not found", None))
    text(DirectiveFailed(at, "x", Problem("no", Some("try")))) shouldBe (("'#x': no", Some("try")))
  }

  "A cycle of sources" should "name them in order" in {
    text(DirectiveCycle(at, ::("a", List("b", "a")))) shouldBe (("a cycle of sources: a -> b -> a", None))
    text(DirectiveTooDeep(at, 64)) shouldBe
      (("sources nested 64 deep, a cycle that changes its source on every step", None))
  }
}
