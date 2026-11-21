package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FormatterMessageTest extends AnyFlatSpec with Matchers {
  private val at = SourceLocation("s", 1, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "A formatter" should "be read, one in a substitution, and known" in {
    text(InvalidFormatter(at)) shouldBe
      ((
        "the formatter of a substitution cannot be read from here",
        Some("a formatter is a name with its arguments in parentheses or none, right before the '}'")))
    text(SecondFormatter(at)) shouldBe
      (("a second formatter in one substitution", Some("give a substitution one formatter")))
    text(UnknownFormatter(at, "f")) shouldBe (("unknown formatter 'f'", None))
  }

  "A formatted substitution where an injection stands" should "say that a string is no fields or elements" in {
    text(FormattedInjection(at)) shouldBe
      ((
        "a formatted substitution stands where an injection does, and a string is no fields or elements",
        Some("write a key and ':' or a '- ' before it, or remove the formatter")))
  }

  "A formatter that failed" should "be named, with its handler's message and hint" in {
    text(FormatterFailed(at, "upper", Problem("no"))) shouldBe (("formatter 'upper': no", None))
    val plain = new FormatterError { def message: String = "plain" }
    text(FormatterFailed(at, "upper", plain)) shouldBe (("formatter 'upper': plain", None))
    text(FormatterFailed(at, "upper", Problem("no", Some("try")))) shouldBe (("formatter 'upper': no", Some("try")))
  }

  "join's errors" should "say where what it could not join is defined" in {
    text(FormatterFailed(at, "join", JoinNotSequence(SourceLocation("s", 2, 4)))) shouldBe
      (("formatter 'join': what it joins is not a sequence; it is defined at s:2:4", None))
    text(FormatterFailed(at, "join", JoinElementNotScalar(1, SourceLocation("s", 2, 8)))) shouldBe
      (("formatter 'join': element 1 of what it joins is not a scalar; it is defined at s:2:8", None))
  }
}
