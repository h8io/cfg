package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkerMessageTest extends AnyFlatSpec with Matchers {
  private val at = SourceLocation("s", 1, 1)

  private def text(error: PsiError): (String, Option[String]) = (error.message, error.hint)

  "A block's opening line" should "hold only markers that can be read" in {
    text(WhitespaceAfterDelimiter(at)) shouldBe
      (("whitespace on the opening line of a block, which holds only its markers", Some("remove it")))
    text(InvalidMarker(at)) shouldBe
      ((
        "the markers of a block cannot be read from here; the block is read with no marker",
        Some(
          "markers are names separated by spaces, each with its arguments in parentheses or none; " +
            "the block itself starts on the next line")))
    text(UnknownMarker(at, "m")) shouldBe (("unknown marker 'm'; the block is read with no marker", None))
    text(ConflictingIndent(at, ">")) shouldBe
      ((
        "marker '>' sets the indent a second time; the block is read with no marker",
        Some("keep one marker that sets the indent")))
    text(BuiltInMarkerAfterLoaders(at, "fold")) shouldBe
      ((
        "built-in marker 'fold' after one of the loader's; the block is read with no marker",
        Some("put the built-in markers first")))
  }

  "A block's lines" should "be indented alike and enough" in {
    text(InconsistentIndent(at)) shouldBe
      ((
        "the indent of this line differs from an earlier one's, such as a tab against a space",
        Some("indent the lines of a block alike")))
    text(InsufficientIndent(at)) shouldBe (("a line of a block indented less than its width requires", None))
  }

  "A loader's marker that failed" should "be named, with its handler's message and hint" in {
    text(MarkerFailed(at, "upper", Problem("no"))) shouldBe (("marker 'upper': no", None))
    val plain = new MarkerError { def message: String = "plain" }
    text(MarkerFailed(at, "upper", plain)) shouldBe (("marker 'upper': plain", None))
    text(MarkerFailed(at, "upper", Problem("no", Some("try")))) shouldBe (("marker 'upper': no", Some("try")))
  }
}
