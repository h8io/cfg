package h8io.cfg.impl.psi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException

class PsiErrorTest extends AnyFlatSpec with Matchers {
  "SourceUnreadable" should "point at the start of the source" in {
    SourceUnreadable("file:/missing.psi", new IOException("gone")).location shouldBe
      SourceLocation("file:/missing.psi", 1, 1)
  }
}
