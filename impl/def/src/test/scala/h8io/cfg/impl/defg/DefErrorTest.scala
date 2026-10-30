package h8io.cfg.impl.defg

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException

class DefErrorTest extends AnyFlatSpec with Matchers {
  "SourceUnreadable" should "point at the start of the source" in {
    SourceUnreadable("file:/missing.def", new IOException("gone")).location shouldBe
      SourceLocation("file:/missing.def", 1, 1)
  }
}
