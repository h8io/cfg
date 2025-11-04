package h8io.cfg.raw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PathTest extends AnyFlatSpec with Matchers {
  "Root.description" should "return the \"root\" string" in {
    Path.Root.description shouldBe "root"
  }

  "Index.fits" should "check if the index is in bounds of collection with size n" in {
    Path.Index(0).fits(1) shouldBe true
    Path.Index(16).fits(17) shouldBe true
    Path.Index(24).fits(42) shouldBe true

    Path.Index(0).fits(0) shouldBe false
    Path.Index(-1).fits(1) shouldBe false
    Path.Index(1).fits(1) shouldBe false
    Path.Index(42).fits(17) shouldBe false
  }
}
