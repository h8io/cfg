package h8io.cfg.raw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RefTest extends AnyFlatSpec with Matchers {
  "Root.description" should "return the \"root\" string" in {
    Ref.Root.description shouldBe "root"
  }

  "Index.fits" should "check if the index is in bounds of collection with size n" in {
    Ref.Index(0).fits(1) shouldBe true
    Ref.Index(16).fits(17) shouldBe true
    Ref.Index(24).fits(42) shouldBe true

    Ref.Index(0).fits(0) shouldBe false
    Ref.Index(-1).fits(1) shouldBe false
    Ref.Index(1).fits(1) shouldBe false
    Ref.Index(42).fits(17) shouldBe false
  }
}
