package h8io.cfg.raw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IdTest extends AnyFlatSpec with Matchers {
  "Root.description" should "return the \"root\" string" in {
    Id.Root.description shouldBe "root"
  }

  "Index.fits" should "check if the index is in bounds of collection with size n" in {
    Id.Index(0).fits(1) shouldBe true
    Id.Index(16).fits(17) shouldBe true
    Id.Index(24).fits(42) shouldBe true

    Id.Index(0).fits(0) shouldBe false
    Id.Index(-1).fits(1) shouldBe false
    Id.Index(1).fits(1) shouldBe false
    Id.Index(42).fits(17) shouldBe false
  }
}
