package h8io.cfg.impl.yaml

import h8io.cfg.impl.yaml.context.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LocationImplTest extends AnyFlatSpec with Matchers {
  private val mapping = yaml"""
a: 1
bb: 22
"""

  "description" should "render the stream name with a one-based line and column" in {
    LocationImpl(valueOf(mapping, "a")).description shouldBe s"$Label: 2:4"
    LocationImpl(valueOf(mapping, "bb")).description shouldBe s"$Label: 3:5"
  }

  it should "report an unknown position for a node without a mark" in {
    LocationImpl(None).description shouldBe LocationImpl.Unknown
    LocationImpl(emptyMapping).description shouldBe LocationImpl.Unknown
  }

  "toString" should "return the description" in {
    val location = LocationImpl(valueOf(mapping, "a"))
    location.toString shouldBe location.description
  }
}
