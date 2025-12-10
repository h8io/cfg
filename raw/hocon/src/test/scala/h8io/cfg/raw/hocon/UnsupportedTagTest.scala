package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigValue
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnsupportedTagTest extends AnyFlatSpec with Matchers with MockFactory {
  "location" should "return a location from underlying ConfigValue" in {
    val value = mock[ConfigValue]
    (value.origin _).expects().returns(mock[com.typesafe.config.ConfigOrigin]).twice()
    UnsupportedTag(value).location shouldBe LocationImpl(value)
  }
}
