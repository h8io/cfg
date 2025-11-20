package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigOrigin
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LocationImplTest extends AnyFlatSpec with Matchers with MockFactory {
  "LocationImpl" should "should return a description from underlying ConfigOrigin" in {
    val origin = mock[ConfigOrigin]
    (origin.description _).expects().returns("LocationImpl test description")
    LocationImpl(origin).description shouldBe "LocationImpl test description"
  }
}
