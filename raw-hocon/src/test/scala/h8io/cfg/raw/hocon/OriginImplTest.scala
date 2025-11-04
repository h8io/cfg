package h8io.cfg.raw.hocon

import com.typesafe.config.ConfigOrigin
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OriginImplTest extends AnyFlatSpec with Matchers with MockFactory {
  "CfgOriginImplTest" should "should return a description from underlying ConfigOrigin" in {
    val origin = mock[ConfigOrigin]
    (origin.description _).expects().returns("CfgOriginImpl test description")
    OriginImpl(origin).description shouldBe "CfgOriginImpl test description"
  }
}
