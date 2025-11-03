package h8io.cfg.impl.hocon

import com.typesafe.config.ConfigOrigin
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CfgOriginImplTest extends AnyFlatSpec with Matchers with MockFactory {
  "CfgOriginImplTest" should "should return a description from underlying ConfigOrigin" in {
    val origin = mock[ConfigOrigin]
    (origin.description _).expects().returns("CfgOriginImpl test description")
    CfgOriginImpl(origin).description shouldBe "CfgOriginImpl test description"
  }
}
