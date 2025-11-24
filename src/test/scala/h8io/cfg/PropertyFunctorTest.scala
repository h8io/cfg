package h8io.cfg

import h8io.cfg.raw.Node
import cats.syntax.all.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PropertyFunctorTest extends AnyFlatSpec with Matchers with MockFactory {
  "map" should "return a Thrown object if the function throws an exception" in {
    val property = mock[Property[String]]("property")
    val cfg = mock[Node.Map]("cfg")
    (property.apply _).expects(cfg).returns("test value".valid)
    val exception = new RuntimeException("map exception")
    val f = mock[String => Int]
    (f.apply _).expects("test value").throws(exception)
    Property.Functor.map(property)(f)(cfg) shouldBe Property.Thrown(cfg, property, exception).invalidNec
  }
}
