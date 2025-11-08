package h8io.cfg.raw

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NodeTest extends AnyFlatSpec with Matchers with MockFactory {
  "Map" should "be mocked successfully" in {
    "mock[Node.Map[Id]]" should compile
    "mock[Node.Map[Id.Root]]" should compile
    "mock[Node.Map[Id.Key]]" should compile
    "mock[Node.Map[Id.Index]]" should compile
  }

  "Seq" should "be mocked successfully" in {
    "mock[Node.Seq[Id]]" should compile
    "mock[Node.Seq[Id.Root]]" should compile
    "mock[Node.Seq[Id.Key]]" should compile
    "mock[Node.Seq[Id.Index]]" should compile
  }
}
