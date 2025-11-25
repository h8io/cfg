package h8io.cfg.raw

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NodeTest extends AnyFlatSpec with Matchers with MockFactory {
  "Map" should "be mocked successfully" in {
    "mock[Node.Map[Id]]" should compile
  }

  "Map.isEmpty" should "return true if size is 0" in {
    val map = mock[Node.Map[Id]]
    (() => map.size).expects().returning(0)
    map.isEmpty shouldBe true
  }

  it should "return false if size is not 0" in {
    val map = mock[Node.Map[Id]]
    (() => map.size).expects().returning(42)
    map.isEmpty shouldBe false
  }

  "Seq" should "be mocked successfully" in {
    "mock[Node.Seq[Id]]" should compile
  }

  "Seq.isEmpty" should "return true if size is 0" in {
    val seq = mock[Node.Seq[Id]]
    (() => seq.size).expects().returning(0)
    seq.isEmpty shouldBe true
  }

  it should "return false if size is not 0" in {
    val seq = mock[Node.Seq[Id]]
    (() => seq.size).expects().returning(42)
    seq.isEmpty shouldBe false
  }
}
