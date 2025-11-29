package h8io.cfg.errors

import h8io.cfg.CfgError
import h8io.cfg.raw.{Id, Location, Node}
import h8io.reflect.typeOf
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnexpectedNodeTest extends AnyFlatSpec with Matchers with MockFactory {
  "hashCode" should "be the same for equal UnexpectedNode objects" in {
    val location = mock[Location]
    val node1 = Node.Scalar(Id.Root, "Cthulhu", location)
    val node2 = Node.Scalar(Id.Root, "Cthulhu", location)
    UnexpectedNode[String](node1).hashCode() shouldEqual UnexpectedNode[String](node2).hashCode()
  }

  "equals" should "be true for equal UnexpectedNode objects" in {
    val node = mock[Node.Map]
    UnexpectedNode[String](node) shouldEqual UnexpectedNode[String](node)
  }

  it should "be false for different UnexpectedNode objects (different nodes)" in {
    val node1 = mock[Node.Seq]
    val node2 = mock[Node.Seq]
    UnexpectedNode[String](node1) should not equal UnexpectedNode[String](node2)
  }

  it should "be false for different UnexpectedNode objects (different types)" in {
    val node = mock[Node.Map]
    UnexpectedNode[String](node) should not equal UnexpectedNode[Int](node)
  }

  it should "be false with any other CfgError" in {
    val node = mock[Node.Seq]
    val error = mock[CfgError]
    UnexpectedNode[Double](node) should not equal error
  }
}
