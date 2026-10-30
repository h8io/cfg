package h8io.cfg.schema.decoders

import cats.syntax.all.*
import h8io.cfg.schema.Decoder
import h8io.cfg.schema.errors.{CfgErrorOps, UnexpectedNode}
import h8io.cfg.schema.testutil.MockLocation
import h8io.cfg.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VectorDecoderTest extends AnyFlatSpec with Matchers with MockFactory {
  "VectorDecoder" should "return a vector of decoded values if all values are decoded successfully" in {
    val seq = mock[Node.Seq]
    (() => seq.iterator).expects().returns(
      Iterator("abc", "def", "ghi").zipWithIndex.map { case (v, i) =>
        Node.Scalar(Id.Index(i, Id.Root), Set.empty, v, MockLocation(s"$v location ($i)"))
      })
    // Shadows the built-in stringDecoder of the package, which would otherwise be ambiguous with it on Scala 2
    implicit def stringDecoder: Decoder[String] = {
      case Node.Scalar(_, _, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode[String](node).invalid
    }
    vectorDecoder[String].apply(seq) shouldBe Vector("decoded abc", "decoded def", "decoded ghi").valid
  }

  it should "return a list of all errors if some values are not decoded successfully" in {
    val seq = mock[Node.Seq]
    val mapItem = mock[Node.IMap[Id.Index]]
    val seqItem = mock[Node.ISeq[Id.Index]]
    val nullItem = Node.Null(Id.Index(6, Id.Root), Set.empty, MockLocation(s"null location (6)"))
    (() => seq.iterator).expects().returns(
      Iterator(
        Node.Scalar(Id.Index(0, Id.Root), Set.empty, "first", MockLocation(s"first location (0)")),
        mapItem,
        Node.Scalar(Id.Index(2, Id.Root), Set.empty, "third", MockLocation(s"third location (2)")),
        seqItem,
        Node.Scalar(Id.Index(4, Id.Root), Set.empty, "fifth", MockLocation(s"fifth location (4)")),
        Node.Scalar(Id.Index(5, Id.Root), Set.empty, "sixth", MockLocation(s"sixth location (5)")),
        nullItem
      ))
    // Shadows the built-in stringDecoder of the package, which would otherwise be ambiguous with it on Scala 2
    implicit def stringDecoder: Decoder[String] = {
      case Node.Scalar(_, _, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode[String](node).invalid
    }
    vectorDecoder[String].apply(seq) shouldBe
      (UnexpectedNode[String](mapItem) & UnexpectedNode[String](seqItem) & nullItem).invalid
  }

  it should "return UnexpectedNode[Vector[String]] if node is not a Seq" in {
    val scalar = Node.Scalar(Id.Root, Set.empty, "value", MockLocation("loc"))
    // Shadows the built-in stringDecoder of the package, which would otherwise be ambiguous with it on Scala 2
    implicit def stringDecoder: Decoder[String] = { case node: Node => UnexpectedNode[String](node).invalid }
    vectorDecoder[String].apply(scalar) shouldBe UnexpectedNode[Vector[String]](scalar).invalid
  }
}
