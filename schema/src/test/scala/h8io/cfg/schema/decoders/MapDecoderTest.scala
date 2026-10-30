package h8io.cfg.schema.decoders

import cats.syntax.all.*
import h8io.cfg.schema.Decoder
import h8io.cfg.schema.errors.{CfgErrorOps, UnexpectedNode}
import h8io.cfg.schema.testutil.MockLocation
import h8io.cfg.{Id, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MapDecoderTest extends AnyFlatSpec with Matchers with MockFactory {
  "MapDecoder" should "return a map of decoded values if all values are decoded successfully" in {
    val map = mock[Node.Map]
    (() => map.iterator).expects().returns(
      Iterator("abc" -> "zyx", "def" -> "wvu", "ghi" -> "tsr").map { case (k, v) =>
        Node.Scalar(Id.Key(k, Id.Root), Set.empty, v, MockLocation(s"$v location ($k)"))
      } ++ List(Node.Null(Id.Key("jkl", Id.Root), Set.empty, MockLocation("null location (qpo)"))))
    // Shadows the built-in stringDecoder of the package, which would otherwise be ambiguous with it on Scala 2
    implicit def stringDecoder: Decoder[String] = {
      case Node.Scalar(_, _, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode[String](node).invalid
    }
    mapDecoder[String].apply(map) shouldBe
      Map("abc" -> "decoded zyx", "def" -> "decoded wvu", "ghi" -> "decoded tsr").valid
  }

  it should "return a list of all errors if some values are not decoded successfully" in {
    val map = mock[Node.Map]
    val mapItem = mock[Node.IMap[Id.Key]]
    val seqItem = mock[Node.ISeq[Id.Key]]
    (() => map.iterator).expects().returns(
      Iterator(
        Node.Scalar(Id.Key("零", Id.Root), Set.empty, "first", MockLocation(s"first location (0)")),
        mapItem,
        Node.Scalar(Id.Key("二", Id.Root), Set.empty, "third", MockLocation(s"third location (2)")),
        seqItem,
        Node.Scalar(Id.Key("四", Id.Root), Set.empty, "fifth", MockLocation(s"fifth location (4)")),
        Node.Scalar(Id.Key("五", Id.Root), Set.empty, "sixth", MockLocation(s"sixth location (5)")),
        Node.Null(Id.Key("六", Id.Root), Set.empty, MockLocation(s"null location (6)"))
      ))
    // Shadows the built-in stringDecoder of the package, which would otherwise be ambiguous with it on Scala 2
    implicit def stringDecoder: Decoder[String] = {
      case Node.Scalar(_, _, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode[String](node).invalid
    }
    mapDecoder[String].apply(map) shouldBe
      (UnexpectedNode[String](mapItem) & UnexpectedNode[String](seqItem)).invalid
  }

  it should "return UnexpectedNode[Map[String, String]] if node is not a Map" in {
    val scalar = Node.Scalar(Id.Root, Set.empty, "value", MockLocation("loc"))
    // Shadows the built-in stringDecoder of the package, which would otherwise be ambiguous with it on Scala 2
    implicit def stringDecoder: Decoder[String] = { case node: Node => UnexpectedNode[String](node).invalid }
    mapDecoder[String].apply(scalar) shouldBe UnexpectedNode[Map[String, String]](scalar).invalid
  }
}
