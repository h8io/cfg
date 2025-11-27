package h8io.cfg.decoders

import cats.data.NonEmptyChain
import cats.syntax.all.*
import h8io.cfg.Decoder
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.MockLocation
import h8io.reflect.typeOf
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MapDecoderTest extends AnyFlatSpec with Matchers with MockFactory {
  "MapDecoder" should "return a map of decoded values if all values are decoded successfully" in {
    val map = mock[Node.Map]
    (() => map.iterator).expects().returns(
      Iterator("abc" -> "zyx", "def" -> "wvu", "ghi" -> "tsr").map { case (k, v) =>
        Node.Scalar(Id.Key(k, Id.Root), v, MockLocation(s"$v location ($k)"))
      } ++ List(Node.Null(Id.Key("jkl", Id.Root), MockLocation("null location (qpo)"))))
    def decoder: Decoder[String] = {
      case Node.Scalar(_, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode[String](node).invalidNec
    }
    MapDecoder[String](decoder)(map) shouldBe
      Map("abc" -> "decoded zyx", "def" -> "decoded wvu", "ghi" -> "decoded tsr").valid
  }

  it should "return a list of all errors if some values are not decoded successfully" in {
    val map = mock[Node.Map]
    val mapItem = mock[Node.IMap[Id.Key]]
    val seqItem = mock[Node.ISeq[Id.Key]]
    (() => map.iterator).expects().returns(
      Iterator(
        Node.Scalar(Id.Key("零", Id.Root), "first", MockLocation(s"first location (0)")),
        mapItem,
        Node.Scalar(Id.Key("二", Id.Root), "third", MockLocation(s"third location (2)")),
        seqItem,
        Node.Scalar(Id.Key("四", Id.Root), "fifth", MockLocation(s"fifth location (4)")),
        Node.Scalar(Id.Key("五", Id.Root), "sixth", MockLocation(s"sixth location (5)")),
        Node.Null(Id.Key("六", Id.Root), MockLocation(s"null location (6)"))
      ))
    def decoder: Decoder[String] = {
      case Node.Scalar(_, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode[String](node).invalidNec
    }
    MapDecoder[String](decoder)(map) shouldBe
      NonEmptyChain(UnexpectedNode[String](mapItem), UnexpectedNode[String](seqItem)).invalid
  }
}
