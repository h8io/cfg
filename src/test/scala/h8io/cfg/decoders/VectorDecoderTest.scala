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

class VectorDecoderTest extends AnyFlatSpec with Matchers with MockFactory {
  "VectorDecoder" should "return a vector of decoded values if all values are decoded successfully" in {
    val seq = mock[Node.Seq]
    (() => seq.iterator).expects().returns(
      Iterator("abc", "def", "ghi").zipWithIndex.map { case (v, i) =>
        Node.Scalar(Id.Index(i, Id.Root), v, MockLocation(s"$v location ($i)"))
      })
    def decoder: Decoder[String] = {
      case Node.Scalar(_, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode(node, typeOf[String]).invalidNec
    }
    VectorDecoder[String](decoder)(seq) shouldBe Vector("decoded abc", "decoded def", "decoded ghi").valid
  }

  it should "return a list of all errors if some values are not decoded successfully" in {
    val seq = mock[Node.Seq]
    val mapItem = mock[Node.IMap[Id.Index]]
    val seqItem = mock[Node.ISeq[Id.Index]]
    val nullItem = Node.Null(Id.Index(6, Id.Root), MockLocation(s"null location (6)"))
    (() => seq.iterator).expects().returns(
      Iterator(
        Node.Scalar(Id.Index(0, Id.Root), "first", MockLocation(s"first location (0)")),
        mapItem,
        Node.Scalar(Id.Index(2, Id.Root), "third", MockLocation(s"third location (2)")),
        seqItem,
        Node.Scalar(Id.Index(4, Id.Root), "fifth", MockLocation(s"fifth location (4)")),
        Node.Scalar(Id.Index(5, Id.Root), "sixth", MockLocation(s"sixth location (5)")),
        nullItem
      ))
    def decoder: Decoder[String] = {
      case Node.Scalar(_, v, _) => s"decoded $v".valid
      case node: Node => UnexpectedNode(node, typeOf[String]).invalidNec
    }
    VectorDecoder[String](decoder)(seq) shouldBe
      NonEmptyChain(UnexpectedNode(mapItem, typeOf[String]), UnexpectedNode(seqItem, typeOf[String]), nullItem).invalid
  }
}
