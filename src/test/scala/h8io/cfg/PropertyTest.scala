package h8io.cfg

import cats.data.Validated
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.MockOrigin
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PropertyTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  "fromDecoder" should "create a property from a decoder for all node types" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, result, description) =>
        val root = mock[Node.Map[Id]]("node")
        implicit val decoder: Decoder[Node[Id], String] = mock[Decoder[Node[Id], String]]("decoder")
        val property = Property.fromDecoder(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockOrigin(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.Valid(result))
          property(root) shouldBe Validated.Valid(result)
        }
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockOrigin(description))
          val error = mock[DecoderError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.invalidNec(error))
          property(root) shouldBe Validated.invalidNec(error)
        }
        inSequence {
          val node = Node.Null(Id.Key(name, Id.Root), MockOrigin(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.Valid(result))
          property(root) shouldBe Validated.Valid(result)
        }
        inSequence {
          val node = Node.Null(Id.Key(name, Id.Root), MockOrigin(description))
          val error = mock[DecoderError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.invalidNec(error))
          property(root) shouldBe Validated.invalidNec(error)
        }
        inSequence {
          val node = Node.None(Id.Key(name, Id.Root), MockOrigin(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.Valid(result))
          property(root) shouldBe Validated.Valid(result)
        }
        inSequence {
          val node = Node.None(Id.Key(name, Id.Root), MockOrigin(description))
          val error = mock[DecoderError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.invalidNec(error))
          property(root) shouldBe Validated.invalidNec(error)
        }
    }
}
