package h8io.cfg.properties

import cats.syntax.all.*
import h8io.cfg.raw.{Id, Node, Tag}
import h8io.cfg.testutil.MockLocation
import h8io.cfg.{Decoder, NodeError}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OptionalPropertyTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  val tag = Tag.None(MockLocation("tag location"))

  "OptionalProperty" should "create a property from a decoder and return the decoder result" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, result, description) =>
        val root = mock[Node.Map]("node")
        implicit val decoder: Decoder[String] = mock[Decoder[String]]("decoder")
        val property = OptionalProperty(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, tag, MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(result.valid)
          property(root) shouldBe Some(result).valid
        }
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, tag, MockLocation(description))
          val error = mock[NodeError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(error.invalid)
          property(root) shouldBe error.invalid
        }
        inSequence {
          val node = Node.Null(Id.Key(name, Id.Root), MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe None.valid
        }
        inSequence {
          val node = Node.None(Id.Key(name, Id.Root), root)
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe None.valid
        }
    }

  it should "create a property from a decoder and return the Thrown error with decoder exception" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, description) =>
        val root = mock[Node.Map]("node")
        implicit val decoder: Decoder[String] = mock[Decoder[String]]("decoder")
        val property = OptionalProperty(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, tag, MockLocation(description))
          val exception = new RuntimeException("decoder exception")
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).throws(exception)
          property(root) shouldBe Decoder.Thrown(node, exception).invalid
        }
    }
}
