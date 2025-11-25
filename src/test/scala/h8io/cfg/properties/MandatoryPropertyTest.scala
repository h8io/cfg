package h8io.cfg.properties

import cats.syntax.all.*
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.MockLocation
import h8io.cfg.{CfgError, Decoder}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MandatoryPropertyTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "mandatory" should "create a property from a decoder and return the decoder result" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, result, description) =>
        val root = mock[Node.Map]("node")
        implicit val decoder: Decoder[String] = mock[Decoder[String]]("decoder")
        val property = MandatoryProperty(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(result.valid)
          property(root) shouldBe result.valid
        }
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          val error = mock[CfgError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(error.invalidNec)
          property(root) shouldBe error.invalidNec
        }
        inSequence {
          val node = Node.Null(Id.Key(name, Id.Root), MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe node.invalidNec
        }
        inSequence {
          val node = Node.None(Id.Key(name, Id.Root), root)
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe node.invalidNec
        }
    }

  it should "create a property from a decoder and return the Thrown error with decoder exception" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, description) =>
        val root = mock[Node.Map]("node")
        implicit val decoder: Decoder[String] = mock[Decoder[String]]("decoder")
        val property = MandatoryProperty(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          val exception = new RuntimeException("decoder exception")
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).throws(exception)
          property(root) shouldBe Decoder.Thrown(node, exception).invalidNec
        }
    }
}
