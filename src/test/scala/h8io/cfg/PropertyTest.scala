package h8io.cfg

import cats.data.Validated
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.MockLocation
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PropertyTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "optional" should "create a property from a decoder" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, result, description) =>
        val root = mock[Node.Map]("node")
        implicit val decoder: Decoder[Node.Value, String] = mock[Decoder[Node.Value, String]]("decoder")
        val property = Property.optional(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.Valid(result))
          property(root) shouldBe Validated.Valid(Some(result))
        }
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          val error = mock[CfgError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.invalidNec(error))
          property(root) shouldBe Validated.invalidNec(error)
        }
        inSequence {
          val node = Node.Null(Id.Key(name, Id.Root), MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe Validated.Valid(None)
        }
        inSequence {
          val node = Node.None(Id.Key(name, Id.Root), root)
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe Validated.Valid(None)
        }
    }

  "mandatory" should "create a property from a decoder" in
    forAll(Gen.zip(Gen.alphaStr, Gen.alphaNumStr, Gen.alphaNumStr, Gen.alphaNumStr)) {
      case (name, value, result, description) =>
        val root = mock[Node.Map]("node")
        implicit val decoder: Decoder[Node.Value, String] = mock[Decoder[Node.Value, String]]("decoder")
        val property = Property.mandatory(name)
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.Valid(result))
          property(root) shouldBe Validated.Valid(result)
        }
        inSequence {
          val node = Node.Scalar(Id.Key(name, Id.Root), value, MockLocation(description))
          val error = mock[CfgError]
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          (decoder.apply _).expects(node).returning(Validated.invalidNec(error))
          property(root) shouldBe Validated.invalidNec(error)
        }
        inSequence {
          val node = Node.Null(Id.Key(name, Id.Root), MockLocation(description))
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe Validated.invalidNec(node)
        }
        inSequence {
          val node = Node.None(Id.Key(name, Id.Root), root)
          (() => root.id).expects().returning(Id.Root)
          (root.apply(_: Id.Key)).expects(Id.Key(name, Id.Root)).returning(node)
          property(root) shouldBe Validated.invalidNec(node)
        }
    }
}
