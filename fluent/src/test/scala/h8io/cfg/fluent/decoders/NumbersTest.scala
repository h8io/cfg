package h8io.cfg.fluent.decoders

import cats.data.Validated
import cats.syntax.all.*
import h8io.cfg.fluent.Decoder
import h8io.cfg.{Id, Location, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class NumbersTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "bigIntDecoder" should "return a byte value from scalar" in
    forAll((value: BigInt) =>
      bigIntDecoder(Node.Scalar(Id.Root, None, value.toString, mock[Location])) shouldBe value.valid)

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, None, "abc", mock[Location])
    Decoder[BigInt](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, None, "", mock[Location])
    Decoder[BigInt](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
    val floatNode = Node.Scalar(Id.Root, None, "1.1", mock[Location])
    Decoder[BigInt](floatNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`floatNode`, _: NumberFormatException)) =>
    }
  }

  "bigDecimalDecoder" should "return a byte value from scalar" in
    forAll { (value: BigDecimal) =>
      bigDecimalDecoder(Node.Scalar(Id.Root, None, value.toString, mock[Location])) shouldBe value.valid
    }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, None, "abc", mock[Location])
    Decoder[BigDecimal](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, None, "", mock[Location])
    Decoder[BigDecimal](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
  }
}
