package h8io.cfg.decoders

import cats.data.Validated
import cats.syntax.all.*
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.{Id, Location, Node}
import h8io.cfg.{errors, Decoder}
import h8io.reflect.typeOf
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.annotation.tailrec

class PrimitivesTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  private def caseIterator(s: String): Iterator[String] = (0 until (1 << s.length)).iterator.map { i =>
    val chars = s.toCharArray
    @tailrec def loop(j: Int, i: Int): Unit =
      if (j < chars.length && i > 0) {
        if (i % 2 == 1) chars(j) = chars(j).toUpper
        loop(j + 1, i >> 1)
      }
    loop(0, i)
    new String(chars)
  }

  "caseIterator" should "return all possible cases for a lower case string" in {
    caseIterator("abc").toSet shouldBe Set("abc", "Abc", "aBc", "ABc", "abC", "AbC", "aBC", "ABC")
  }

  "booleanDecoder" should "return a true value from scalar" in
    (caseIterator("true") ++ caseIterator("on") ++ caseIterator("yes")).foreach { value =>
      booleanDecoder(Node.Scalar(Id.Root, value, mock[Location])) shouldBe true.valid
    }

  it should "return a false value from scalar" in
    (caseIterator("false") ++ caseIterator("off") ++ caseIterator("no")).foreach { value =>
      booleanDecoder(Node.Scalar(Id.Root, value, mock[Location])) shouldBe false.valid
    }

  it should "return an error from non-boolean scalar" in {
    val nonEmptyNode = Node.Scalar(Id.Root, "non-boolean", mock[Location])
    booleanDecoder(nonEmptyNode) shouldBe UnexpectedNode[Boolean](nonEmptyNode).invalid
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    booleanDecoder(emptyNode) shouldBe errors.UnexpectedNode[Boolean](emptyNode).invalid
  }

  "byteDecoder" should "return a byte value from scalar" in
    forAll { (value: Byte) =>
      byteDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  it should "return an error for out of bounds values" in {
    val positiveNode =
      Node.Scalar(Id.Root, (Byte.MaxValue.toLong + 1).toString, mock[Location])
    Decoder[Byte](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode =
      Node.Scalar(Id.Root, (Byte.MinValue.toLong - 1).toString, mock[Location])
    Decoder[Byte](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, "abc", mock[Location])
    Decoder[Byte](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    Decoder[Byte](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
    val floatNode = Node.Scalar(Id.Root, "1.1", mock[Location])
    Decoder[Byte](floatNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`floatNode`, _: NumberFormatException)) =>
    }
  }

  "shortDecoder" should "return a short value from scalar" in
    forAll { (value: Short) =>
      shortDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  it should "return an error for out of bounds values" in {
    val positiveNode =
      Node.Scalar(Id.Root, (Short.MaxValue.toLong + 1).toString, mock[Location])
    Decoder[Short](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode =
      Node.Scalar(Id.Root, (Short.MinValue.toLong - 1).toString, mock[Location])
    Decoder[Short](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, "abc", mock[Location])
    Decoder[Short](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    Decoder[Short](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
    val floatNode = Node.Scalar(Id.Root, "1.1", mock[Location])
    Decoder[Short](floatNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`floatNode`, _: NumberFormatException)) =>
    }
  }

  "intDecoder" should "return an integer value from scalar" in
    forAll { (value: Int) =>
      intDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  it should "return an error for out of bounds values" in {
    val positiveNode =
      Node.Scalar(Id.Root, (Int.MaxValue.toLong + 1).toString, mock[Location])
    Decoder[Int](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode =
      Node.Scalar(Id.Root, (Int.MinValue.toLong - 1).toString, mock[Location])
    Decoder[Int](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, "abc", mock[Location])
    Decoder[Int](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    Decoder[Int](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
    val floatNode = Node.Scalar(Id.Root, "1.1", mock[Location])
    Decoder[Int](floatNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`floatNode`, _: NumberFormatException)) =>
    }
  }

  "longDecoder" should "return a long value from scalar" in
    forAll { (value: Long) =>
      longDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  private val BigIntOne = BigInt(1)

  it should "return an error for out of bounds values" in {
    val positiveNode =
      Node.Scalar(Id.Root, (BigInt(Long.MaxValue) + BigIntOne).toString, mock[Location])
    Decoder[Long](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode =
      Node.Scalar(Id.Root, (BigInt(Long.MinValue) - BigIntOne).toString, mock[Location])
    Decoder[Long](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, "abc", mock[Location])
    Decoder[Long](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    Decoder[Long](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
    val floatNode = Node.Scalar(Id.Root, "1.1", mock[Location])
    Decoder[Long](floatNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`floatNode`, _: NumberFormatException)) =>
    }
  }

  "floatDecoder" should "return a floating point value from scalar" in {
    forAll { (value: Float) =>
      floatDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }
    floatDecoder(Node.Scalar(Id.Root, Double.MaxValue.toString, mock[Location])) shouldBe
      Float.PositiveInfinity.valid
    floatDecoder(Node.Scalar(Id.Root, Double.MinValue.toString, mock[Location])) shouldBe
      Float.NegativeInfinity.valid
  }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, "abc", mock[Location])
    Decoder[Float](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    Decoder[Float](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
  }

  private val BigDecimalTwo = BigDecimal(2)

  "doubleDecoder" should "return a double precision floating point value from scalar" in {
    forAll { (value: Double) =>
      doubleDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }
    doubleDecoder(Node.Scalar(Id.Root, (BigDecimal(Double.MaxValue) * BigDecimalTwo).toString, mock[Location])) shouldBe
      Double.PositiveInfinity.valid
    doubleDecoder(Node.Scalar(Id.Root, (BigDecimal(Double.MinValue) * BigDecimalTwo).toString, mock[Location])) shouldBe
      Double.NegativeInfinity.valid
  }

  it should "return an error for invalid scalar value" in {
    val alphaNode = Node.Scalar(Id.Root, "abc", mock[Location])
    Decoder[Double](alphaNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`alphaNode`, _: NumberFormatException)) =>
    }
    val emptyNode = Node.Scalar(Id.Root, "", mock[Location])
    Decoder[Double](emptyNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`emptyNode`, _: NumberFormatException)) =>
    }
  }
}
