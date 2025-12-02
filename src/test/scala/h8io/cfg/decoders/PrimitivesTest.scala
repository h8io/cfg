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

import scala.annotation.tailrec

class PrimitivesTest extends AnyFlatSpec with Matchers with MockFactory {
  "stringDecoder" should "return a string value from scalar" in {
    stringDecoder(Node.Scalar(Id.Root, "Cthulhu", mock[Location])) shouldBe "Cthulhu".valid
    stringDecoder(Node.Scalar(Id.Root, "", mock[Location])) shouldBe "".valid
  }

  private def caseIterator(s: String): Iterator[String] =
    (0 until (1 << s.length)).iterator.map { i =>
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

  "byteDecoder" should "return a byte value from scalar" in {
    byteDecoder(Node.Scalar(Id.Root, "0", mock[Location])) shouldBe 0.valid
    byteDecoder(Node.Scalar(Id.Root, Byte.MaxValue.toString, mock[Location])) shouldBe Byte.MaxValue.valid
    byteDecoder(Node.Scalar(Id.Root, Byte.MinValue.toString, mock[Location])) shouldBe Byte.MinValue.valid
  }

  it should "return an error for out of bounds values" in {
    val positiveNode = Node.Scalar(Id.Root, (Byte.MaxValue.toLong + 1).toString, mock[Location])
    Decoder[Byte](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode = Node.Scalar(Id.Root, (Byte.MinValue.toLong - 1).toString, mock[Location])
    Decoder[Byte](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  "shortDecoder" should "return a short value from scalar" in {
    shortDecoder(Node.Scalar(Id.Root, "0", mock[Location])) shouldBe 0.valid
    shortDecoder(Node.Scalar(Id.Root, Short.MaxValue.toString, mock[Location])) shouldBe Short.MaxValue.valid
    shortDecoder(Node.Scalar(Id.Root, Short.MinValue.toString, mock[Location])) shouldBe Short.MinValue.valid
  }

  it should "return an error for out of bounds values" in {
    val positiveNode = Node.Scalar(Id.Root, (Short.MaxValue.toLong + 1).toString, mock[Location])
    Decoder[Short](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode = Node.Scalar(Id.Root, (Short.MinValue.toLong - 1).toString, mock[Location])
    Decoder[Short](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  "intDecoder" should "return an integer value from scalar" in {
    intDecoder(Node.Scalar(Id.Root, "0", mock[Location])) shouldBe 0.valid
    intDecoder(Node.Scalar(Id.Root, Int.MaxValue.toString, mock[Location])) shouldBe Int.MaxValue.valid
    intDecoder(Node.Scalar(Id.Root, Int.MinValue.toString, mock[Location])) shouldBe Int.MinValue.valid
  }

  it should "return an error for out of bounds values" in {
    val positiveNode = Node.Scalar(Id.Root, (Int.MaxValue.toLong + 1).toString, mock[Location])
    Decoder[Int](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode = Node.Scalar(Id.Root, (Int.MinValue.toLong - 1).toString, mock[Location])
    Decoder[Int](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }

  "longDecoder" should "return a long value from scalar" in {
    longDecoder(Node.Scalar(Id.Root, "0", mock[Location])) shouldBe 0L.valid
    longDecoder(Node.Scalar(Id.Root, Long.MaxValue.toString, mock[Location])) shouldBe Long.MaxValue.valid
    longDecoder(Node.Scalar(Id.Root, Long.MinValue.toString, mock[Location])) shouldBe Long.MinValue.valid
  }

  val BigIntOne = BigInt(1)

  it should "return an error for out of bounds values" in {
    val positiveNode = Node.Scalar(Id.Root, (BigInt(Long.MaxValue) + BigIntOne).toString, mock[Location])
    Decoder[Long](positiveNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`positiveNode`, _: NumberFormatException)) =>
    }
    val negativeNode = Node.Scalar(Id.Root, (BigInt(Long.MinValue) - BigIntOne).toString, mock[Location])
    Decoder[Long](negativeNode) should matchPattern {
      case Validated.Invalid(Decoder.Thrown(`negativeNode`, _: NumberFormatException)) =>
    }
  }
}
