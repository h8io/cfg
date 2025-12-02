package h8io.cfg.decoders

import cats.implicits.catsSyntaxValidatedId
import h8io.cfg.errors
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.{Id, Location, Node}
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
}
