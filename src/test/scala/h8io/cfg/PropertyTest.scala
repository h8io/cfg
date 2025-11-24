package h8io.cfg

import cats.syntax.all.*
import h8io.cfg.raw.Node
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class PropertyTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "decode" should "return a valid result returned by decoder" in {
    implicit val decoder: Decoder[String] = mock[Decoder[String]]
    val node = mock[Node.Map]
    (decoder.apply _).expects(node).returning("test result".valid)
    Property.decode[String](node) shouldBe "test result".valid
  }

  it should "return an invalid result returned by decoder" in {
    implicit val decoder: Decoder[String] = mock[Decoder[String]]
    val node = mock[Node.Seq]
    val error = mock[Decoder.Error].invalidNec
    (decoder.apply _).expects(node).returning(error)
    Property.decode[String](node) shouldBe error
  }

  it should "return a Thrown result if decoder throws an exception" in {
    implicit val decoder: Decoder[String] = mock[Decoder[String]]
    val node = mock[Node.Map]
    val exception = new RuntimeException("decoder exception")
    (decoder.apply _).expects(node).throws(exception)
    Property.decode[String](node) shouldBe Decoder.Thrown(node, exception).invalidNec
  }

  ">=>" should "pass a successful property into the function" in {
    val property = mock[Property[Long]]
    val f = mockFunction[Long, Property.Value[String]]
    val node = mock[Node.Map]
    val composition = property >=> f
    inSequence {
      (property.apply _).expects(node).returning(42L.valid)
      f.expects(42L).returning("answer".valid)
      composition(node) shouldBe "answer".valid
    }
  }

  it should "return the property error without calling the next function when the property fails" in {
    val property = mock[Property[Long]]
    val f = mockFunction[Long, Property.Value[String]]
    val node = mock[Node.Map]
    val composition = property >=> f
    val propertyError = mock[Property.Error].invalidNec
    (property.apply _).expects(node).returning(propertyError)
    composition(node) shouldBe propertyError
  }

  it should "return the function's error when the property succeeds but the function fails" in {
    val property = mock[Property[Long]]
    val f = mockFunction[Long, Property.Value[String]]
    val node = mock[Node.Map]
    val composition = property >=> f
    val fError = mock[Decoder.Error].invalidNec
    inSequence {
      (property.apply _).expects(node).returning(42L.valid)
      f.expects(42L).returning(fError)
      composition(node) shouldBe fError
    }
  }

  it should "return a Thrown error if the function throws an exception" in {
    val property = mock[Property[Long]]
    val f = mockFunction[Long, Property.Value[String]]
    val node = mock[Node.Map]
    val composition = property >=> f
    val exception = new RuntimeException("function exception")
    inSequence {
      (property.apply _).expects(node).returning(42L.valid)
      f.expects(42L).throws(exception)
      composition(node) shouldBe Property.Thrown(node, composition, exception).invalidNec
    }
  }

  it should "keep the property name" in {
    val property = mock[Property[Long]]
    val f = mockFunction[Long, Property.Value[String]]
    (() => property.name).expects().returning("question")
    (property >=> f).name shouldBe "question"
  }
}
