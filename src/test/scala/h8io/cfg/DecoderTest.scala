package h8io.cfg

import cats.syntax.all.*
import h8io.cfg.raw.Node
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecoderTest extends AnyFlatSpec with Matchers with MockFactory {
  "decode" should "return a valid result returned by decoder" in {
    implicit val decoder: Decoder[String] = mock[Decoder[String]]
    val node = mock[Node.Map]
    (decoder.apply _).expects(node).returning("test result".valid)
    Decoder(node) shouldBe "test result".valid
  }

  it should "return an invalid result returned by decoder" in {
    implicit val decoder: Decoder[String] = mock[Decoder[String]]
    val node = mock[Node.Seq]
    val error = mock[CfgError].invalidNec
    (decoder.apply _).expects(node).returning(error)
    Decoder(node) shouldBe error
  }

  it should "return a Thrown result if decoder throws an exception" in {
    implicit val decoder: Decoder[String] = mock[Decoder[String]]
    val node = mock[Node.Map]
    val exception = new RuntimeException("decoder exception")
    (decoder.apply _).expects(node).throws(exception)
    Decoder(node) shouldBe Decoder.Thrown(node, exception).invalidNec
  }
}
