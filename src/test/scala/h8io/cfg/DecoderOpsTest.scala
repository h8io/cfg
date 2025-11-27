package h8io.cfg

import cats.data.{NonEmptyChain, Validated}
import cats.syntax.all.*
import h8io.cfg.raw.Node
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DecoderOpsTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  ">=>" should "pass a successful decoder result into the function" in {
    val decoder = mock[Decoder[Long]]
    val f = mockFunction[Long, Decoder.Result[String]]
    val node = mock[Node.Map]
    val composition = decoder >=> f
    inSequence {
      (decoder.apply _).expects(node).returning(42L.valid)
      f.expects(42L).returning("answer".valid)
      composition(node) shouldBe "answer".valid
    }
  }

  it should "return the decoder error without calling the next function when the decoder fails" in {
    val decoder = mock[Decoder[Long]]
    val f = mockFunction[Long, Decoder.Result[String]]
    val node = mock[Node.Seq]
    val composition = decoder >=> f
    val decoderError = mock[CfgError].invalidNec
    (decoder.apply _).expects(node).returning(decoderError)
    composition(node) shouldBe decoderError
  }

  it should "return the function's error when the decoder succeeds but the function fails" in {
    val decoder = mock[Decoder[Long]]
    val f = mockFunction[Long, Decoder.Result[String]]
    val node = mock[Node.Map]
    val composition = decoder >=> f
    val fError = mock[CfgError].invalidNec
    inSequence {
      (decoder.apply _).expects(node).returning(42L.valid)
      f.expects(42L).returning(fError)
      composition(node) shouldBe fError
    }
  }

  "||" should "return the left decoder result when the left decoder succeeds" in {
    val left = mock[Decoder[String]]("left decoder")
    val right = mock[Decoder[String]]("right decoder")
    val decoder = left || right
    val node = mock[Node.Map]
    (left.apply _).expects(node).returning("left result".valid)
    decoder(node) shouldBe "left result".valid
  }

  it should "accumulate errors from both decoders when both fail" in {
    val left = mock[Decoder[String]]("left decoder")
    val right = mock[Decoder[String]]("right decoder")
    val decoder = left || right
    val node = mock[Node.Map]
    (left.apply _).expects(node).returning(mock[CfgError].invalidNec)
    (right.apply _).expects(node).returning("right result".valid)
    decoder(node) shouldBe "right result".valid
  }

  it should "return the combined error if both decoders' results are invalid" in {
    val left = mock[Decoder[String]]("left decoder")
    val right = mock[Decoder[String]]("right decoder")
    val decoder = left || right
    val node = mock[Node.Map]
    val leftError = NonEmptyChain(mock[CfgError])
    val rightError = NonEmptyChain(mock[CfgError])
    (left.apply _).expects(node).returning(leftError.invalid)
    (right.apply _).expects(node).returning(rightError.invalid)
    decoder(node) shouldBe Validated.Invalid(leftError ++ rightError)
  }
}
