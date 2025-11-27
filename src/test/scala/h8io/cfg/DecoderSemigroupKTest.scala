package h8io.cfg

import cats.data.{NonEmptyChain, Validated}
import cats.syntax.all.*
import h8io.cfg.Decoder.semigroupK
import h8io.cfg.raw.Node
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecoderSemigroupKTest extends AnyFlatSpec with Matchers with MockFactory {
  "Decoder <+>" should "return the left decoder result when the left decoder succeeds" in {
    val left = mock[Decoder[String]]("left decoder")
    val right = mock[Decoder[String]]("right decoder")
    val decoder = left <+> right
    val node = mock[Node.Map]
    (left.apply _).expects(node).returning("left result".valid)
    decoder(node) shouldBe "left result".valid
  }

  it should "accumulate errors from both decoders when both fail" in {
    val left = mock[Decoder[String]]("left decoder")
    val right = mock[Decoder[String]]("right decoder")
    val decoder = left <+> right
    val node = mock[Node.Map]
    (left.apply _).expects(node).returning(mock[CfgError].invalidNec)
    (right.apply _).expects(node).returning("right result".valid)
    decoder(node) shouldBe "right result".valid
  }

  it should "return the combined error if both decoders' results are invalid" in {
    val left = mock[Decoder[String]]("left decoder")
    val right = mock[Decoder[String]]("right decoder")
    val decoder = left <+> right
    val node = mock[Node.Map]
    val leftError = NonEmptyChain(mock[CfgError])
    val rightError = NonEmptyChain(mock[CfgError])
    (left.apply _).expects(node).returning(leftError.invalid)
    (right.apply _).expects(node).returning(rightError.invalid)
    decoder(node) shouldBe Validated.Invalid(leftError ++ rightError)
  }
}
