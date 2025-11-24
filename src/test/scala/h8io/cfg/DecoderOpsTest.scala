package h8io.cfg

import cats.implicits.{catsSyntaxValidatedId, catsSyntaxValidatedIdBinCompat0}
import h8io.cfg.raw.Node
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DecoderOpsTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  ">=>" should "return a Kleisli composition of a decoder and a function" in
    forAll(Gen.zip(Gen.long, Arbitrary.arbitrary[String])) { case (decoderResult, fResult) =>
      val decoder = mock[Decoder[Long]]
      val f = mock[Long => Decoder.Result[String]]
      val node = mock[Node.Map]
      val composition = decoder >=> f
      inSequence {
        (decoder.apply _).expects(node).returning(decoderResult.valid)
        (f.apply _).expects(decoderResult).returning(fResult.valid)
        composition(node) shouldBe fResult.valid

        val decoderError = mock[Decoder.Error].invalidNec
        (decoder.apply _).expects(node).returning(decoderError)
        composition(node) shouldBe decoderError

        val fError = mock[Decoder.Error].invalidNec
        (decoder.apply _).expects(node).returning(decoderResult.valid)
        (f.apply _).expects(decoderResult).returning(fError)
        composition(node) shouldBe fError
      }
    }
}
