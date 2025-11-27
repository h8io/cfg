package h8io.cfg

import cats.data.Validated
import cats.syntax.all.*
import h8io.cfg.Decoder.Result
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.{Id, Location, Node}
import h8io.reflect.typeOf
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BaseDecoderTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "BaseDecoder" should "parse scalar if scalar parse method is overridden" in {
    val decoder = new BaseDecoder[Node.Scalar] {
      override protected def parse(scalar: Node.Scalar): Result[Node.Scalar] = scalar.valid
    }
    val scalar = Node.Scalar(Id.Root, "test scalar", mock[Location])
    inside(decoder(scalar)) { case Validated.Valid(result) => result should be theSameInstanceAs scalar }
  }

  it should "not parse scalar if scalar parse method is not overridden" in {
    val decoder = new BaseDecoder[Any] {}
    val scalar = Node.Scalar(Id.Root, "test scalar", mock[Location])
    decoder(scalar) shouldBe UnexpectedNode[Any](scalar).invalidNec
  }

  it should "parse map if map parse method is overridden" in {
    val decoder = new BaseDecoder[Node.Map] {
      override protected def parse(map: Node.Map): Result[Node.Map] = map.valid
    }
    val map = mock[Node.Map]
    inside(decoder(map)) { case Validated.Valid(result) => result should be theSameInstanceAs map }
  }

  it should "not parse map if map parse method is not overridden" in {
    val decoder = new BaseDecoder[Any] {}
    val map = mock[Node.Map]
    decoder(map) shouldBe UnexpectedNode[Any](map).invalidNec
  }

  it should "parse seq if seq parse method is overridden" in {
    val decoder = new BaseDecoder[Node.Seq] {
      override protected def parse(seq: Node.Seq): Result[Node.Seq] = seq.valid
    }
    val map = mock[Node.Seq]
    inside(decoder(map)) { case Validated.Valid(result) => result should be theSameInstanceAs map }
  }

  it should "not parse seq if seq parse method is not overridden" in {
    val decoder = new BaseDecoder[Any] {}
    val map = mock[Node.Seq]
    decoder(map) shouldBe UnexpectedNode[Any](map).invalidNec
  }
}
