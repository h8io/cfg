package h8io.cfg

import cats.Eq
import cats.syntax.all.*
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.{MockCfgError, MockLocation}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite

trait DecoderLawsTest extends MockFactory {
  self: TestSuite =>

  protected implicit def arbDecoder[T: Arbitrary]: Arbitrary[Decoder[T]] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[T].map[Decoder[T]](value => (_: Node.Value) => value.valid),
        Gen.const[Decoder[T]]((key: Node.Value) => MockCfgError(key).invalidNec)
      )
    }

  protected implicit def decoderEq[T]: Eq[Decoder[T]] = {
    def eq(id: Id, a: Decoder[T], b: Decoder[T]): Boolean = {
      val scalarNode = Node.Scalar(id, "scalar value", MockLocation(s"location for $id"))
      val mapNode = mock[Node.Map]
      val seqNode = mock[Node.Seq]
      a.apply(scalarNode) == b.apply(scalarNode) &&
      a.apply(mapNode) == b.apply(mapNode) &&
      a.apply(seqNode) == b.apply(seqNode)
    }
    Eq.instance[Decoder[T]] { (a, b) =>
      eq(Id.Root, a, b) && eq(Id.Key("input-key", Id.Root), a, b) && eq(Id.Index(42, Id.Root), a, b)
    }
  }
}
