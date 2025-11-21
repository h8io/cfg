package h8io.cfg

import cats.Eq
import cats.data.Validated
import cats.laws.discipline.FunctorTests
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.{MockCfgError, MockLocation}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.Instant

class DecoderFunctorLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  private implicit def arbDecoder[T: Arbitrary]: Arbitrary[Decoder[Node.Value, T]] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[T].map(value => (_: Node.Value) => Validated.valid(value)),
        Gen.const((key: Node.Value) => Validated.invalidNec(MockCfgError(key)))
      )
    }

  private implicit def decoderEq[T]: Eq[Decoder[Node.Value, T]] = {
    def eq(id: Id, a: Decoder[Node.Value, T], b: Decoder[Node.Value, T]): Boolean = {
      val scalarNode = Node.Scalar(id, "scalar value", MockLocation(s"location for $id"))
      val mapNode = mock[Node.Map]
      val seqNode = mock[Node.Seq]
      a.apply(scalarNode) == b.apply(scalarNode) &&
      a.apply(mapNode) == b.apply(mapNode) &&
      a.apply(seqNode) == b.apply(seqNode)
    }
    Eq.instance[Decoder[Node.Value, T]] { (a, b) =>
      eq(Id.Root, a, b) && eq(Id.Key("input-key", Id.Root), a, b) && eq(Id.Index(42, Id.Root), a, b)
    }
  }

  checkAll("DecoderFunctor", FunctorTests[λ[T => Decoder[Node.Value, T]]].functor[String, Instant, Long])
}
