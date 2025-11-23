package h8io.cfg

import cats.Eq
import cats.data.Validated
import cats.laws.discipline.MonadTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import h8io.cfg.Decoder.Monad
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.{MockDecoderError, MockLocation}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.Instant

class DecoderMonadLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  private implicit def arbDecoder[T: Arbitrary]: Arbitrary[Decoder[T]] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[T].map[Decoder[T]](value => (_: Node.Value) => Validated.valid(value)),
        Gen.const[Decoder[T]]((key: Node.Value) => Validated.invalidNec(MockDecoderError(key)))
      )
    }

  private implicit def decoderEq[T]: Eq[Decoder[T]] = {
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

  private implicit def instantEq: Eq[Instant] = Eq.fromUniversalEquals

  implicit val decoderIsomorphisms: Isomorphisms[Decoder] = Isomorphisms.invariant[Decoder]

  checkAll("DecoderMonad", MonadTests[Decoder].monad[String, Instant, Long])
}
