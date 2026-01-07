package h8io.cfg

import cats.Eq
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.{MonadTests, SemigroupKTests}
import cats.syntax.all.*
import h8io.cfg.Decoder.{monad, semigroupK}
import h8io.cfg.raw.{Id, Node}
import h8io.cfg.testutil.{MockLocation, MockNodeError}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.Instant
import java.util.UUID

class DecoderLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  protected implicit def arbDecoder[T: Arbitrary]: Arbitrary[Decoder[T]] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[T].map[Decoder[T]](value => (_: Node.Value) => value.valid),
        Gen.const[Decoder[T]]((key: Node.Value) => MockNodeError(key).invalid)
      )
    }

  protected implicit def decoderEq[T]: Eq[Decoder[T]] = {
    def eq(id: Id, a: Decoder[T], b: Decoder[T]): Boolean = {
      val location = MockLocation(s"location for $id")
      val scalarNode = Node.Scalar(id, "scalar value", location)
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

  checkAll("Decoder semigroupK", SemigroupKTests[Decoder].semigroupK[UUID])

  private implicit def instantEq: Eq[Instant] = Eq.fromUniversalEquals

  implicit val decoderIsomorphisms: Isomorphisms[Decoder] = Isomorphisms.invariant[Decoder]

  checkAll("Decoder monad", MonadTests[Decoder].monad[String, Instant, Long])
}
