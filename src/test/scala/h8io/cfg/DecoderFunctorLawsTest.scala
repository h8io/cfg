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
  private implicit def arbDecoder[T: Arbitrary]: Arbitrary[Decoder[Node.Null, T]] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[T].map(value => (_: Node.Null) => Validated.valid(value)),
        Gen.const((key: Node.Null) => Validated.invalidNec(MockCfgError(key)))
      )
    }

  private implicit def nullDecoderEq[T]: Eq[Decoder[Node.Null, T]] =
    Eq.instance[Decoder[Node.Null, T]] { (a, b) =>
      val rootNode = Node.Null(Id.Root, MockLocation("root location"))
      val keyNode = Node.Null(Id.Key("input-key", Id.Root), MockLocation("key location"))
      val indexNode = Node.Null(Id.Index(42, Id.Root), MockLocation("index location"))
      a.apply(rootNode) == b.apply(rootNode) &&
      a.apply(keyNode) == b.apply(keyNode) &&
      a.apply(indexNode) == b.apply(indexNode)
    }

  checkAll("DecoderFunctor", FunctorTests[λ[T => Decoder[Node.Null, T]]].functor[String, Instant, Long])
}
