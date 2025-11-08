package h8io.cfg

import cats.Eq
import cats.data.Validated
import cats.laws.discipline.FunctorTests
import h8io.cfg.raw.{Id, Node, Origin}
import h8io.cfg.testutil.{MockDecoderError, MockOrigin}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.Instant

class DecoderFunctorTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  private val genOrigin: Gen[Origin] = Arbitrary.arbitrary[String].map(MockOrigin(_))

  private implicit def arbDecoder[T: Arbitrary]: Arbitrary[Decoder[Node.Null[Id], T]] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[T].map(value => (_: Node.Null[Id]) => Validated.valid(value)),
        genOrigin.map(o => (key: Node.Null[Id]) => Validated.invalidNec(MockDecoderError(key.id, o)))
      )
    }

  private implicit def nullDecoderEq[T]: Eq[Decoder[Node.Null[Id], T]] =
    Eq.instance[Decoder[Node.Null[Id], T]] { (a, b) =>
      val rootNode = Node.Null(Id.Root, MockOrigin("input origin"))
      val keyNode = Node.Null(Id.Key("input-key", Id.Root), MockOrigin("input origin"))
      val indexNode = Node.Null(Id.Index(42, Id.Root), MockOrigin("input origin"))
      a.apply(rootNode) == b.apply(rootNode) &&
      a.apply(keyNode) == b.apply(keyNode) &&
      a.apply(indexNode) == b.apply(indexNode)
    }

  checkAll("DecoderFunctor", FunctorTests[λ[T => Decoder[Node.Null[Id], T]]].functor[String, Instant, Long])
}
