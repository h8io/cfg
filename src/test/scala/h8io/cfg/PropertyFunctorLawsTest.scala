package h8io.cfg

import cats.Eq
import cats.laws.discipline.FunctorTests
import cats.syntax.all.*
import h8io.cfg.Property.Value
import h8io.cfg.raw.Node
import h8io.cfg.testutil.MockPropertyError
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.{ZoneId, ZonedDateTime}

class PropertyFunctorLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  private implicit def arbProperty[T: Arbitrary]: Arbitrary[Property[T]] =
    Arbitrary {
      Arbitrary.arbitrary[String].flatMap { nm =>
        Gen.oneOf(
          Arbitrary.arbitrary[T].map[Property[T]](value =>
            new Property[T] {
              def name: String = nm
              def apply(cfg: Node.Map): Value[T] = value.valid
            }),
          Gen.const[Property[T]](
            new Property[T] {
              def name: String = nm
              def apply(cfg: Node.Map): Value[T] = MockPropertyError(cfg).invalidNec
            })
        )
      }
    }

  private implicit def propertyEq[T]: Eq[Property[T]] = {
    val cfg = mock[Node.Map]
    Eq.instance[Property[T]]((a, b) => a.name == b.name && a.apply(cfg) == b.apply(cfg))
  }

  checkAll("PropertyFunctor", FunctorTests[Property].functor[ZoneId, ZonedDateTime, Long])
}
