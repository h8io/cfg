package h8io.cfg

import cats.Eq
import cats.laws.discipline.FunctorTests
import cats.syntax.all.*
import h8io.cfg.raw.Node
import h8io.cfg.testutil.MockNodeError
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.{ZoneId, ZonedDateTime}

class PropertyFunctorLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  private implicit def arbProperty[PT: Arbitrary]: Arbitrary[Property[PT]] =
    Arbitrary {
      Arbitrary.arbitrary[String].flatMap { nm =>
        Gen.oneOf(
          Arbitrary.arbitrary[PT].map[Property[PT]](value =>
            new Property[PT] {
              def name: String = nm
              def apply(cfg: Node.Map): CfgValue[PT] = value.valid
            }),
          Gen.const[Property[PT]](
            new Property[PT] {
              def name: String = nm
              def apply(cfg: Node.Map): CfgValue[PT] = MockNodeError(cfg).invalid
            })
        )
      }
    }

  private implicit def propertyEq[PT]: Eq[Property[PT]] = {
    val cfg = mock[Node.Map]
    Eq.instance[Property[PT]]((a, b) => a.name == b.name && a.apply(cfg) == b.apply(cfg))
  }

  checkAll("Property functor", FunctorTests[Property].functor[ZoneId, ZonedDateTime, Long])
}
