package h8io.cfg.schema.errors

import cats.data.NonEmptyChain
import cats.kernel.{Eq, Semigroup}
import cats.kernel.laws.SemigroupLaws
import cats.kernel.laws.discipline.SemigroupTests
import h8io.cfg.CfgError
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class CfgErrorLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  private val genPlainError: Gen[CfgError] = Gen.delay(new DummyError)

  private val genCfgErrors: Gen[NonEmptyChain[CfgError]] =
    for {
      head <- genPlainError
      tail <- Gen.listOf(genPlainError)
    } yield NonEmptyChain(head, tail*)

  private val genAndError: Gen[AndError] = genCfgErrors.map(AndError(_))

  private val genOrError: Gen[OrError] = genCfgErrors.map(OrError(_))

  protected implicit def arbError: Arbitrary[CfgError] = Arbitrary(Gen.oneOf(genPlainError, genAndError, genOrError))

  protected implicit def eqError: Eq[CfgError] = Eq.fromUniversalEquals

  checkAll("CfgError AND semigroup", SemigroupTests[CfgError].semigroup)

  // The AND semigroup is the implicit one, so the OR one is handed to the laws directly
  checkAll(
    "CfgError OR semigroup",
    new SemigroupTests[CfgError] {
      def laws: SemigroupLaws[CfgError] =
        new SemigroupLaws[CfgError] {
          override implicit def S: Semigroup[CfgError] = Semigroup.instance(_ | _)
        }
    }.semigroup
  )
}
