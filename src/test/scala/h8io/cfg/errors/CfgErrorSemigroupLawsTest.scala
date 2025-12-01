package h8io.cfg.errors

import cats.data.NonEmptyChain
import cats.kernel.Eq
import cats.kernel.laws.discipline.SemigroupTests
import h8io.cfg.CfgError
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class CfgErrorSemigroupLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with MockFactory {
  private val genPlainError: Gen[CfgError] = Gen.delay(mock[CfgError])

  private val genCfgErrors: Gen[NonEmptyChain[CfgError]] =
    for {
      head <- genPlainError
      tail <- Gen.listOf(genPlainError)
    } yield NonEmptyChain(head, tail*)

  private val genAndError: Gen[AndError] = genCfgErrors.map(AndError(_))

  private val genOrError: Gen[AndError] = genCfgErrors.map(AndError(_))

  protected implicit def arbError: Arbitrary[CfgError] = Arbitrary(Gen.oneOf(genPlainError, genAndError, genOrError))

  protected implicit def eqError: Eq[CfgError] = Eq.fromUniversalEquals

  checkAll("Cfg error `AND` semigroup", SemigroupTests[CfgError].semigroup)

  checkAll("Cfg error `OR` semigroup", SemigroupTests[CfgError](_ | _).semigroup)
}
