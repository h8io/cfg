package h8io.cfg.errors

import cats.data.NonEmptyChain
import h8io.cfg.CfgError
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CfgErrorOpsTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  private val genCfgError: Gen[CfgError] = Gen.delay(new DummyError)

  private val genCfgErrors: Gen[NonEmptyChain[CfgError]] =
    for {
      head <- genCfgError
      tail <- Gen.listOf(genCfgError)
    } yield NonEmptyChain(head, tail*)

  "|" should "combine two OrError values into a single OrError containing errors from both sides in order" in
    forAll(Gen.zip(genCfgErrors, genCfgErrors)) { case (leftErrors, rightErrors) =>
      OrError(leftErrors) | OrError(rightErrors) shouldBe OrError(leftErrors ++ rightErrors)
    }

  it should "append a CfgError on the right to an existing OrError" in
    forAll(Gen.zip(genCfgErrors, genCfgError)) { case (leftErrors, rightError) =>
      OrError(leftErrors) | rightError shouldBe OrError(leftErrors :+ rightError)
    }

  it should "prepend a CfgError on the left to an existing OrError" in
    forAll(Gen.zip(genCfgError, genCfgErrors)) { case (leftError, rightErrors) =>
      leftError | OrError(rightErrors) shouldBe OrError(leftError +: rightErrors)
    }

  it should "wrap two CfgError values into a new OrError" in
    forAll(Gen.zip(genCfgError, genCfgError)) { case (leftError, rightError) =>
      leftError | rightError shouldBe OrError(NonEmptyChain(leftError, rightError))
    }

  "&" should "combine two AndError values into a single AndError containing errors from both sides in order" in
    forAll(Gen.zip(genCfgErrors, genCfgErrors)) { case (leftErrors, rightErrors) =>
      AndError(leftErrors) & AndError(rightErrors) shouldBe AndError(leftErrors ++ rightErrors)
    }

  it should "append a CfgError on the right to an existing AndError" in
    forAll(Gen.zip(genCfgErrors, genCfgError)) { case (leftErrors, rightError) =>
      AndError(leftErrors) & rightError shouldBe AndError(leftErrors :+ rightError)
    }

  it should "prepend a CfgError on the left to an existing AndError" in
    forAll(Gen.zip(genCfgError, genCfgErrors)) { case (leftError, rightErrors) =>
      leftError & AndError(rightErrors) shouldBe AndError(leftError +: rightErrors)
    }

  it should "wrap two CfgError values into a new AndError" in
    forAll(Gen.zip(genCfgError, genCfgError)) { case (leftError, rightError) =>
      leftError & rightError shouldBe AndError(NonEmptyChain(leftError, rightError))
    }
}
