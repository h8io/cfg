package h8io.cfg.impl.defg

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException

class ResultTest extends AnyFlatSpec with Matchers {
  private def unreadable(source: String): DefError = SourceUnreadable(source, new IOException(source))

  private val e1 = unreadable("e1")
  private val e2 = unreadable("e2")
  private val e3 = unreadable("e3")

  "pure" should "hold a value with no errors" in {
    Result.pure(1) shouldBe Result(1, Nil)
  }

  "failed" should "hold a value with one error" in {
    Result.failed(1, e1) shouldBe Result(1, e1 :: Nil)
  }

  "map" should "transform the value and keep the errors" in {
    Result(1, e1 :: Nil).map(_ + 1) shouldBe Result(2, e1 :: Nil)
  }

  "flatMap" should "append the errors of the next step to those found before it" in {
    Result(1, e1 :: Nil).flatMap(n => Result(n.toString, e2 :: e3 :: Nil)) shouldBe Result("1", e1 :: e2 :: e3 :: Nil)
  }

  "zip" should "pair the values and keep the errors of both sides in order" in {
    Result(1, e1 :: Nil).zip(Result("x", e2 :: Nil)) shouldBe Result((1, "x"), e1 :: e2 :: Nil)
  }

  "sequence" should "collect every value and every error in order" in {
    Result.sequence(Seq(Result(1, e1 :: Nil), Result.pure(2), Result(3, e2 :: e3 :: Nil))) shouldBe
      Result(Vector(1, 2, 3), e1 :: e2 :: e3 :: Nil)
  }

  it should "give an empty vector for no results" in {
    Result.sequence(Seq.empty[Result[Int]]) shouldBe Result(Vector.empty[Int], Nil)
  }

  "toEither" should "give the value when there are no errors" in {
    Result.pure(1).toEither shouldBe Right(1)
  }

  it should "give every error, in order, when there are any" in {
    Result(1, e1 :: e2 :: Nil).toEither shouldBe Left(DefErrors(::(e1, e2 :: Nil)))
  }
}
