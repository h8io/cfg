package h8io.cfg

import cats.Eq
import cats.laws.discipline.MonadTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import h8io.cfg.Decoder.monad
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.time.Instant

class DecoderMonadLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with DecoderLawsTest {
  private implicit def instantEq: Eq[Instant] = Eq.fromUniversalEquals

  implicit val decoderIsomorphisms: Isomorphisms[Decoder] = Isomorphisms.invariant[Decoder]

  checkAll("Decoder monad", MonadTests[Decoder].monad[String, Instant, Long])
}
