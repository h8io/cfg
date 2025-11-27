package h8io.cfg

import cats.laws.discipline.SemigroupKTests
import h8io.cfg.Decoder.semigroupK
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.util.UUID

class DecoderSemigroupKLawsTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with DecoderLawsTest {
  checkAll("Decoder semigroupK", SemigroupKTests[Decoder].semigroupK[UUID])
}
