package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.raw.{Id, Location, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "stringDecoder" should "return a string value from scalar" in
    forAll { (value: String) =>
      stringDecoder(Node.Scalar(Id.Root, value, mock[Location])) shouldBe value.valid
    }
}
