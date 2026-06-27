package h8io.cfg.fluent.decoders

import cats.syntax.all.*
import h8io.cfg.{Id, Location, Node}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "stringDecoder" should "return a string value from scalar" in
    forAll { (value: String) =>
      stringDecoder(Node.Scalar(Id.Root, None, value, mock[Location])) shouldBe value.valid
    }
}
