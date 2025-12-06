package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.errors.UnexpectedNode
import h8io.cfg.raw.{Id, Location, Node}
import h8io.reflect.typeOf
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.*
import scala.concurrent.duration.{Duration, FiniteDuration}

class TimeTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "durationDecoder" should "return a finite duration value from scalar" in
    forAll { (value: FiniteDuration) =>
      durationDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  it should "return a duration value from scalar with positive infinite value" in {
    for (value <- List("Inf", "+Inf", "PlusInf"))
      durationDecoder(Node.Scalar(Id.Root, value, mock[Location])) shouldBe Duration.Inf.valid
  }

  it should "return a duration value from scalar with negative infinite value" in {
    for (value <- List("-Inf", "MinusInf"))
      durationDecoder(Node.Scalar(Id.Root, value, mock[Location])) shouldBe Duration.MinusInf.valid
  }

  "finiteDurationDecoder" should "return a finite duration value from scalar" in
    forAll { (value: FiniteDuration) =>
      finiteDurationDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  it should "return an error from scalar with positive infinite value" in {
    for (value <- List("Inf", "+Inf", "PlusInf")) {
      val infNode = Node.Scalar(Id.Root, value, mock[Location])
      finiteDurationDecoder(infNode) shouldBe UnexpectedNode[FiniteDuration](infNode).invalid
    }
  }

  it should "return an error from scalar with negative infinite value" in {
    for (value <- List("-Inf", "MinusInf")) {
      val infNode = Node.Scalar(Id.Root, value, mock[Location])
      finiteDurationDecoder(infNode) shouldBe UnexpectedNode[FiniteDuration](infNode).invalid
    }
  }

  it should "return a duration value from scalar with negative infinite value" in {
    durationDecoder(Node.Scalar(Id.Root, "-Inf", mock[Location])) shouldBe Duration.MinusInf.valid
    durationDecoder(Node.Scalar(Id.Root, "MinusInf", mock[Location])) shouldBe Duration.MinusInf.valid
  }

  "javaDurationDecoder" should "return a Java duration value from scalar" in
    forAll { (value: java.time.Duration) =>
      javaDurationDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "javaPeriodDecoder" should "return a period value from scalar" in
    forAll { (value: java.time.Period) =>
      periodDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "instantDecoder" should "return an instant value from scalar" in
    forAll { (value: Instant) =>
      instantDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "localDateDecoder" should "return a local date value from scalar" in
    forAll { (value: LocalDate) =>
      localDateDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "localDateTimeDecoder" should "return a local datetime value from scalar" in
    forAll { (value: LocalDateTime) =>
      localDateTimeDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "monthDayDecoder" should "return a month and day value from scalar" in
    forAll { (value: MonthDay) =>
      monthDayDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "offsetDateTimeDecoder" should "return a datetime with offset from scalar" in
    forAll { (value: OffsetDateTime) =>
      offsetDateTimeDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "offsetTimeDecoder" should "return a time with offset from scalar" in
    forAll { (value: OffsetTime) =>
      offsetTimeDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "yearDecoder" should "return an year from scalar" in
    forAll { (value: Year) =>
      yearDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }

  "yearMonthDecoder" should "return an year from scalar" in
    forAll(Gen.zip(Gen.choose(-999999999, 9999), Gen.choose(1, 12))) { case (year: Int, month: Int) =>
      yearMonthDecoder(Node.Scalar(Id.Root, f"$year%04d-$month%02d", mock[Location])) shouldBe
        YearMonth.of(year, month).valid
    }

  "zonedDateTimeDecoder" should "return a datetime with zone from scalar" in
    forAll { (value: ZonedDateTime) =>
      zonedDateTimeDecoder(Node.Scalar(Id.Root, value.toString, mock[Location])) shouldBe value.valid
    }
}
