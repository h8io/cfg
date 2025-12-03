package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.Decoder
import h8io.cfg.Decoder.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, TimeUnit}

trait time {
  implicit val durationDecoder: Decoder[Duration] = stringDecoder.map(Duration(_))

  implicit val javaDurationDecoder: Decoder[java.time.Duration] = stringDecoder.map(java.time.Duration.parse)

  implicit val instantDecoder: Decoder[java.time.Instant] = stringDecoder.map(java.time.Instant.parse)
  implicit val localDateDecoder: Decoder[java.time.LocalDate] = stringDecoder.map(java.time.LocalDate.parse)
  implicit val localDateTimeDecoder: Decoder[java.time.LocalDateTime] = stringDecoder.map(java.time.LocalDateTime.parse)
  implicit val localTimeDecoder: Decoder[java.time.LocalTime] = stringDecoder.map(java.time.LocalTime.parse)
  implicit val monthDayDecoder: Decoder[java.time.MonthDay] = stringDecoder.map(java.time.MonthDay.parse)
  implicit val offsetDateTimeDecoder: Decoder[java.time.OffsetDateTime] =
    stringDecoder.map(java.time.OffsetDateTime.parse)
  implicit val offsetTimeDecoder: Decoder[java.time.OffsetTime] = stringDecoder.map(java.time.OffsetTime.parse)
  implicit val periodDecoder: Decoder[java.time.Period] = stringDecoder.map(java.time.Period.parse)
  implicit val yearDecoder: Decoder[java.time.Year] = stringDecoder.map(java.time.Year.parse)
  implicit val yearMonthDecoder: Decoder[java.time.YearMonth] = stringDecoder.map(java.time.YearMonth.parse)
  implicit val zonedDateTimeDecoder: Decoder[java.time.ZonedDateTime] = stringDecoder.map(java.time.ZonedDateTime.parse)

  implicit val dateTimeFormatter: Decoder[java.time.format.DateTimeFormatter] =
    stringDecoder.map(java.time.format.DateTimeFormatter.ofPattern)
}
