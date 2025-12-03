package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.Decoder
import h8io.cfg.Decoder.*

import scala.concurrent.duration.Duration

trait time {
  val durationDecoder: Decoder[Duration] = stringDecoder.map(Duration(_))

  val javaDurationDecoder: Decoder[java.time.Duration] = stringDecoder.map(java.time.Duration.parse)

  val instantDecoder: Decoder[java.time.Instant] = stringDecoder.map(java.time.Instant.parse)
  val localDateDecoder: Decoder[java.time.LocalDate] = stringDecoder.map(java.time.LocalDate.parse)
  val localDateTimeDecoder: Decoder[java.time.LocalDateTime] = stringDecoder.map(java.time.LocalDateTime.parse)
  val localTimeDecoder: Decoder[java.time.LocalTime] = stringDecoder.map(java.time.LocalTime.parse)
  val monthDayDecoder: Decoder[java.time.MonthDay] = stringDecoder.map(java.time.MonthDay.parse)
  val offsetDateTimeDecoder: Decoder[java.time.OffsetDateTime] = stringDecoder.map(java.time.OffsetDateTime.parse)
  val offsetTimeDecoder: Decoder[java.time.OffsetTime] = stringDecoder.map(java.time.OffsetTime.parse)
  val periodDecoder: Decoder[java.time.Period] = stringDecoder.map(java.time.Period.parse)
  val yearDecoder: Decoder[java.time.Year] = stringDecoder.map(java.time.Year.parse)
  val yearMonthDecoder: Decoder[java.time.YearMonth] = stringDecoder.map(java.time.YearMonth.parse)
  val zonedDateTimeDecoder: Decoder[java.time.ZonedDateTime] = stringDecoder.map(java.time.ZonedDateTime.parse)
}
