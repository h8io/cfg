package h8io.cfg.fluent.decoders

import cats.syntax.all.*
import h8io.cfg.fluent.Decoder
import h8io.cfg.fluent.Decoder.*

trait numbers {
  implicit val bigIntDecoder: Decoder[BigInt] = stringDecoder.map(BigInt(_))

  implicit val bigDecimalDecoder: Decoder[BigDecimal] = stringDecoder.map(BigDecimal(_))
}
