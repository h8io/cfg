package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.Decoder
import h8io.cfg.Decoder.*

trait numbers {
  implicit val bigIntDecoder: Decoder[BigInt] = stringDecoder.map(BigInt(_))

  implicit val bigDecimalDecoder: Decoder[BigDecimal] = stringDecoder.map(BigDecimal(_))
}
