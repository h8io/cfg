package h8io.cfg.schema.decoders

import cats.syntax.all.*
import h8io.cfg.schema.Decoder
import h8io.cfg.schema.Decoder.*

trait numbers {
  implicit val bigIntDecoder: Decoder[BigInt] = stringDecoder.map(BigInt(_))

  implicit val bigDecimalDecoder: Decoder[BigDecimal] = stringDecoder.map(BigDecimal(_))
}
