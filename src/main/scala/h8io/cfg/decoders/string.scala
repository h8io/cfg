package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.{BaseDecoder, CfgValue, Decoder}
import h8io.cfg.raw.Node.Scalar
import h8io.reflect.typeOf

trait string {
  implicit val stringDecoder: Decoder[String] = new BaseDecoder[String] {
    override protected def parse(scalar: Scalar): CfgValue[String] = scalar.value.valid
  }
}
