package h8io.cfg.fluent.decoders

import cats.syntax.all.*
import h8io.cfg.fluent.{BaseDecoder, CfgValue, Decoder}
import h8io.cfg.Node.Scalar
import h8io.reflect.typeOf

trait string {
  implicit val stringDecoder: Decoder[String] = new BaseDecoder[String] {
    override protected def parse(scalar: Scalar): CfgValue[String] = scalar.value.valid
  }
}
