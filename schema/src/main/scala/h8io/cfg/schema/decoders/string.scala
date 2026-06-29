package h8io.cfg.schema.decoders

import cats.syntax.all.*
import h8io.cfg.Node.Scalar
import h8io.cfg.schema.{CfgValue, Decoder, SelectiveDecoder}

trait string {
  implicit val stringDecoder: Decoder[String] = new SelectiveDecoder[String] {
    override protected def parse(scalar: Scalar): CfgValue[String] = scalar.value.valid
  }
}
