package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.Decoder
import h8io.cfg.Decoder.*
import h8io.cfg.errors.UnexpectedNode
import h8io.reflect.typeOf

trait primitives {
  implicit val booleanDecoder: Decoder[Boolean] = stringDecoder >=> {
    _.toLowerCase match {
      case "true" | "on" | "yes" => _ => true.valid
      case "false" | "off" | "no" => _ => false.valid
      case _ => node => UnexpectedNode[Boolean](node).invalid
    }
  }

  implicit val byteDecoder: Decoder[Byte] = stringDecoder.map(_.toByte)
  implicit val shortDecoder: Decoder[Short] = stringDecoder.map(_.toShort)
  implicit val intDecoder: Decoder[Int] = stringDecoder.map(_.toInt)
  implicit val longDecoder: Decoder[Long] = stringDecoder.map(_.toLong)

  implicit val floatDecoder: Decoder[Float] = stringDecoder.map(_.toFloat)
  implicit val doubleDecoder: Decoder[Double] = stringDecoder.map(_.toDouble)
}
