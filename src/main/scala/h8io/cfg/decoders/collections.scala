package h8io.cfg.decoders

import cats.syntax.all.*
import h8io.cfg.raw.Node
import h8io.cfg.{BaseDecoder, Decoder}
import h8io.reflect.typeOf

trait collections {
  def VectorDecoder[T: Decoder]: Decoder[Vector[T]] =
    new BaseDecoder[Vector[T]] {
      override def parse(seq: Node.Seq): Decoder.Result[Vector[T]] =
        seq.iterator.map {
          case item: Node.Value => Decoder[T](item)
          case item: Node.Null => item.invalidNec
        }.toVector.sequence
    }
}
